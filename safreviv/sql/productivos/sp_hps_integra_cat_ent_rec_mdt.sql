






CREATE PROCEDURE "safreviv".sp_hps_integra_cat_ent_rec_mdt(p_usuario CHAR(20), p_folio DECIMAL(10,0), p_proceso_cod SMALLINT, v_nom_archivo CHAR(40))
--===============================================================
-- Version: 2.0.0
-- Fecha ultima modificacion: 9 de julio de 2015
-- Integra los datos de mandato de las tablas temporales realizadas para 
-- carga de archivos
--===============================================================
RETURNING INTEGER,
          INTEGER,
          CHAR(254);

DEFINE v_contador_servicio   INTEGER;
DEFINE v_tipo_mandato        SMALLINT;
DEFINE v_entidad_federativa  INTEGER;
DEFINE v_municipio           INTEGER;
DEFINE v_nombre_desarrollo   CHAR(120);
DEFINE v_desarrollador       CHAR(120);
DEFINE v_numero_cuenta       DECIMAL(10,0);
DEFINE v_administrador       CHAR(120);
DEFINE v_rfc                 CHAR(13);
DEFINE v_numero_acreedor     DECIMAL(10,0);
DEFINE v_promotor_vecinal    CHAR(120);
DEFINE v_paquete             CHAR(18); 
DEFINE v_id_atr_nivel        DECIMAL(9,0);
DEFINE v_id_gpo_mandato      DECIMAL(9,0);

DEFINE v_municipio_desc      VARCHAR(40);   
DEFINE v_entidad_desc_larga  VARCHAR(40);
DEFINE v_entidad_abrev       VARCHAR(05);

DEFINE vmapa_id_cat_gpo      INTEGER  ;
DEFINE vmapa_id_gpo_etiqueta INTEGER  ;
DEFINE vmapa_etiqueta        CHAR(40) ;

-- regitro table mdt_cat_mandato 
DEFINE vcatmdt_id_cat_mandato          DECIMAL(9,0);
DEFINE vcatmdt_cve_mandato             CHAR(007);
DEFINE vcatmdt_desc_mandato            VARCHAR(40);
DEFINE vcatmdt_desc_larga_mandato      VARCHAR(120);
DEFINE vcatmdt_usuario                 CHAR(20);
DEFINE vcatmdt_tpo_mandato             SMALLINT;
DEFINE vcatmdt_f_creacion              DATE ;
DEFINE vcatmdt_estado                  SMALLINT;

--Validaciones 
DEFINE valida_mdt_paquete			   CHAR(18);
DEFINE mandato_exist				   SMALLINT;
DEFINE paquete_exist				   SMALLINT;
DEFINE paquete_count				   SMALLINT;
DEFINE r_ind						   SMALLINT;
DEFINE r_diag						   CHAR(3);		  
DEFINE r_error_sql  				   INTEGER;
DEFINE r_isam_error 			   	   INTEGER;
DEFINE r_msg_error  				   CHAR(254);
DEFINE ins_entidad_federativa  		   CHAR(40);        
DEFINE ins_municipio                   CHAR(40);
DEFINE ins_mensaje 					   CHAR(254);
--Errores
DEFINE v_error_sql  INTEGER;
DEFINE v_isam_error INTEGER;
DEFINE v_msg_error  CHAR(254);

--retorna datos segun excepcion
ON EXCEPTION SET 	v_error_sql,
                    v_isam_error,
                    v_msg_error   
   RETURN v_error_sql,
		  v_isam_error,
		  v_msg_error;

END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_hps_act_entidad_receptora_mdt.trace';
   --TRACE ON;
	--valores iniciales
   LET v_error_sql  = 0;
   LET v_isam_error = 0;
   LET v_msg_error  = "Consulta realizada correctamente";
   LET mandato_exist = 0;
  
   --Actualiza glo_ctr_archivo para eliminar archivo a integrar de la lista
      UPDATE glo_ctr_archivo
         SET estado = 2,
          folio = p_folio
         WHERE nombre_archivo = v_nom_archivo;

      UPDATE glo_folio
         SET status = 1
         WHERE folio = p_folio;

      UPDATE bat_ctr_operacion
         SET    folio       = p_folio
         WHERE  proceso_cod = p_proceso_cod
         AND    opera_cod   = 2
         AND    nom_archivo = v_nom_archivo;
   
   -- se extraen los registros cargados y validados previamente
   FOREACH SELECT a.contador_servicio ,
                  a.tipo_mandato, 
                  a.entidad_federativa, 
                  a.municipio, 
                  a.nombre_desarrollo, 
                  a.desarrollador,
                  a.numero_cuenta , 
                  a.administrador, 
                  a.rfc, 
                  a.numero_acreedor, 
                  a.promotor_vecinal
           INTO   v_contador_servicio ,
                  v_tipo_mandato, 
                  v_entidad_federativa, 
                  v_municipio, 
                  v_nombre_desarrollo, 
                  v_desarrollador,
                  v_numero_cuenta , 
                  v_administrador, 
                  v_rfc, 
                  v_numero_acreedor, 
                  v_promotor_vecinal
            FROM safre_tmp:hps_tmp_det_acmdt a 
            WHERE a.resultado_opera = "01"
            AND   a.diagnostico = "000"

         ------------------------------------
         -- validar la existencia del mandato respecto al paquete, dependiendo del tipo mandato
	  IF v_tipo_mandato == 2 THEN 
		 FOREACH SELECT paquete 
		            INTO valida_mdt_paquete
			        FROM safre_tmp:hps_tmp_det02_acmdt
		            WHERE contador_servicio = v_contador_servicio
		
			       SELECT COUNT(*) 
				      INTO paquete_count
		              FROM mdt_cat_mandato_paquete
		              WHERE cve_mandato = valida_mdt_paquete;
					  
				   IF paquete_count > 0 THEN 
				      LET mandato_exist = mandato_exist + 1;
				      --Si se encuentra el paquete, devuelve el id_cat_mandato
		              SELECT id_cat_mandato
		                 INTO vcatmdt_id_cat_mandato
		                 FROM mdt_cat_mandato_paquete
			             WHERE cve_mandato = valida_mdt_paquete;
				   END IF; 
		 END FOREACH;
	  ELSE
	     LET valida_mdt_paquete = "01"||LPAD(v_municipio,16,"0");
		 SELECT COUNT(*) INTO mandato_exist
		    FROM mdt_cat_mandato_paquete
		    WHERE cve_mandato = valida_mdt_paquete;
	  END IF;
	  
	  --Si mandato cuota de conservacion no existe por paquete, validar que exista por nombre_desarrollo		
      IF v_tipo_mandato == 2 AND mandato_exist == 0 THEN
	     SELECT COUNT(*)
		    INTO mandato_exist
			FROM mdt_cat_mandato
			WHERE desc_mandato = v_nombre_desarrollo;
		 IF mandato_exist > 0 THEN
		    SELECT id_cat_mandato
			   INTO vcatmdt_id_cat_mandato
			   FROM mdt_cat_mandato
			   WHERE desc_mandato = v_nombre_desarrollo;
		 END IF;
	  END IF; 
	  
	  IF mandato_exist > 0 THEN  
		--Si ya existe se actualizarán todas las etiquetas
		   --Para tipo CUOTA DE CONSERVACION Solamente
		IF v_tipo_mandato == 2 THEN
				
		   --Entidad Federativa
		   SELECT entidad_desc_larga 
		      INTO ins_entidad_federativa 
		      FROM cat_entidad_federativa 
		      WHERE entidad_federativa = v_entidad_federativa;
		   EXECUTE FUNCTION sp_hps_actualiza_ent_rec_mdt(v_nombre_desarrollo,"ENTIDAD FEDERATIVA",ins_entidad_federativa) INTO r_ind,r_diag,r_error_sql,r_isam_error,r_msg_error;
		   IF r_ind == 1 THEN
		      LET ins_mensaje = TRIM(v_nombre_desarrollo)||"|ENTIDAD FEDERATIVA|"||ins_entidad_federativa||"|"||r_diag;
		      INSERT INTO hps_actualiza_ent_mdt VALUES(ins_mensaje);
		   END IF;
		
		   --Municipio
		   SELECT municipio_desc
		      INTO ins_municipio
		      FROM cat_municipio
		      WHERE municipio = v_municipio;
		   EXECUTE FUNCTION sp_hps_actualiza_ent_rec_mdt(v_nombre_desarrollo,"MUNICIPIO",ins_municipio) INTO r_ind,r_diag,r_error_sql,r_isam_error,r_msg_error;   
	       IF r_ind == 1 THEN
		      LET ins_mensaje = TRIM(v_nombre_desarrollo)||"|MUNICIPIO|"||ins_municipio||"|"||r_diag;
		      INSERT INTO hps_actualiza_ent_mdt VALUES(ins_mensaje);
		   END IF;
		
		   --NUMERO DE CUENTA
		   EXECUTE FUNCTION sp_hps_actualiza_ent_rec_mdt(v_nombre_desarrollo,"NUMERO DE CUENTA",v_numero_cuenta) INTO r_ind,r_diag,r_error_sql,r_isam_error,r_msg_error;
		   IF r_ind == 1 THEN
		      LET ins_mensaje = TRIM(v_nombre_desarrollo)||"|NUMERO DE CUENTA|"||v_numero_cuenta||"|"||r_diag;
		      INSERT INTO hps_actualiza_ent_mdt VALUES(ins_mensaje);
		   END IF;
		   
           --ADMINISTRADOR       v_administrador, 
		   EXECUTE FUNCTION sp_hps_actualiza_ent_rec_mdt(v_nombre_desarrollo,"ADMINISTRADOR",v_administrador) INTO r_ind,r_diag,r_error_sql,r_isam_error,r_msg_error;
		   IF r_ind == 1 THEN
		      LET ins_mensaje = TRIM(v_nombre_desarrollo)||"|ADMINISTRADOR|"||v_administrador||"|"||r_diag;
		      INSERT INTO hps_actualiza_ent_mdt VALUES(ins_mensaje);
		   END IF;           
		   
		   --RFC
		   EXECUTE FUNCTION sp_hps_actualiza_ent_rec_mdt(v_nombre_desarrollo,"RFC",v_rfc) INTO r_ind,r_diag,r_error_sql,r_isam_error,r_msg_error;
		   IF r_ind == 1 THEN
		      LET ins_mensaje = TRIM(v_nombre_desarrollo)||"|RFC|"||v_rfc||"|"||r_diag;
		      INSERT INTO hps_actualiza_ent_mdt VALUES(ins_mensaje);
		   END IF;		   
		   
           --NUMERO ACREEDOR v_numero_acreedor
		   EXECUTE FUNCTION sp_hps_actualiza_ent_rec_mdt(v_nombre_desarrollo,"NUMERO ACREEDOR",v_numero_acreedor) INTO r_ind,r_diag,r_error_sql,r_isam_error,r_msg_error;
		   IF r_ind == 1 THEN
		      LET ins_mensaje = TRIM(v_nombre_desarrollo)||"|NUMERO ACREEDOR|"||v_numero_acreedor||"|"||r_diag;
		      INSERT INTO hps_actualiza_ent_mdt VALUES(ins_mensaje);
		   END IF;
		   
           --PROMOTOR VECINAL       v_promotor_vecinal
		   EXECUTE FUNCTION sp_hps_actualiza_ent_rec_mdt(v_nombre_desarrollo,"PROMOTOR VECINAL",v_promotor_vecinal) INTO r_ind,r_diag,r_error_sql,r_isam_error,r_msg_error;
		   IF r_ind == 1 THEN
		      LET ins_mensaje = TRIM(v_nombre_desarrollo)||"|PROMOTOR VECINAL|"||v_promotor_vecinal||"|"||r_diag;
		      INSERT INTO hps_actualiza_ent_mdt VALUES(ins_mensaje);
		   END IF;
		   
		    -- se insertan los paquetes relacionados al mandato
           FOREACH SELECT a.paquete 
		              INTO v_paquete 
					  FROM safre_tmp:hps_tmp_det02_acmdt a
                      WHERE a.contador_servicio = v_contador_servicio
                      ORDER BY a.contador_servicio
                      
					  --Si no existe paquete lo inserta
					  SELECT COUNT(*)
						 INTO paquete_exist
					     FROM mdt_cat_mandato_paquete
						 WHERE cve_mandato = v_paquete
						 AND id_cat_mandato = vcatmdt_id_cat_mandato;
					  IF paquete_exist == 0 THEN 
					     INSERT INTO mdt_cat_mandato_paquete VALUES (v_paquete ,
                                                           vcatmdt_id_cat_mandato );									   
					  END IF;
           END FOREACH;
		   
		END IF;
      ELSE		
         
		 LET vcatmdt_id_cat_mandato = seq_mdt_cat_mandato.NEXTVAL;
          
         -- se extraen descripciones de entidad federativa y municipio. son comunes a predial y cuota de conservacion
         SELECT a.municipio_desc     ,
                b.entidad_desc_larga ,
                b.entidad_abrev
         INTO   v_municipio_desc     ,
                v_entidad_desc_larga ,
                v_entidad_abrev
         FROM   cat_municipio a ,
                cat_entidad_federativa b
         WHERE a.entidad_federativa = b.entidad_federativa
         AND   a.municipio = v_municipio ;

         IF v_tipo_mandato = 1 THEN    ---1  PARA TIPO PREDIAL
         -- se asignan valores para mdt_cat_mandato
         LET vcatmdt_cve_mandato    = "01"||LPAD(vcatmdt_id_cat_mandato,5,"0"); 
         LET vcatmdt_desc_mandato = "PREDIAL "||TRIM(v_municipio_desc)||" ("||TRIM(v_entidad_abrev)||")";
         LET vcatmdt_desc_larga_mandato = "MUNICIPIO DE "||TRIM(v_municipio_desc)||", "||TRIM(v_entidad_desc_larga);
         LET vcatmdt_usuario     = p_usuario      ;
         LET vcatmdt_tpo_mandato = v_tipo_mandato ;
         LET vcatmdt_f_creacion  = TODAY          ;
         LET vcatmdt_estado      = 100            ;

         INSERT INTO mdt_cat_mandato VALUES (vcatmdt_id_cat_mandato,
                                             vcatmdt_cve_mandato        ,
                                             vcatmdt_desc_mandato       ,
                                             vcatmdt_desc_larga_mandato ,
                                             vcatmdt_usuario            ,
                                             vcatmdt_tpo_mandato        ,
                                             vcatmdt_f_creacion         ,
                                             vcatmdt_estado );
         -- si construye e inserta el paquete correspondiente, solo para tipo predial, para cuota de conservacion
         -- viene en el detalle 3
         LET v_paquete = "01"||LPAD(v_municipio,16,"0");

         INSERT INTO mdt_cat_mandato_paquete VALUES (v_paquete ,
                                                     vcatmdt_id_cat_mandato );
         -- se construyen los valores para atributo nivel e instancia mandato
         -- para predial solo entidad federativa y municipio

         LET v_id_gpo_mandato = seq_mdt_gpo_mandato.NEXTVAL;

         SELECT a.id_cat_gpo    ,
                a.id_gpo_etiqueta ,
                b.etiqueta
         INTO   vmapa_id_cat_gpo,
                vmapa_id_gpo_etiqueta ,
                vmapa_etiqueta 
         FROM   hps_cat_mapa_etiqueta a,
                mdt_cat_gpo_etiqueta b
         WHERE  a.id_gpo_etiqueta = b.id_gpo_etiqueta
         AND    a.nombre_campo = "entidad_federativa";


         LET v_id_atr_nivel = seq_mdt_cat_atributo_nivel.NEXTVAL;
         INSERT INTO mdt_cat_atributo_nivel VALUES (v_id_atr_nivel,
                                                    vmapa_id_gpo_etiqueta ,
                                                    vcatmdt_id_cat_mandato ,
                                                    v_id_gpo_mandato  ,
                                                    1 ,  -- orden
                                                    0 ,
                                                    p_usuario );

         INSERT INTO mdt_cat_instancia_mandato VALUES (seq_mdt_cat_instancia_mandato.NEXTVAL,
                                                       v_id_atr_nivel ,
                                                       v_entidad_desc_larga ,
                                                       p_usuario ); 

         SELECT a.id_cat_gpo    ,
                a.id_gpo_etiqueta ,
                b.etiqueta
         INTO   vmapa_id_cat_gpo,
                vmapa_id_gpo_etiqueta ,
                vmapa_etiqueta 
         FROM   hps_cat_mapa_etiqueta a,
                mdt_cat_gpo_etiqueta b
         WHERE  a.id_gpo_etiqueta = b.id_gpo_etiqueta
         AND    a.nombre_campo = "municipio";


         LET v_id_atr_nivel = seq_mdt_cat_atributo_nivel.NEXTVAL;
         INSERT INTO mdt_cat_atributo_nivel VALUES (v_id_atr_nivel,
                                                    vmapa_id_gpo_etiqueta ,
                                                    vcatmdt_id_cat_mandato ,
                                                    v_id_gpo_mandato  ,
                                                    2 , -- orden
                                                    0 ,
                                                    p_usuario );

         INSERT INTO mdt_cat_instancia_mandato VALUES (seq_mdt_cat_instancia_mandato.NEXTVAL,
                                                       v_id_atr_nivel ,
                                                       v_municipio_desc ,
                                                       p_usuario ); 




        ELSE   --2 -- TIPO CUOTA CONSERVACION 

		 LET vcatmdt_cve_mandato    = "02"||LPAD(vcatmdt_id_cat_mandato,5,"0");
         LET vcatmdt_desc_mandato = TRIM(v_nombre_desarrollo)  ;
         LET vcatmdt_desc_larga_mandato = TRIM(v_desarrollador);
         LET vcatmdt_usuario = TRIM(p_usuario);
         LET vcatmdt_tpo_mandato = 2;
         LET vcatmdt_f_creacion = TODAY ;
         LET vcatmdt_estado  = 100;

         INSERT INTO mdt_cat_mandato VALUES (vcatmdt_id_cat_mandato,
                                             vcatmdt_cve_mandato        ,
                                             vcatmdt_desc_mandato       ,
                                             vcatmdt_desc_larga_mandato ,
                                             vcatmdt_usuario            ,
                                             vcatmdt_tpo_mandato        ,
                                             vcatmdt_f_creacion         ,
                                             vcatmdt_estado );

         -- se insertan los paquetes relacionados al mandato

         FOREACH SELECT a.paquete INTO v_paquete FROM safre_tmp:hps_tmp_det02_acmdt a
                 WHERE a.contador_servicio = v_contador_servicio
                 ORDER BY a.contador_servicio
 
               INSERT INTO mdt_cat_mandato_paquete VALUES (v_paquete ,
                                                           vcatmdt_id_cat_mandato );
         END FOREACH;

         LET v_id_gpo_mandato = seq_mdt_gpo_mandato.NEXTVAL;

         SELECT a.id_cat_gpo    ,
                a.id_gpo_etiqueta ,
                b.etiqueta
         INTO   vmapa_id_cat_gpo,
                vmapa_id_gpo_etiqueta ,
                vmapa_etiqueta 
         FROM   hps_cat_mapa_etiqueta a,
                mdt_cat_gpo_etiqueta b
         WHERE  a.id_gpo_etiqueta = b.id_gpo_etiqueta
         AND    a.nombre_campo = "entidad_federativa";


         LET v_id_atr_nivel = seq_mdt_cat_atributo_nivel.NEXTVAL;
         INSERT INTO mdt_cat_atributo_nivel VALUES (v_id_atr_nivel,
                                                    vmapa_id_gpo_etiqueta ,
                                                    vcatmdt_id_cat_mandato ,
                                                    v_id_gpo_mandato  ,
                                                    1 ,
                                                    0 ,
                                                    p_usuario );

         INSERT INTO mdt_cat_instancia_mandato VALUES (seq_mdt_cat_instancia_mandato.NEXTVAL,
                                                       v_id_atr_nivel ,
                                                       v_entidad_desc_larga ,
                                                       p_usuario ); 

         SELECT a.id_cat_gpo    ,
                a.id_gpo_etiqueta ,
                b.etiqueta
         INTO   vmapa_id_cat_gpo,
                vmapa_id_gpo_etiqueta ,
                vmapa_etiqueta 
         FROM   hps_cat_mapa_etiqueta a,
                mdt_cat_gpo_etiqueta b
         WHERE  a.id_gpo_etiqueta = b.id_gpo_etiqueta
         AND    a.nombre_campo = "municipio";


         LET v_id_atr_nivel = seq_mdt_cat_atributo_nivel.NEXTVAL;
         INSERT INTO mdt_cat_atributo_nivel VALUES (v_id_atr_nivel,
                                                    vmapa_id_gpo_etiqueta ,
                                                    vcatmdt_id_cat_mandato ,
                                                    v_id_gpo_mandato  ,
                                                    2 ,
                                                    0 ,
                                                    p_usuario );

         INSERT INTO mdt_cat_instancia_mandato VALUES (seq_mdt_cat_instancia_mandato.NEXTVAL,
                                                       v_id_atr_nivel ,
                                                       v_municipio_desc ,
                                                       p_usuario ); 

         SELECT a.id_cat_gpo    ,
                a.id_gpo_etiqueta ,
                b.etiqueta
         INTO   vmapa_id_cat_gpo,
                vmapa_id_gpo_etiqueta ,
                vmapa_etiqueta 
         FROM   hps_cat_mapa_etiqueta a,
                mdt_cat_gpo_etiqueta b
         WHERE  a.id_gpo_etiqueta = b.id_gpo_etiqueta
         AND    a.nombre_campo = "numero_cuenta";

         LET v_id_atr_nivel = seq_mdt_cat_atributo_nivel.NEXTVAL;
         INSERT INTO mdt_cat_atributo_nivel VALUES (v_id_atr_nivel,
                                                    vmapa_id_gpo_etiqueta ,
                                                    vcatmdt_id_cat_mandato ,
                                                    v_id_gpo_mandato  ,
                                                    3 ,
                                                    0 ,
                                                    p_usuario );

         INSERT INTO mdt_cat_instancia_mandato VALUES (seq_mdt_cat_instancia_mandato.NEXTVAL,
                                                       v_id_atr_nivel ,
                                                       v_numero_cuenta ,
                                                       p_usuario ); 

         SELECT a.id_cat_gpo    ,
                a.id_gpo_etiqueta ,
                b.etiqueta
         INTO   vmapa_id_cat_gpo,
                vmapa_id_gpo_etiqueta ,
                vmapa_etiqueta 
         FROM   hps_cat_mapa_etiqueta a,
                mdt_cat_gpo_etiqueta b
         WHERE  a.id_gpo_etiqueta = b.id_gpo_etiqueta
         AND    a.nombre_campo = "administrador";

         LET v_id_atr_nivel = seq_mdt_cat_atributo_nivel.NEXTVAL;
         INSERT INTO mdt_cat_atributo_nivel VALUES (v_id_atr_nivel,
                                                    vmapa_id_gpo_etiqueta ,
                                                    vcatmdt_id_cat_mandato ,
                                                    v_id_gpo_mandato  ,
                                                    4 ,
                                                    0 ,
                                                    p_usuario );

         INSERT INTO mdt_cat_instancia_mandato VALUES (seq_mdt_cat_instancia_mandato.NEXTVAL,
                                                       v_id_atr_nivel ,
                                                       v_administrador ,
                                                       p_usuario ); 

         SELECT a.id_cat_gpo    ,
                a.id_gpo_etiqueta ,
                b.etiqueta
         INTO   vmapa_id_cat_gpo,
                vmapa_id_gpo_etiqueta ,
                vmapa_etiqueta 
         FROM   hps_cat_mapa_etiqueta a,
                mdt_cat_gpo_etiqueta b
         WHERE  a.id_gpo_etiqueta = b.id_gpo_etiqueta
         AND    a.nombre_campo = "promotor_vecinal";


         LET v_id_atr_nivel = seq_mdt_cat_atributo_nivel.NEXTVAL;
         INSERT INTO mdt_cat_atributo_nivel VALUES (v_id_atr_nivel,
                                                    vmapa_id_gpo_etiqueta ,
                                                    vcatmdt_id_cat_mandato ,
                                                    v_id_gpo_mandato  ,
                                                    5 ,
                                                    0 ,
                                                    p_usuario );

         INSERT INTO mdt_cat_instancia_mandato VALUES (seq_mdt_cat_instancia_mandato.NEXTVAL,
                                                       v_id_atr_nivel ,
                                                       v_promotor_vecinal,
                                                       p_usuario ); 

         SELECT a.id_cat_gpo    ,
                a.id_gpo_etiqueta ,
                b.etiqueta
         INTO   vmapa_id_cat_gpo,
                vmapa_id_gpo_etiqueta ,
                vmapa_etiqueta 
         FROM   hps_cat_mapa_etiqueta a,
                mdt_cat_gpo_etiqueta b
         WHERE  a.id_gpo_etiqueta = b.id_gpo_etiqueta
         AND    a.nombre_campo = "numero_acreedor";


         LET v_id_atr_nivel = seq_mdt_cat_atributo_nivel.NEXTVAL;
         INSERT INTO mdt_cat_atributo_nivel VALUES (v_id_atr_nivel,
                                                    vmapa_id_gpo_etiqueta ,
                                                    vcatmdt_id_cat_mandato ,
                                                    v_id_gpo_mandato  ,
                                                    6 ,
                                                    0 ,
                                                    p_usuario );

         INSERT INTO mdt_cat_instancia_mandato VALUES (seq_mdt_cat_instancia_mandato.NEXTVAL,
                                                       v_id_atr_nivel ,
                                                       v_numero_acreedor,
                                                       p_usuario );
													   
													   
         SELECT a.id_cat_gpo    ,
                a.id_gpo_etiqueta ,
                b.etiqueta
         INTO   vmapa_id_cat_gpo,
                vmapa_id_gpo_etiqueta ,
                vmapa_etiqueta 
         FROM   hps_cat_mapa_etiqueta a,
                mdt_cat_gpo_etiqueta b
         WHERE  a.id_gpo_etiqueta = b.id_gpo_etiqueta
         AND    a.nombre_campo = "rfc";


         LET v_id_atr_nivel = seq_mdt_cat_atributo_nivel.NEXTVAL;
         INSERT INTO mdt_cat_atributo_nivel VALUES (v_id_atr_nivel,
                                                    vmapa_id_gpo_etiqueta ,
                                                    vcatmdt_id_cat_mandato ,
                                                    v_id_gpo_mandato  ,
                                                    7 ,
                                                    0 ,
                                                    p_usuario );

         INSERT INTO mdt_cat_instancia_mandato VALUES (seq_mdt_cat_instancia_mandato.NEXTVAL,
                                                       v_id_atr_nivel ,
                                                       v_rfc,
                                                       p_usuario );


      END IF; --2 FIN IF TIPO MANDATO
	 END IF; -- Fin de validación
   END FOREACH;
   
   RETURN v_error_sql,
		  v_isam_error,
		  v_msg_error;
		  
END PROCEDURE;


