






CREATE FUNCTION "safreviv".sp_hps_actualiza_ent_rec_mdt(p_desc_mdt varchar(40),p_etiqueta_mdt CHAR(40),p_valor_eti CHAR(120))
--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 23 de junio de 2015
-- Valida si la etiqueta existe para el mandato y si no existe la añade 
-- como una nueva etiqueta.
--===============================================================
RETURNING SMALLINT,
		  CHAR(3),
		  INTEGER,
          INTEGER,
          CHAR(254);

--Definiendo variables de salida
DEFINE v_ind		SMALLINT;
DEFINE v_diag		CHAR(3);		  
DEFINE v_error_sql  INTEGER;
DEFINE v_isam_error INTEGER;
DEFINE v_msg_error  CHAR(254);

--Definiendo variales internas
DEFINE v_etiqueta 		CHAR(40);
DEFINE v_valor_etiqueta	CHAR(120);
DEFINE v_id_instancia   DECIMAL(9,0);
DEFINE v_id_atributo    DECIMAL(9,0);

DEFINE v_id_atr_nivel     DECIMAL(9,0);
DEFINE v_id_cat_mandato SMALLINT;
DEFINE v_id_gpo_etiqueta INTEGER;
DEFINE v_id_cat_gpo		DECIMAL(9,0);		
DEFINE v_usuario 		CHAR(20);
DEFINE v_orden          SMALLINT;
DEFINE v_id_habilita    SMALLINT;

--retorna datos segun excepcion
ON EXCEPTION SET 	v_error_sql,
                    v_isam_error,
                    v_msg_error
					
   LET v_ind = 1;
   LET v_diag = "999"; --EXCEPCION EN LA BD
   
   RETURN v_ind,
		  v_diag,
		  v_error_sql,
		  v_isam_error,
		  v_msg_error;

END EXCEPTION

--inicia la función, definiendo valores iniciales
   LET v_ind = 0;
   LET v_diag = "000"; -- SIN CAMBIOS
   LET v_error_sql  = 0;
   LET v_isam_error = 0;
   LET v_msg_error  = "Consulta realizada correctamente";

   --query para validar si existe la etiqueta
   PREPARE prp_etiqueta FROM "SELECT d.etiqueta,e.valor_etiqueta,e.id_instancia_mandato,b.id_atr_nivel FROM mdt_cat_mandato a,mdt_cat_atributo_nivel b,mdt_tpo_mandato c,mdt_cat_gpo_etiqueta d,mdt_cat_instancia_mandato e WHERE a.id_cat_mandato = b.id_cat_mandato AND a.tpo_mandato = c.tpo_mandato AND b.id_gpo_etiqueta = d.id_gpo_etiqueta AND b.id_atr_nivel = e.id_atr_nivel AND a.desc_mandato = ? AND d.etiqueta = ? ";
   DECLARE cur_etiqueta CURSOR FOR prp_etiqueta;
   OPEN cur_etiqueta USING p_desc_mdt,p_etiqueta_mdt;
   FETCH cur_etiqueta INTO 	v_etiqueta,
							v_valor_etiqueta,
							v_id_instancia,
							v_id_atributo;
   
   IF SQLCODE == 0 THEN --Existe la etiqueta
     
	  IF p_valor_eti == v_valor_etiqueta THEN
	     LET v_diag = "001"; -- EXISTE ETIQUETA Y TIENE MISMO VALOR
	  ELSE
	     UPDATE mdt_cat_instancia_mandato SET valor_etiqueta = p_valor_eti
	        WHERE id_instancia_mandato = v_id_instancia
            AND id_atr_nivel = v_id_atributo;
	     LET v_diag = "002"; -- EXISTE ETIQUETA Y SE ACTUALIZA
	  END IF;
	  
   ELSE
      IF SQLCODE == 100 THEN --No existe
	  
		 -- INSERT EN mdt_cat_atributo_nivel
		    --Para realizar el insert se toman primero los datos necesarios
		 
		 LET v_id_habilita = 0;
		 
		 SELECT MAX(id_atr_nivel)+1 INTO v_id_atr_nivel FROM mdt_cat_atributo_nivel;
		 SELECT MAX(id_instancia_mandato)+1 INTO v_id_instancia FROM mdt_cat_instancia_mandato;
		 
		 PREPARE prp_mandato FROM "SELECT id_cat_mandato FROM mdt_cat_mandato WHERE desc_mandato = ? ORDER BY f_creacion DESC"; 
		 DECLARE cur_mandato CURSOR FOR prp_mandato;
		 OPEN cur_mandato USING p_desc_mdt;
		 FETCH cur_mandato INTO v_id_cat_mandato;
		 IF SQLCODE == 100 THEN
		       LET v_ind = 1;
			   LET v_diag = "004"; --NO EXISTE MANDATO
			   CLOSE cur_mandato;
			   FREE cur_mandato;
			   FREE prp_mandato;
			   RETURN 	v_ind,
						v_diag,
						v_error_sql,
						v_isam_error,
						v_msg_error;
		 END IF;
		 CLOSE cur_mandato;
         FREE cur_mandato;
         FREE prp_mandato;
		 
		 PREPARE prp_gpo_etiqueta FROM "SELECT id_gpo_etiqueta,id_cat_gpo,usuario FROM mdt_cat_gpo_etiqueta WHERE etiqueta = ? ";
		 DECLARE cur_gpo_etiqueta CURSOR FOR prp_gpo_etiqueta;
		 OPEN cur_gpo_etiqueta USING p_etiqueta_mdt;
		 FETCH cur_gpo_etiqueta INTO v_id_gpo_etiqueta,v_id_cat_gpo,v_usuario;
		 IF SQLCODE == 100 THEN
		       LET v_ind = 1;
			   LET v_diag = "005"; --NO EXISTE ETIQUETA
			   CLOSE cur_gpo_etiqueta;
			   FREE cur_gpo_etiqueta;
			   FREE prp_gpo_etiqueta;
			   RETURN 	v_ind,
						v_diag,
						v_error_sql,
						v_isam_error,
						v_msg_error;
		 END IF;
		 CLOSE cur_gpo_etiqueta;
	     FREE cur_gpo_etiqueta;
	     FREE prp_gpo_etiqueta;
		 
		 PREPARE prp_obtiene_orden FROM "SELECT orden FROM mdt_gpo_mandato WHERE id_cat_mandato = ? AND id_cat_gpo = ? ";
		 DECLARE cur_orden CURSOR FOR prp_obtiene_orden;
		 OPEN cur_orden USING v_id_cat_mandato,v_id_cat_gpo;
		 FETCH cur_orden INTO v_orden;
		 IF SQLCODE == 100 THEN
		       LET v_ind = 1;
			   LET v_diag = "006"; -- ERROR AL HAYAR ORDEN
			   CLOSE cur_orden;
			   FREE cur_orden;
			   FREE prp_obtiene_orden;
			   RETURN 	v_ind,
						v_diag,
						v_error_sql,
						v_isam_error,
						v_msg_error;
		 END IF;
		 CLOSE cur_orden;
	     FREE cur_orden;
	     FREE prp_obtiene_orden;
		 
		 INSERT INTO mdt_cat_atributo_nivel VALUES (v_id_atr_nivel,v_id_gpo_etiqueta,v_id_cat_mandato,v_id_cat_gpo,v_orden,v_id_habilita,v_usuario);
		 INSERT INTO mdt_cat_instancia_mandato VALUES (v_id_instancia,v_id_atr_nivel,p_valor_eti,v_usuario);
		 
		 LET v_diag = "003"; -- 003 NO EXISTE ETIQUETA Y SE AGREGA NUEVA
		 
	  END IF;
   END IF;
	
   CLOSE cur_etiqueta;
   FREE cur_etiqueta;
   FREE prp_etiqueta;
   
   RETURN v_ind,
		  v_diag,
		  v_error_sql,
		  v_isam_error,
		  v_msg_error;

END FUNCTION;


