--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 07-09-2012
--==============================================================================

################################################################################
#Modulo            => MDT                                                      #
#Programa          => MDTP21                                                   #
#Objetivo          => Programa batch de carga inicial de información           # 
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => Septiembre 07, 2012                                      #
################################################################################

DATABASE safre_viv

MAIN
DEFINE v_consulta STRING,
       v_dat_mandato RECORD
         v_tipo_registro SMALLINT,
         v_consecutivo   SMALLINT,
         v_entidad_federativa SMALLINT,
         v_descripcion_larga VARCHAR(120),
         v_descripcion_corta VARCHAR(120),
         v_localidad         VARCHAR(120),
         v_municipio         SMALLINT,
         v_administrador     VARCHAR(120),
         v_rfc               VARCHAR(120),
         v_numero_acreedor   VARCHAR(50),
         v_numero_cuenta     VARCHAR(50),  
         v_promotor_vecinal  VARCHAR(120)
       END RECORD,
       v_id_cat_mandato LIKE mdt_cat_mandato.id_cat_mandato,
       v_id_cat_mandato_aux LIKE mdt_cat_mandato.id_cat_mandato,
       v_cat_mandato RECORD
         v_cve_mandato  LIKE mdt_cat_mandato.cve_mandato,
         v_desc_mandato LIKE mdt_cat_mandato.desc_mandato,
         v_desc_larga_mandato LIKE mdt_cat_mandato.desc_larga_mandato,
         v_usuario            LIKE mdt_cat_mandato.usuario,
         v_tpo_mandato        LIKE mdt_cat_mandato.tpo_mandato,
         v_f_creacion         LIKE mdt_cat_mandato.f_creacion,
         v_estado             LIKE mdt_cat_mandato.estado
       END RECORD,
       v_paquete        CHAR(18),
       v_id_cat_gpo     LIKE mdt_cat_gpo.id_cat_gpo,
       v_id_gpo_mandato LIKE mdt_gpo_mandato.id_gpo_mandato,
       v_orden          LIKE mdt_gpo_mandato.orden,
       v_orden_atr      LIKE mdt_cat_atributo_nivel.orden,
       v_usuario        LIKE seg_usuario.usuario_cod,
       v_id_atr_nivel   LIKE mdt_cat_atributo_nivel.id_atr_nivel,
       v_etiqueta       LIKE mdt_cat_gpo_etiqueta.etiqueta,
       v_id_gpo_etiqueta LIKE mdt_cat_gpo_etiqueta.id_gpo_etiqueta,
       v_id_habilita     LIKE mdt_cat_atributo_nivel.id_habilita,
       v_id_instancia_mandato LIKE mdt_cat_instancia_mandato.id_instancia_mandato,
       v_municipio_desc LIKE cat_municipio.municipio_desc,
       v_entidad_desc   LIKE cat_entidad_federativa.entidad_desc_larga,
       v_indice         INTEGER

   DISPLAY "INICIA CARGA INCIAL"
   LET v_indice = 0
   LET v_orden = 1
   LET v_usuario = "safreviv"
   LET v_id_habilita = 0
   DISPLAY "."
   
   LET v_consulta = "\n SELECT tipo_registro,consecutivo,entidad_federativa,",
                    "\n        descripcion_larga,descripcion_corta,localidad,",
                    "\n        municipio,administrador,rfc,numero_acreedor,",
                    "\n        numero_cuenta,promotor_vecinal",
                    "\n   FROM safre_tmp:tmp_mdt_det1",
                    "\n  WHERE 1 = 1"
   PREPARE prp_rec_datos FROM v_consulta
   DECLARE cur_rec_datos CURSOR FOR prp_rec_datos

   LET v_consulta = "\n INSERT INTO mdt_cat_mandato(",
                    "\n id_cat_mandato,",
                    "\n cve_mandato,",
                    "\n desc_mandato,",
                    "\n desc_larga_mandato,",
                    "\n usuario,",
                    "\n tpo_mandato,",
                    "\n f_creacion,",
                    "\n estado)",
                    "\n VALUES(?,?,?,?,?,?,?,?)"
   PREPARE prp_ins_cat_mandato FROM v_consulta 
   DISPLAY ".."
   LET v_consulta = "\n SELECT paquete",
                    "\n   FROM safre_tmp:tmp_mdt_det2",
                    "\n  WHERE consecutivo = ?"
   PREPARE prp_rec_paquete FROM v_consulta
   DECLARE cur_rec_paquete CURSOR FOR prp_rec_paquete

   LET v_consulta = "\n INSERT INTO mdt_cat_mandato_paquete(cve_mandato,id_cat_mandato)",
                    "\n VALUES(?,?)"
   PREPARE prp_ins_paquete FROM v_consulta 
   DISPLAY "..."
   LET v_consulta = "\n SELECT NVL(MAX(id_cat_mandato),0)+1",
                    "\n   FROM mdt_cat_mandato"
   PREPARE prp_rec_max_id_mandato FROM v_consulta
   
   LET v_consulta = "\n INSERT INTO mdt_gpo_mandato(",
                    "\n id_gpo_mandato,",
                    "\n id_cat_mandato,",
                    "\n id_cat_gpo,",
                    "\n orden,",
                    "\n usuario)",
                    "\n VALUES(?,?,?,?,?)"
   PREPARE prp_ins_gpo_mandato FROM v_consulta

   LET v_consulta = "\n SELECT NVL(MAX(id_gpo_mandato),0)+1",
                    "\n   FROM mdt_gpo_mandato"
   PREPARE prp_rec_max_id_gpo_mandato FROM v_consulta
   DISPLAY "...."
   LET v_consulta = "\n INSERT INTO mdt_cat_atributo_nivel(",
                    "\n id_atr_nivel,",
                    "\n id_gpo_etiqueta,",
                    "\n id_cat_mandato,",
                    "\n id_gpo_mandato,",
                    "\n orden,",
                    "\n id_habilita,",
                    "\n usuario)",
                    "\n VALUES(?,?,?,?,?,?,?)"
   PREPARE prp_ins_atr_nivel FROM v_consulta  

   LET v_consulta = "\n SELECT NVL(MAX(id_atr_nivel),0)+1",
                    "\n   FROM mdt_cat_atributo_nivel"
   PREPARE prp_rec_max_id_atr_nivel FROM v_consulta

   LET v_consulta = "\n SELECT NVL(MAX(orden),0)+1",
                    "\n   FROM mdt_cat_atributo_nivel",
                    "\n  WHERE id_cat_mandato = ?"
   PREPARE prp_rec_max_orden FROM v_consulta

   LET v_consulta = "\n SELECT id_gpo_etiqueta",
                    "\n   FROM mdt_cat_gpo_etiqueta",
                    "\n  WHERE etiqueta =?"
   PREPARE prp_rec_id_gpo_etiqueta FROM v_consulta
   DISPLAY "....."
   LET v_consulta = "\n INSERT INTO mdt_cat_instancia_mandato(",
                    "\n id_instancia_mandato,",
                    "\n id_atr_nivel,",
                    "\n valor_etiqueta,",
                    "\n usuario)",
                    "\n VALUES(?,?,?,?)"
   PREPARE prp_ins_instancia_mandato FROM v_consulta
   LET v_consulta = "\n SELECT NVL(MAX(id_instancia_mandato),0)+1",
                    "\n   FROM mdt_cat_instancia_mandato"                    
   PREPARE prp_rec_max_id_instancia_mandato FROM v_consulta

   LET v_consulta = "\n SELECT municipio_desc",
                    "\n   FROM cat_municipio",
                    "\n  WHERE municipio = ?",
                    "\n  ORDER BY 1"
   PREPARE prp_rec_municipio_desc FROM v_consulta
   
   LET v_consulta = "\n SELECT entidad_desc_larga",
                    "\n   FROM cat_entidad_federativa",
                    "\n  WHERE entidad_federativa = ?",
                    "\n  ORDER BY 1"
   PREPARE prp_rec_entidad_desc FROM v_consulta
   
   DISPLAY "Inicia insercion de infromación"
    
   FOREACH cur_rec_datos INTO v_dat_mandato.*
      # Recupera el maximo id del catálogo de mandatos
      EXECUTE prp_rec_max_id_mandato INTO v_id_cat_mandato
      # Todos los registros se guardan como mandatos de tipo MANTENIMIENTO
      LET v_dat_mandato.v_tipo_registro = 2
      
      LET v_cat_mandato.v_cve_mandato = v_dat_mandato.v_tipo_registro USING "&&",v_dat_mandato.v_consecutivo USING "&&&&&"
      LET v_cat_mandato.v_desc_larga_mandato = v_dat_mandato.v_descripcion_larga
      LET v_cat_mandato.v_desc_mandato       = v_dat_mandato.v_descripcion_corta
      LET v_cat_mandato.v_estado             = 100 # aceptado
      LET v_cat_mandato.v_f_creacion         = TODAY
      LET v_cat_mandato.v_tpo_mandato        = v_dat_mandato.v_tipo_registro
      LET v_cat_mandato.v_usuario            = v_usuario
      # inserta en mdt_cat_mandato
      EXECUTE prp_ins_cat_mandato USING v_id_cat_mandato,v_cat_mandato.*
      # recupera todos los paquetes realcionados al mandato para insertar en el catalogo
      --DISPLAY "v_dat_mandato.v_consecutivo: ",v_dat_mandato.v_consecutivo
      FOREACH cur_rec_paquete USING v_dat_mandato.v_consecutivo
                               INTO v_paquete
         --DISPLAY v_id_cat_mandato," v_paquete:",v_paquete
         # inserta en mdt_cat_mandato_paquete
         EXECUTE prp_ins_paquete USING v_paquete,v_id_cat_mandato
         INITIALIZE v_paquete TO NULL
      END FOREACH

      
      # inserta la relacion del grupo direccion con el madato
      IF(v_dat_mandato.v_entidad_federativa > 0 OR v_dat_mandato.v_municipio > 0)THEN
         # identificador de grupo diereccion
         LET v_id_cat_gpo = 1
         # recupera el maximo de mdt_gpo_mandato
         EXECUTE prp_rec_max_id_gpo_mandato INTO v_id_gpo_mandato
         EXECUTE prp_ins_gpo_mandato USING v_id_gpo_mandato,
                                           v_id_cat_mandato,
                                           v_id_cat_gpo,
                                           v_orden,
                                           v_usuario
         # ingresa información en mdt_cat_atributo_nivel para ENTIDAD FEDERATIVA 
         IF(v_dat_mandato.v_entidad_federativa > 0)THEN
            EXECUTE prp_rec_max_id_atr_nivel INTO v_id_atr_nivel
            LET v_etiqueta = "ENTIDAD FEDERATIVA"
            EXECUTE prp_rec_id_gpo_etiqueta USING v_etiqueta 
                                             INTO v_id_gpo_etiqueta
            EXECUTE prp_rec_max_orden USING v_id_cat_mandato
                                       INTO v_orden_atr
            EXECUTE prp_ins_atr_nivel USING v_id_atr_nivel,
                                            v_id_gpo_etiqueta,
                                            v_id_cat_mandato,
                                            v_id_gpo_mandato,
                                            v_orden_atr,
                                            v_id_habilita,
                                            v_usuario

            EXECUTE prp_rec_entidad_desc USING v_dat_mandato.v_entidad_federativa
                                          INTO v_entidad_desc
            # inserta valor de la etiqueta
            EXECUTE prp_rec_max_id_instancia_mandato INTO v_id_instancia_mandato
            EXECUTE prp_ins_instancia_mandato USING v_id_instancia_mandato,
                                                    v_id_atr_nivel,
                                                    v_entidad_desc,
                                                    v_usuario
            

         END IF
         # ingresa información en mdt_cat_atributo_nivel para MUNICIPIO 
         IF(v_dat_mandato.v_municipio > 0)THEN
            EXECUTE prp_rec_max_id_atr_nivel INTO v_id_atr_nivel
            LET v_etiqueta = "MUNICIPIO"
            EXECUTE prp_rec_id_gpo_etiqueta USING v_etiqueta 
                                             INTO v_id_gpo_etiqueta
            EXECUTE prp_rec_max_orden USING v_id_cat_mandato
                                       INTO v_orden_atr
            EXECUTE prp_ins_atr_nivel USING v_id_atr_nivel,
                                            v_id_gpo_etiqueta,
                                            v_id_cat_mandato,
                                            v_id_gpo_mandato,
                                            v_orden_atr,
                                            v_id_habilita,
                                            v_usuario

            EXECUTE prp_rec_municipio_desc USING v_dat_mandato.v_municipio
                                            INTO v_municipio_desc 
             
            # inserta valor de la etiqueta
            EXECUTE prp_rec_max_id_instancia_mandato INTO v_id_instancia_mandato
            EXECUTE prp_ins_instancia_mandato USING v_id_instancia_mandato,
                                                    v_id_atr_nivel,
                                                    v_municipio_desc,
                                                    v_usuario

         END IF
      END IF
      # inserta la relacion del grupo banco con el madato
      IF(LENGTH(v_dat_mandato.v_numero_cuenta CLIPPED) > 0)THEN
         # identificador de grupo banco
         LET v_id_cat_gpo = 2
         # recupera el maximo de mdt_gpo_mandato
         EXECUTE prp_rec_max_id_gpo_mandato INTO v_id_gpo_mandato
         EXECUTE prp_ins_gpo_mandato USING v_id_gpo_mandato,
                                           v_id_cat_mandato,
                                           v_id_cat_gpo,
                                           v_orden,
                                           v_usuario
         # ingresa información en mdt_cat_atributo_nivel para NUMERO DE CUENTA 
         EXECUTE prp_rec_max_id_atr_nivel INTO v_id_atr_nivel
         LET v_etiqueta = "NUMERO DE CUENTA"
         EXECUTE prp_rec_id_gpo_etiqueta USING v_etiqueta 
                                          INTO v_id_gpo_etiqueta
         EXECUTE prp_rec_max_orden USING v_id_cat_mandato
                                    INTO v_orden_atr
         EXECUTE prp_ins_atr_nivel USING v_id_atr_nivel,
                                         v_id_gpo_etiqueta,
                                         v_id_cat_mandato,
                                         v_id_gpo_mandato,
                                         v_orden_atr,
                                         v_id_habilita,
                                         v_usuario
         # inserta valor de la etiqueta
         EXECUTE prp_rec_max_id_instancia_mandato INTO v_id_instancia_mandato
         EXECUTE prp_ins_instancia_mandato USING v_id_instancia_mandato,
                                                 v_id_atr_nivel,
                                                 v_dat_mandato.v_numero_cuenta,
                                                 v_usuario
      END IF
      # inserta la relacion del grupo datos <mantenimiento> con el madato
      IF(LENGTH(v_dat_mandato.v_administrador CLIPPED) > 0 OR
         LENGTH(v_dat_mandato.v_rfc CLIPPED) > 0 OR
         LENGTH(v_dat_mandato.v_numero_acreedor CLIPPED) > 0 OR
         LENGTH(v_dat_mandato.v_promotor_vecinal CLIPPED) > 0 OR
         LENGTH(v_dat_mandato.v_localidad CLIPPED) > 0)THEN
         # identificador de grupo mantenimiento
         LET v_id_cat_gpo = 3
         # recupera el maximo de mdt_gpo_mandato
         EXECUTE prp_rec_max_id_gpo_mandato INTO v_id_gpo_mandato
         EXECUTE prp_ins_gpo_mandato USING v_id_gpo_mandato,
                                           v_id_cat_mandato,
                                           v_id_cat_gpo,
                                           v_orden,
                                           v_usuario

         # ingresa información en mdt_cat_atributo_nivel para ADMINISTRADOR 
         IF(LENGTH(v_dat_mandato.v_administrador CLIPPED) > 0)THEN
            EXECUTE prp_rec_max_id_atr_nivel INTO v_id_atr_nivel
            LET v_etiqueta = "ADMINISTRADOR"
            EXECUTE prp_rec_id_gpo_etiqueta USING v_etiqueta 
                                             INTO v_id_gpo_etiqueta
            EXECUTE prp_rec_max_orden USING v_id_cat_mandato
                                       INTO v_orden_atr
            EXECUTE prp_ins_atr_nivel USING v_id_atr_nivel,
                                            v_id_gpo_etiqueta,
                                            v_id_cat_mandato,
                                            v_id_gpo_mandato,
                                            v_orden_atr,
                                            v_id_habilita,
                                            v_usuario
            # inserta valor de la etiqueta
            EXECUTE prp_rec_max_id_instancia_mandato INTO v_id_instancia_mandato
            EXECUTE prp_ins_instancia_mandato USING v_id_instancia_mandato,
                                                    v_id_atr_nivel,
                                                    v_dat_mandato.v_administrador,
                                                    v_usuario
         END IF
         # ingresa información en mdt_cat_atributo_nivel para RFC
         IF(LENGTH(v_dat_mandato.v_rfc CLIPPED) > 0)THEN
            EXECUTE prp_rec_max_id_atr_nivel INTO v_id_atr_nivel
            LET v_etiqueta = "RFC"
            EXECUTE prp_rec_id_gpo_etiqueta USING v_etiqueta 
                                             INTO v_id_gpo_etiqueta
            EXECUTE prp_rec_max_orden USING v_id_cat_mandato
                                       INTO v_orden_atr
            EXECUTE prp_ins_atr_nivel USING v_id_atr_nivel,
                                            v_id_gpo_etiqueta,
                                            v_id_cat_mandato,
                                            v_id_gpo_mandato,
                                            v_orden_atr,
                                            v_id_habilita,
                                            v_usuario
            # inserta valor de la etiqueta
            EXECUTE prp_rec_max_id_instancia_mandato INTO v_id_instancia_mandato
            EXECUTE prp_ins_instancia_mandato USING v_id_instancia_mandato,
                                                    v_id_atr_nivel,
                                                    v_dat_mandato.v_rfc,
                                                    v_usuario
         END IF
         # ingresa información en mdt_cat_atributo_nivel para numero_acreedor
         IF(LENGTH(v_dat_mandato.v_numero_acreedor CLIPPED) > 0)THEN
            EXECUTE prp_rec_max_id_atr_nivel INTO v_id_atr_nivel
            LET v_etiqueta = "NUMERO ACREEDOR"
            EXECUTE prp_rec_id_gpo_etiqueta USING v_etiqueta 
                                             INTO v_id_gpo_etiqueta
            EXECUTE prp_rec_max_orden USING v_id_cat_mandato
                                       INTO v_orden_atr
            EXECUTE prp_ins_atr_nivel USING v_id_atr_nivel,
                                            v_id_gpo_etiqueta,
                                            v_id_cat_mandato,
                                            v_id_gpo_mandato,
                                            v_orden_atr,
                                            v_id_habilita,
                                            v_usuario
            # inserta valor de la etiqueta
            EXECUTE prp_rec_max_id_instancia_mandato INTO v_id_instancia_mandato
            EXECUTE prp_ins_instancia_mandato USING v_id_instancia_mandato,
                                                    v_id_atr_nivel,
                                                    v_dat_mandato.v_numero_acreedor,
                                                    v_usuario
         END IF
         # ingresa información en mdt_cat_atributo_nivel para PROMOTOR VECINAL
         IF(LENGTH(v_dat_mandato.v_promotor_vecinal CLIPPED) > 0)THEN
            EXECUTE prp_rec_max_id_atr_nivel INTO v_id_atr_nivel
            LET v_etiqueta = "PROMOTOR VECINAL"
            EXECUTE prp_rec_id_gpo_etiqueta USING v_etiqueta 
                                             INTO v_id_gpo_etiqueta
            EXECUTE prp_rec_max_orden USING v_id_cat_mandato
                                       INTO v_orden_atr
            EXECUTE prp_ins_atr_nivel USING v_id_atr_nivel,
                                            v_id_gpo_etiqueta,
                                            v_id_cat_mandato,
                                            v_id_gpo_mandato,
                                            v_orden_atr,
                                            v_id_habilita,
                                            v_usuario
            # inserta valor de la etiqueta
            EXECUTE prp_rec_max_id_instancia_mandato INTO v_id_instancia_mandato
            EXECUTE prp_ins_instancia_mandato USING v_id_instancia_mandato,
                                                    v_id_atr_nivel,
                                                    v_dat_mandato.v_promotor_vecinal,
                                                    v_usuario
         END IF
         # ingresa información en mdt_cat_atributo_nivel para LOCALIDAD
         IF(LENGTH(v_dat_mandato.v_localidad CLIPPED) > 0)THEN
            EXECUTE prp_rec_max_id_atr_nivel INTO v_id_atr_nivel
            LET v_etiqueta = "LOCALIDAD"
            EXECUTE prp_rec_id_gpo_etiqueta USING v_etiqueta 
                                             INTO v_id_gpo_etiqueta
            EXECUTE prp_rec_max_orden USING v_id_cat_mandato
                                       INTO v_orden_atr
            EXECUTE prp_ins_atr_nivel USING v_id_atr_nivel,
                                            v_id_gpo_etiqueta,
                                            v_id_cat_mandato,
                                            v_id_gpo_mandato,
                                            v_orden_atr,
                                            v_id_habilita,
                                            v_usuario
            # inserta valor de la etiqueta
            EXECUTE prp_rec_max_id_instancia_mandato INTO v_id_instancia_mandato
            EXECUTE prp_ins_instancia_mandato USING v_id_instancia_mandato,
                                                    v_id_atr_nivel,
                                                    v_dat_mandato.v_localidad,
                                                    v_usuario
         END IF
      END IF      
      
      
      LET v_indice = v_indice + 1
   END FOREACH
   DISPLAY "Regstros procesados: ",v_indice 
   DISPLAY "\n"
   DISPLAY "FIN CARGA INICIAL"

END MAIN