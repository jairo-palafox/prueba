--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================
################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPE26                                                   #
#Descripcion       => Batch para carga de restitucion de ssv                   #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => Junio 22, 2012                                           #
################################################################################

DATABASE safre_viv

DEFINE v_restitucion RECORD
         v_tpo_registro          			SMALLINT,
	     v_folio_procesar        			CHAR(40),
	     v_cve_afore             			SMALLINT,
	     v_invadido              			CHAR(11),
	     v_asociado              			CHAR(11),
	     v_nombre_reclamante    	 	    CHAR(120),
	     v_n_caso               		 	CHAR(40),
	     v_sar92_aivs_solicitado  			DECIMAL(22,2),
	     v_sar92_pesos_solicitado  		    DECIMAL(22,2),
	     v_viv97_aivs_solicitado  			DECIMAL(22,2),
	     v_viv97_pesos_solicitado  		    DECIMAL(22,2),
	     v_sdo_reclamante_solicitado  	    DECIMAL(22,2),
	     v_sdo_no_reclamante_solicitado     DECIMAL(22,2),
	     v_sar92_pesos_devolver             DECIMAL(22,2),
	     v_sar92_aivs_devolver              DECIMAL(22,2),
	     v_viv97_pesos_devolver             DECIMAL(22,2),
	     v_viv97_aivs_devolver              DECIMAL(22,2),
	     v_subsc_viv97_pesos_devolver       DECIMAL(22,2),
	     v_subsc_viv97_aivs_devolver  	    DECIMAL(22,2),
	     v_subsc_viv97_pesos_no_aplica      DECIMAL(22,2),
	     v_subsc_viv97_aivs_no_aplica       DECIMAL(22,2),
	     v_subsc_viv97_pesos_no_aplica_pscd DECIMAL(22,2),
	     v_subsc_viv97_aivs_no_aplica_pscd  DECIMAL(22,2),
	     v_amort_devolver        		    DECIMAL(22,2),
	     v_avance_aportacion_devolver       DECIMAL(22,2),
	     v_avance_amort_devolver            DECIMAL(22,2),
	     v_dif_aivs_sar92                   DECIMAL(22,2),
	     v_dif_aivs_viv97                   DECIMAL(22,2),
	     v_dif_pesos_ret72                  DECIMAL(22,2),
	     v_sdo_total_115        	        DECIMAL(22,2),
	     v_nss_115                          CHAR(11),
	     v_sar92_pesos_115_trabajador       DECIMAL(22,2),
	     v_sar92_aivs_115_trabajador        DECIMAL(22,2),
	     v_viv97_pesos_115_trabajador       DECIMAL(22,2),
	     v_viv97_aivs_115_trabajador        DECIMAL(22,2),
	     v_sbusc_viv97_pesos_115_trabajador DECIMAL(22,2),
	     v_subsc_viv97_aivs_115_trabajador  DECIMAL(22,2),
	     v_sar92_pesos_115_acreditado       DECIMAL(22,2),
	     v_sar92_aivs_115_acreditado        DECIMAL(22,2),
	     v_viv97_pesos_115_acreditado       DECIMAL(22,2),
	     v_viv97_aivs_115_acreditado        DECIMAL(22,2),
	     v_subsc_viv97_pesos_115_acreditado DECIMAL(22,2),
	     v_subsc_viv97_aivs_115_acreditado  DECIMAL(22,2),
	     v_observacion                      CHAR(120)
       END RECORD  

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPE26                                                   #
#Objetivo          => carga datos de restitucion aplicadas y no aplicadas      #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => Junio 22, 2012                                           #
#Modificación      => 21 Enero 2015 - modificaciones para contemplar           #
#                     restitución complementaria                               #
################################################################################
--MAIN
FUNCTION fn_carga_restitucion(p_id_expediente,p_usuario,p_parametro,v_nom_archivo)
DEFINE ch base.Channel
-- jdym
DEFINE v_carga       CHAR(80)
DEFINE p_id_expediente  LIKE sep_expediente.id_expediente,
       p_usuario     LIKE seg_usuario.usuario_cod,
       p_parametro   SMALLINT,
       v_nom_archivo LIKE glo_ctr_archivo.nombre_archivo,
       v_consulta    STRING,
       v_ruta_docto  LIKE seg_modulo.ruta_docto,
       v_conteo      INTEGER,
       v_comando     STRING,
       v_error       SMALLINT,
       v_mensaje     STRING

   # Datos prueba
   {LET p_id_expediente = 58
   LET p_usuario       = "safreviv"
   LET p_parametro     = 1
   LET v_nom_archivo   = "22062012_sep_restitucion.csv"}
   LET v_mensaje = "Ocurrió un error al cargar información"
   LET v_error = 0
   
   WHENEVER ERROR CONTINUE
   DISPLAY "0"
   LET v_consulta = "\n DROP TABLE IF EXISTS tmp_sep_restitucion"
   PREPARE prp_elimina_tabla FROM v_consulta
   EXECUTE prp_elimina_tabla
   IF(SQLCA.SQLCODE <> 0)THEN
      LET v_error = -1
      RETURN v_error,v_mensaje
   END IF 

   # Tabla temporal para contener datos del archivo a cargar
   LET v_consulta = "\n CREATE TEMP TABLE tmp_sep_restitucion",
                    "\n (",
                    "\n 	tpo_registro           SMALLINT,",
                    "\n 	folio_procesar         CHAR(40),",
                    "\n 	cve_afore              SMALLINT,",
                    "\n 	invadido               CHAR(11),",
                    "\n 	asociado               CHAR(11),",
                    "\n 	nombre_reclamante    	 CHAR(120),",
                    "\n 	n_caso               	 CHAR(40),",
                    "\n 	sar92_aivs_solicitado  DECIMAL(22,2),",
                    "\n 	sar92_pesos_solicitado DECIMAL(22,2),",
                    "\n 	viv97_aivs_solicitado  DECIMAL(22,2),",
                    "\n 	viv97_pesos_solicitado DECIMAL(22,2),",
                    "\n 	sdo_reclamante_solicitado DECIMAL(22,2),",
                    "\n 	sdo_no_reclamante_solicitado DECIMAL(22,2),",
                    "\n 	sar92_pesos_devolver DECIMAL(22,2),",
                    "\n 	sar92_aivs_devolver  DECIMAL(22,2),",
                    "\n 	viv97_pesos_devolver DECIMAL(22,2),",
                    "\n 	viv97_aivs_devolver  DECIMAL(22,2),",
                    "\n 	subsc_viv97_pesos_devolver  DECIMAL(22,2),",
                    "\n 	subsc_viv97_aivs_devolver  	DECIMAL(22,2),",
                    "\n 	subsc_viv97_pesos_no_aplica DECIMAL(22,2),",
                    "\n 	subsc_viv97_aivs_no_aplica  DECIMAL(22,2),",
                    "\n 	subsc_viv97_pesos_no_aplica_pscd DECIMAL(22,2),",
                    "\n 	subsc_viv97_aivs_no_aplica_pscd  DECIMAL(22,2),",
                    "\n 	amort_devolver        		 DECIMAL(22,2),",
                    "\n 	avance_aportacion_devolver DECIMAL(22,2),",
                    "\n 	avance_amort_devolver DECIMAL(22,2),",
                    "\n 	dif_aivs_sar92        DECIMAL(22,2),",
                    "\n 	dif_aivs_viv97        DECIMAL(22,2),",
                    "\n 	dif_pesos_ret72       DECIMAL(22,2),",
                    "\n 	sdo_total_115        	DECIMAL(22,2),",
                    "\n 	nss_115               CHAR(11),",
                    "\n 	sar92_pesos_115_trabajador DECIMAL(22,2),",
                    "\n 	sar92_aivs_115_trabajador  DECIMAL(22,2),",
                    "\n 	viv97_pesos_115_trabajador DECIMAL(22,2),",
                    "\n 	viv97_aivs_115_trabajador  DECIMAL(22,2),",
                    "\n 	sbusc_viv97_pesos_115_trabajador DECIMAL(22,2),",
                    "\n 	subsc_viv97_aivs_115_trabajador  DECIMAL(22,2),",
                    "\n 	sar92_pesos_115_acreditado DECIMAL(22,2),",
                    "\n 	sar92_aivs_115_acreditado  DECIMAL(22,2),",
                    "\n 	viv97_pesos_115_acreditado DECIMAL(22,2),",
                    "\n 	viv97_aivs_115_acreditado  DECIMAL(22,2),",
                    "\n 	subsc_viv97_pesos_115_acreditado DECIMAL(22,2),",
                    "\n 	subsc_viv97_aivs_115_acreditado  DECIMAL(22,2),",
                    "\n 	observacion CHAR(120)",
                    "\n );"
   PREPARE prp_crea_tbl_tmp FROM v_consulta
   EXECUTE prp_crea_tbl_tmp
   IF(SQLCA.SQLCODE <> 0)THEN
      LET v_error = -1
      RETURN v_error,v_mensaje
   END IF
   
   DATABASE safre_viv
   # Se recupera la ruta en servidor del documento a cargar a tablas
   SELECT ruta_docto
     INTO v_ruta_docto
     FROM seg_modulo
    WHERE modulo_cod = "sep"

   # Separa los registros que cominzan con 2 
   LET v_comando = "sed -e '/^2/!d' '"||
                   v_ruta_docto CLIPPED||v_nom_archivo CLIPPED||
                   "' >"||v_ruta_docto CLIPPED||p_usuario CLIPPED||"."||v_nom_archivo CLIPPED
   DISPLAY v_comando 
   RUN v_comando
   IF(STATUS)THEN # En caso de que no se pueda separar los registros, se termina la ejecucion
      DISPLAY "ERROR AL SEPARAR ARCHIVO PARA CARGA DE INFORMACIÓN"
      LET v_error = -1
      RETURN v_error, v_mensaje
   ELSE
      DISPLAY "ARCHIVO A CARGAR: ",v_ruta_docto CLIPPED,p_usuario CLIPPED,".",v_nom_archivo CLIPPED
      # realiza la carga desde el arhivo a la tabla

      LET v_carga = v_ruta_docto CLIPPED,
                    p_usuario CLIPPED,".",
                    v_nom_archivo CLIPPED

      LET v_carga = v_carga CLIPPED
                  
      LET ch = base.Channel.create()
      CALL ch.setDelimiter("CSV")
      CALL ch.openFile(v_carga,"r")
      WHILE ch.read([v_restitucion.*])
         IF v_restitucion.v_tpo_registro = 2 THEN         
            INSERT INTO tmp_sep_restitucion VALUES (v_restitucion.*)
            IF(SQLCA.SQLCODE <> 0)THEN
               DISPLAY "OCURRIÓ UN ERROR AL CARGAR ARCHIVO DE RESTITUCIÓN"
               DISPLAY "CÓDIGO DE ERROR:",SQLCA.SQLCODE
               DISPLAY "CÓDIGO DE ERROR:",SQLCA.SQLERRM
               CALL ch.close()
               LET    v_error = -1
               RETURN v_error, v_mensaje
            END IF           
         END IF
      END WHILE
       
      CALL ch.close()
       
      IF(STATUS)THEN
         # En caso de error al cargar información, muestra mensajes y termina ejecución
         DISPLAY "OCURRIÓ UN ERROR AL CARGAR ARCHIVO DE RESTITUCIÓN"
         DISPLAY "CÓDIGO DE ERROR:",SQLCA.SQLCODE
         DISPLAY "CÓDIGO DE ERROR:",SQLCA.SQLERRM
         LET v_error = -1
         RETURN v_error, v_mensaje
      ELSE
         LET v_conteo = 0
         SELECT COUNT(*)
           INTO v_conteo
           FROM tmp_sep_restitucion
          WHERE 1 = 1

         LET v_comando = "rm ",v_ruta_docto CLIPPED,p_usuario CLIPPED,".",v_nom_archivo CLIPPED
         DISPLAY v_comando 
         RUN v_comando

         DISPLAY "TOTAL DE REGISTROS CARGADOS: ",v_conteo

         # Validación de registros del archivo
         CASE 

           WHEN v_conteo = 0 # no contiene registros
              --LET v_mensaje = "Carga no realizada, archivo de restitución no contiene registros"
              LET v_error = -1
              # Continua ejecución ya que para la restitución sin archivo sólo actualiza la maquinaria
              --RETURN v_error,v_mensaje

           WHEN v_conteo > 1 # tiene más de un registro
              LET v_mensaje = "Carga no realizada, archivo de restitución contiene más de un registro"
              LET v_error = -1
              RETURN v_error,v_mensaje

         END CASE
      END IF
   END IF

   # 0 Restitución aplicadas
   # 1 Restitución no aplicadas
   # 2 Restitución complementaria 
   CASE p_parametro

     WHEN 0
        CALL fn_carga_restitucion_aplicadas(p_id_expediente,
                                            p_usuario,
                                            TRUE) # Ejecutar maquinaria 
                                             RETURNING v_error

     WHEN 1
        CALL fn_carga_restitucion_no_aplicadas(p_id_expediente) RETURNING v_error

     WHEN 2
        CALL fn_carga_restitucion_aplicadas(p_id_expediente,
                                            p_usuario,
                                            FALSE) # cargar sin ejecutar maquinaria
                                            RETURNING v_error

   END CASE
   
   RETURN v_error,
          v_mensaje
END FUNCTION
--END MAIN 

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPE26                                                   #
#Objetivo          => carga datos de restitucion aplicadas                     #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => Junio 22, 2012                                           #
################################################################################
FUNCTION fn_carga_restitucion_aplicadas(p_id_expediente,
                                        p_usuario,
                                        p_avanza_maquinaria)
DEFINE p_id_expediente       LIKE sep_expediente.id_expediente,
       p_usuario             LIKE seg_usuario.usuario_cod,
       p_avanza_maquinaria   BOOLEAN,
       v_consulta            STRING,
       v_id_restitucion      LIKE sep_restitucion.id_restitucion,
       v_senial              SMALLINT,
       v_estado_destino      SMALLINT,
       v_ind                 SMALLINT,
       v_diag                CHAR(3),
       v_error               SMALLINT,
       v_ind_restitucion_apl SMALLINT

   
   LET v_error = 0

   IF(p_avanza_maquinaria)THEN
      LET v_ind_restitucion_apl = 1 # RESTITUCION SOLICITADA
   ELSE
      # 0 para poder manupular los registros como restitución complementaria sin confirmar y poder visualizarla en la solicitud de 
      # restitución complementaria pero  no en la solicitud de restitución normal
      LET v_ind_restitucion_apl = 0 # SIN RESTITUCION
   END IF

   WHENEVER ERROR CONTINUE

   LET v_consulta = "\n INSERT INTO sep_restitucion",
                    "\n (id_restitucion,",
                    "\n  id_expediente,",
                    "\n  folio_procesar,",
                    "\n  cve_afore,",
                    "\n  invadido,",
                    "\n  asociado,",
                    "\n  nombre_reclamante,",
                    "\n  n_caso,",
                    "\n  sar92_aivs_solicitado,",
                    "\n  sar92_pesos_solicitado,",
                    "\n  viv97_aivs_solicitado,",
                    "\n  viv97_pesos_solicitado,",
                    "\n  sdo_reclamante_solicitado,",
                    "\n  sdo_no_reclamante_solicitado,",
                    "\n  observacion)",
                    "\n VALUES(seq_sep_restitucion.NEXTVAL,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
   PREPARE prp_ins_restitucion FROM v_consulta
   IF(SQLCA.SQLCODE <> 0)THEN
      LET v_error = -1
      DISPLAY "ERROR SQL:",SQLCA.SQLCODE
   END IF

   LET v_consulta = "\n SELECT FIRST 1 seq_sep_restitucion.CURRVAL",
                    "\n   FROM sep_restitucion",
                    "\n  WHERE 1=1"
   PREPARE prp_rec_id_restitucion FROM v_consulta
   IF(SQLCA.SQLCODE <> 0)THEN
      LET v_error = -1
      DISPLAY "ERROR SQL:",SQLCA.SQLCODE
   END IF

   LET v_consulta = "\n INSERT INTO sep_mto_restitucion_analisis",
                    "\n (id_mto_restitucion,",
                    "\n  id_restitucion,",
                    "\n  id_expediente,",
                    "\n  sar92_pesos_devolver,",
                    "\n  sar92_aivs_devolver,",
                    "\n  viv97_pesos_devolver,",
                    "\n  viv97_aivs_devolver,",
                    "\n  subsc_viv97_pesos_devolver,",
                    "\n  subsc_viv97_aivs_devolver,",
                    "\n  ind_restitucion) ",
                    "\n VALUES(seq_sep_mto_restitucion_analisis.NEXTVAL,?,?,?,?,?,?,?,?,?)"
   PREPARE prp_ins_mto_analisis FROM v_consulta

   IF(SQLCA.SQLCODE <> 0)THEN
      LET v_error = -1
      DISPLAY "ERROR SQL:",SQLCA.SQLCODE
   END IF
   LET v_consulta = "\n INSERT INTO sep_inf_restitucion",
                    "\n (id_inf_restitucion,",
                    "\n  id_restitucion,",
                    "\n  id_expediente,",
                    "\n  amort_devolver,",
                    "\n  avance_aportacion_devolver,",
                    "\n  avance_amort_devolver,",
                    "\n  dif_aivs_sar92,",
                    "\n  dif_aivs_viv97,",
                    "\n  dif_pesos_ret72)",
                    "\n VALUES(seq_sep_inf_restitucion.NEXTVAL,?,?,?,?,?,?,?,?)"
   PREPARE prp_info_restitucion FROM v_consulta

   IF(SQLCA.SQLCODE <> 0)THEN
      LET v_error = -1
      DISPLAY "ERROR SQL:",SQLCA.SQLCODE
   END IF

   LET v_consulta = "\n INSERT INTO sep_115_restitucion",
                    "\n (id_115_restitucion,",
                    "\n  id_restitucion,",
                    "\n  id_expediente,",
                    "\n  sdo_total_115,",
                    "\n  nss_115,",
                    "\n  sar92_pesos_115_trabajador,",
                    "\n  sar92_aivs_115_trabajador,",
                    "\n  viv97_pesos_115_trabajador,",
                    "\n  viv97_aivs_115_trabajador,",
                    "\n  subsc_viv97_pesos_115_trabajador,",
                    "\n  subsc_viv97_aivs_115_trabajador,",
                    "\n  sar92_pesos_115_acreditado,",
                    "\n  sar92_aivs_115_acreditado,",
                    "\n  viv97_pesos_115_acreditado,",
                    "\n  viv97_aivs_115_acreditado,",
                    "\n  subsc_viv97_pesos_115_acreditado,",
                    "\n  subsc_viv97_aivs_115_acreditado,",
                    "\n  ind_restitucion)  ",
                    "\n VALUES(seq_sep_115_restitucion.NEXTVAL,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
   PREPARE prp_rec_115_restitucion FROM v_consulta

   IF(SQLCA.SQLCODE <> 0)THEN
      LET v_error = -1
      DISPLAY "ERROR SQL:",SQLCA.SQLCODE
   END IF

   LET v_consulta = "\n SELECT tpo_registro,",
	                "\n folio_procesar,",
	                "\n cve_afore,",
	                "\n invadido,",
	                "\n asociado,",
	                "\n nombre_reclamante,",
	                "\n n_caso,",
	                "\n sar92_aivs_solicitado,",
	                "\n sar92_pesos_solicitado,",
	                "\n viv97_aivs_solicitado,",
	                "\n viv97_pesos_solicitado,",
	                "\n sdo_reclamante_solicitado,",
	                "\n sdo_no_reclamante_solicitado,",
	                "\n sar92_pesos_devolver,",
	                "\n sar92_aivs_devolver,",
	                "\n viv97_pesos_devolver,",
	                "\n viv97_aivs_devolver,",
	                "\n subsc_viv97_pesos_devolver,",
	                "\n subsc_viv97_aivs_devolver,",
	                "\n subsc_viv97_pesos_no_aplica,",
	                "\n subsc_viv97_aivs_no_aplica,",
	                "\n subsc_viv97_pesos_no_aplica_pscd,",
	                "\n subsc_viv97_aivs_no_aplica_pscd,",
	                "\n amort_devolver,",
	                "\n avance_aportacion_devolver,",
	                "\n avance_amort_devolver,",
	                "\n dif_aivs_sar92,",
	                "\n dif_aivs_viv97,",
	                "\n dif_pesos_ret72,",
	                "\n sdo_total_115,",
	                "\n nss_115,",
	                "\n sar92_pesos_115_trabajador,",
	                "\n sar92_aivs_115_trabajador,",
	                "\n viv97_pesos_115_trabajador,",
	                "\n viv97_aivs_115_trabajador,",
	                "\n sbusc_viv97_pesos_115_trabajador,",
	                "\n subsc_viv97_aivs_115_trabajador,",
	                "\n sar92_pesos_115_acreditado,",
	                "\n sar92_aivs_115_acreditado,",
	                "\n viv97_pesos_115_acreditado,",
	                "\n viv97_aivs_115_acreditado,",
	                "\n subsc_viv97_pesos_115_acreditado,",
	                "\n subsc_viv97_aivs_115_acreditado,",
	                "\n observacion",
                    "\n FROM tmp_sep_restitucion",
                    "\n WHERE 1 = 1"
   PREPARE prp_rec_restitucion FROM v_consulta
   DECLARE cur_rec_restitucion CURSOR FOR prp_rec_restitucion

   FOREACH cur_rec_restitucion INTO v_restitucion.*
      EXECUTE prp_ins_restitucion USING p_id_expediente,
                                        v_restitucion.v_folio_procesar,
                                        v_restitucion.v_cve_afore,
                                        v_restitucion.v_invadido,
                                        v_restitucion.v_asociado,
                                        v_restitucion.v_nombre_reclamante,
                                        v_restitucion.v_n_caso,
                                        v_restitucion.v_sar92_aivs_solicitado,
                                        v_restitucion.v_sar92_pesos_solicitado,
                                        v_restitucion.v_viv97_aivs_solicitado,
                                        v_restitucion.v_viv97_pesos_solicitado,
                                        v_restitucion.v_sdo_reclamante_solicitado,
                                        v_restitucion.v_sdo_no_reclamante_solicitado,
                                        v_restitucion.v_observacion
      IF(SQLCA.SQLCODE <> 0)THEN
         LET v_error = -1
         DISPLAY "ERROR SQL:",SQLCA.SQLCODE
      END IF

      EXECUTE prp_rec_id_restitucion INTO v_id_restitucion

      IF(SQLCA.SQLCODE <> 0)THEN
         LET v_error = -1
         DISPLAY "ERROR SQL:",SQLCA.SQLCODE
      END IF

      IF(v_restitucion.v_sar92_pesos_devolver <> 0 OR
         v_restitucion.v_sar92_aivs_devolver  <> 0 OR
         v_restitucion.v_viv97_pesos_devolver <> 0 OR
         v_restitucion.v_viv97_aivs_devolver <> 0 OR
         v_restitucion.v_subsc_viv97_pesos_devolver <> 0 OR 
         v_restitucion.v_subsc_viv97_aivs_devolver <> 0)THEN

         EXECUTE prp_ins_mto_analisis USING v_id_restitucion,
                                            p_id_expediente,
                                            v_restitucion.v_sar92_pesos_devolver,
                                            v_restitucion.v_sar92_aivs_devolver,
                                            v_restitucion.v_viv97_pesos_devolver,
                                            v_restitucion.v_viv97_aivs_devolver,
                                            v_restitucion.v_subsc_viv97_pesos_devolver,
                                            v_restitucion.v_subsc_viv97_aivs_devolver, 
                                            v_ind_restitucion_apl 
         IF(SQLCA.SQLCODE <> 0)THEN
            LET v_error = -1
            DISPLAY "ERROR SQL:",SQLCA.SQLCODE
         END IF
      END IF
      IF(v_restitucion.v_amort_devolver <> 0 OR
         v_restitucion.v_avance_aportacion_devolver <> 0 OR
         v_restitucion.v_avance_amort_devolver <> 0 OR
         v_restitucion.v_dif_aivs_sar92 <> 0 OR
         v_restitucion.v_dif_aivs_viv97 <> 0 OR
         v_restitucion.v_dif_pesos_ret72 <> 0)THEN

         EXECUTE prp_info_restitucion USING v_id_restitucion,
                                            p_id_expediente,
                                            v_restitucion.v_amort_devolver,
                                            v_restitucion.v_avance_aportacion_devolver,
                                            v_restitucion.v_avance_amort_devolver,
                                            v_restitucion.v_dif_aivs_sar92,
                                            v_restitucion.v_dif_aivs_viv97,
                                            v_restitucion.v_dif_pesos_ret72
         IF(SQLCA.SQLCODE <> 0)THEN
            LET v_error = -1
            DISPLAY "ERROR SQL:",SQLCA.SQLCODE
         END IF
      END IF

      IF(v_restitucion.v_sar92_pesos_115_trabajador <> 0 OR
         v_restitucion.v_sar92_aivs_115_trabajador <> 0 OR
         v_restitucion.v_viv97_pesos_115_trabajador <> 0 OR
         v_restitucion.v_viv97_aivs_115_trabajador <> 0 OR
         v_restitucion.v_sbusc_viv97_pesos_115_trabajador <> 0 OR
         v_restitucion.v_subsc_viv97_aivs_115_trabajador <> 0 OR
         v_restitucion.v_sar92_pesos_115_acreditado <> 0 OR
         v_restitucion.v_sar92_aivs_115_acreditado <> 0 OR
         v_restitucion.v_viv97_pesos_115_acreditado <> 0 OR
         v_restitucion.v_viv97_aivs_115_acreditado <> 0 OR
         v_restitucion.v_subsc_viv97_pesos_115_acreditado <> 0 OR
         v_restitucion.v_subsc_viv97_aivs_115_acreditado <> 0)THEN

         EXECUTE prp_rec_115_restitucion USING v_id_restitucion,
                                               p_id_expediente,
                                               v_restitucion.v_sdo_total_115,
                                               v_restitucion.v_nss_115,
                                               v_restitucion.v_sar92_pesos_115_trabajador,
                                               v_restitucion.v_sar92_aivs_115_trabajador,
                                               v_restitucion.v_viv97_pesos_115_trabajador,
                                               v_restitucion.v_viv97_aivs_115_trabajador,
                                               v_restitucion.v_sbusc_viv97_pesos_115_trabajador,
                                               v_restitucion.v_subsc_viv97_aivs_115_trabajador,
                                               v_restitucion.v_sar92_pesos_115_acreditado,
                                               v_restitucion.v_sar92_aivs_115_acreditado,
                                               v_restitucion.v_viv97_pesos_115_acreditado,
                                               v_restitucion.v_viv97_aivs_115_acreditado,
                                               v_restitucion.v_subsc_viv97_pesos_115_acreditado,
                                               v_restitucion.v_subsc_viv97_aivs_115_acreditado,
                                               v_ind_restitucion_apl 
         IF(SQLCA.SQLCODE <> 0)THEN
            LET v_error = -1
            DISPLAY "ERROR SQL:",SQLCA.SQLCODE
         END IF
      END IF
   END FOREACH
   FREE cur_rec_restitucion

   # Avanza maquinaria sólo para restitución normal
   IF(p_avanza_maquinaria)THEN
      LET v_senial = 55
      LET v_consulta = " EXECUTE FUNCTION fn_maquinaria_individual(?,?,?,?,?)"
      PREPARE prp_actualiza_maquinaria FROM v_consulta
      EXECUTE prp_actualiza_maquinaria USING "maq_sep_expediente",
                                             p_id_expediente,
                                             "id_expediente",
                                             v_senial,
                                             p_usuario
                                        INTO v_ind, 
                                             v_diag, 
                                             v_estado_destino

      IF(v_ind = -1 OR v_estado_destino = -1)THEN
         DISPLAY "REGISTRO IMPOSIBLE DE ACTUALIZAR ESTADO"
         LET v_error = -1
      END IF
   END IF

   RETURN v_error
                    
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPE26                                                   #
#Objetivo          => carga datos de restitucion no aplicadas                  #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => Junio 22, 2012                                           #
################################################################################
FUNCTION fn_carga_restitucion_no_aplicadas(p_id_expediente)
DEFINE p_id_expediente          LIKE sep_expediente.id_expediente,
       v_consulta               STRING,
       v_id_restitucion         LIKE sep_restitucion.id_restitucion,
       v_folio_no_aplicados     LIKE sep_mto_restitucion_no_aplicados.folio_no_aplicados,
       v_ind_restitucion_no_apl LIKE sep_mto_restitucion_no_aplicados.ind_restitucion,
       v_error                  SMALLINT


   WHENEVER ERROR CONTINUE
   LET v_error = 0

   # siempre es cero
   LET v_folio_no_aplicados = 0
   LET v_ind_restitucion_no_apl = 1 # Restitucion Solicitada

   LET v_consulta = "\n INSERT INTO sep_mto_restitucion_no_aplicados",
                    "\n (id_mto_restitucion_no_aplicados,",
                    "\n  id_restitucion,",
                    "\n  id_expediente,",
                    "\n  folio_no_aplicados,",
                    "\n  subsc_viv97_pesos_no_aplica,",
                    "\n  subsc_viv97_aivs_no_aplica,",
                    "\n  subsc_viv97_pesos_no_aplica_pscd,",
                    "\n  subsc_viv97_aivs_no_aplica_pscd,",
                    "\n  ind_restitucion)",
                    "\n VALUES(seq_sep_mto_restitucion_no_aplicados.NEXTVAL,?,?,?,?,?,?,?,?)"
                    
   PREPARE prp_ins_no_aplicados FROM v_consulta
   IF(SQLCA.SQLCODE <> 0)THEN
      LET v_error = -1
      DISPLAY "ERROR SQL:",SQLCA.SQLCODE
      DISPLAY "ERROR EN PREPARE INSERT INTO sep_mto_restitucion_no_aplicados"
   END IF

   SELECT id_restitucion
     INTO v_id_restitucion
     FROM sep_restitucion
    WHERE id_expediente = p_id_expediente

   LET v_consulta = "\n SELECT tpo_registro,",
	                "\n folio_procesar,",
	                "\n cve_afore,",
	                "\n invadido,",
	                "\n asociado,",
	                "\n nombre_reclamante,",
	                "\n n_caso,",
	                "\n sar92_aivs_solicitado,",
	                "\n sar92_pesos_solicitado,",
	                "\n viv97_aivs_solicitado,",
	                "\n viv97_pesos_solicitado,",
	                "\n sdo_reclamante_solicitado,",
	                "\n sdo_no_reclamante_solicitado,",
	                "\n sar92_pesos_devolver,",
	                "\n sar92_aivs_devolver,",
	                "\n viv97_pesos_devolver,",
	                "\n viv97_aivs_devolver,",
	                "\n subsc_viv97_pesos_devolver,",
	                "\n subsc_viv97_aivs_devolver,",
	                "\n subsc_viv97_pesos_no_aplica,",
	                "\n subsc_viv97_aivs_no_aplica,",
	                "\n subsc_viv97_pesos_no_aplica_pscd,",
	                "\n subsc_viv97_aivs_no_aplica_pscd,",
	                "\n amort_devolver,",
	                "\n avance_aportacion_devolver,",
	                "\n avance_amort_devolver,",
	                "\n dif_aivs_sar92,",
	                "\n dif_aivs_viv97,",
	                "\n dif_pesos_ret72,",
	                "\n sdo_total_115,",
	                "\n nss_115,",
	                "\n sar92_pesos_115_trabajador,",
	                "\n sar92_aivs_115_trabajador,",
	                "\n viv97_pesos_115_trabajador,",
	                "\n viv97_aivs_115_trabajador,",
	                "\n sbusc_viv97_pesos_115_trabajador,",
	                "\n subsc_viv97_aivs_115_trabajador,",
	                "\n sar92_pesos_115_acreditado,",
	                "\n sar92_aivs_115_acreditado,",
	                "\n viv97_pesos_115_acreditado,",
	                "\n viv97_aivs_115_acreditado,",
	                "\n subsc_viv97_pesos_115_acreditado,",
	                "\n subsc_viv97_aivs_115_acreditado,",
	                "\n observacion",
                    "\n FROM tmp_sep_restitucion",
                    "\n WHERE 1 = 1"
                    
   PREPARE prp_rec_restitucion_no_apl FROM v_consulta
   DECLARE cur_rec_restitucion_no_apl CURSOR FOR prp_rec_restitucion_no_apl
   FOREACH cur_rec_restitucion_no_apl INTO v_restitucion.*

      IF(v_restitucion.v_subsc_viv97_pesos_no_aplica      > 0 OR
         v_restitucion.v_subsc_viv97_aivs_no_aplica       > 0 OR
         v_restitucion.v_subsc_viv97_pesos_no_aplica_pscd > 0 OR
         v_restitucion.v_subsc_viv97_aivs_no_aplica_pscd  > 0)THEN
         EXECUTE prp_ins_no_aplicados USING v_id_restitucion,
                                            p_id_expediente,
                                            v_folio_no_aplicados,
                                            v_restitucion.v_subsc_viv97_pesos_no_aplica,
                                            v_restitucion.v_subsc_viv97_aivs_no_aplica,
                                            v_restitucion.v_subsc_viv97_pesos_no_aplica_pscd,
                                            v_restitucion.v_subsc_viv97_aivs_no_aplica_pscd,
                                            v_ind_restitucion_no_apl
         IF(SQLCA.SQLCODE <> 0)THEN
            LET v_error = -1
            DISPLAY "ERROR EN INSERT INTO sep_mto_restitucion_no_aplicados"
         END IF
      END IF                                         

   END FOREACH
   IF(SQLCA.SQLCODE <> 0)THEN
      LET v_error = -1
      DISPLAY "ERROR SQL:",SQLCA.SQLCODE
      DISPLAY "ERROR EN SELECT tmp_sep_restitucion"
   END IF   
   FREE cur_rec_restitucion_no_apl
   
   RETURN v_error
END FUNCTION