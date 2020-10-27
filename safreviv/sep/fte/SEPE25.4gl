--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================
################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPE25                                                   #
#Descripcion       => Batch para carga de baja de mptoficacion                 #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => Junio 23, 2012                                           #
################################################################################

DATABASE safre_viv

DEFINE v_baja_notififcacion RECORD
         v_tpo_registro      SMALLINT,
         v_trabajador        CHAR(11),
         v_nombre_trabajador CHAR(120),
         v_num_caso    		 CHAR(40),
         v_acreditado 		 CHAR(40),
         v_nombre_acreditado CHAR(120),
         v_num_credito 		 DECIMAL(10,0),
         v_f_envio_dm 		 CHAR(10),
         v_f_confirmacion 	 CHAR(10)
       END RECORD  

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPE25                                                   #
#Objetivo          => carga datos de baja de notificaicon      #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => Junio 23, 2012                                           #
################################################################################
--MAIN
FUNCTION fn_carga_baja_notificacion(p_id_expediente,p_usuario,v_nom_archivo)
DEFINE p_id_expediente  LIKE sep_expediente.id_expediente,
       p_usuario       LIKE seg_usuario.usuario_cod,
       v_nom_archivo   LIKE glo_ctr_archivo.nombre_archivo,
       v_consulta      STRING,
       v_ruta_docto    LIKE seg_modulo.ruta_docto,
       v_conteo        INTEGER,
       v_comando       STRING,
       v_error         SMALLINT
DEFINE v_archivo_carga VARCHAR(100),
       v_canal         base.Channel

   # Datos prueba
   {LET p_id_expediente = 58
   LET p_usuario       = "safreviv"
   LET p_parametro     = 1
   LET v_nom_archivo   = "22062012_sep_restitucion.csv"}
   LET v_error = 0
   WHENEVER ERROR CONTINUE

   DATABASE safre_tmp
   # Tabla temporal para contener datos del archivo a cargar
   LET v_consulta = "\n CREATE TEMP TABLE safre_tmp:tmp_sep_aviso_desvinculacion(",
                    "\n    tpo_registro      SMALLINT,",
                    "\n    trabajador        char(11),",
                    "\n    nombre_trabajador char(120),",
                    "\n    num_caso          char(40),",
                    "\n    acreditado        char(40),",
                    "\n    nombre_acreditado char(120),",
                    "\n    num_credito       decimal(10,0),",
                    "\n    f_envio_dm        char(10),",
                    "\n    f_confirmacion    char(10)",
                    "\n );"
   PREPARE prp_crea_tbl_tmp FROM v_consulta
   EXECUTE prp_crea_tbl_tmp
   IF(SQLCA.SQLCODE <> 0)THEN
      LET v_error = -1
      RETURN v_error 
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
   IF(STATUS)THEN
      # En caso de que no se pueda separar los registros, se termina la ejecucion
      DISPLAY "ERROR AL SEPARAR ARCHIVO PARA CARGA DE INFORMACIÓN"
      LET v_error = -1
      RETURN v_error 
      --EXIT PROGRAM
   ELSE
      DISPLAY "ARCHIVO A CARGAR: ",v_ruta_docto CLIPPED,p_usuario CLIPPED,".",v_nom_archivo CLIPPED
      # realiza la carga desde el arhivo a la tabla
      --LOAD FROM v_ruta_docto CLIPPED||p_usuario CLIPPED||"."||v_nom_archivo CLIPPED DELIMITER ","
      --INSERT INTO safre_tmp:tmp_sep_aviso_desvinculacion

      LET v_archivo_carga = v_ruta_docto CLIPPED||
                            p_usuario CLIPPED||"."||
                            v_nom_archivo CLIPPED

      LET v_archivo_carga = v_archivo_carga CLIPPED

      LET v_canal = base.Channel.create()
      CALL v_canal.setDelimiter("CSV")
      CALL v_canal.openFile(v_archivo_carga,"r")

      WHILE v_canal.read([v_baja_notififcacion.*])
         IF(v_baja_notififcacion.v_tpo_registro = 2)THEN
            INSERT INTO safre_tmp:tmp_sep_aviso_desvinculacion
                        (tpo_registro,
                         trabajador,
                         nombre_trabajador,
                         num_caso,
                         acreditado,
                         nombre_acreditado,
                         num_credito,
                         f_envio_dm,
                         f_confirmacion)
            VALUES (v_baja_notififcacion.*)
            
            IF(SQLCA.SQLCODE <> 0)THEN
               CALL v_canal.close()
               # En caso de error al cargar información, muestra mensajes y termina ejecución
               --CALL fn_mensaje("","OCURRIÓ UN ERROR AL CARGAR ARCHIVO DE RESTITUCIÓN, CÓDIGO: "||SQLCA.SQLCODE,"stop")
               DISPLAY "OCURRIÓ UN ERROR AL CARGAR ARCHIVO"
               DISPLAY "CÓDIGO DE ERROR:",SQLCA.SQLCODE
               DISPLAY "CÓDIGO DE ERROR:",SQLCA.SQLERRM
               LET v_error = -1
               RETURN v_error 
               --EXIT PROGRAM
            END IF
         END IF
      END WHILE
      CALL v_canal.close()
      IF(STATUS)THEN
         # En caso de error al cargar información, muestra mensajes y termina ejecución
         --CALL fn_mensaje("","OCURRIÓ UN ERROR AL CARGAR ARCHIVO DE RESTITUCIÓN, CÓDIGO: "||SQLCA.SQLCODE,"stop")
         DISPLAY "OCURRIÓ UN ERROR AL CARGAR ARCHIVO"
         DISPLAY "CÓDIGO DE ERROR:",SQLCA.SQLCODE
         DISPLAY "CÓDIGO DE ERROR:",SQLCA.SQLERRM
         LET v_error = -1
         RETURN v_error 
         --EXIT PROGRAM
      ELSE
         SELECT COUNT(*)
           INTO v_conteo
           FROM safre_tmp:tmp_sep_aviso_desvinculacion
          WHERE 1 = 1

         LET v_comando = "rm ",v_ruta_docto CLIPPED,p_usuario CLIPPED,".",v_nom_archivo CLIPPED
         DISPLAY v_comando 
         RUN v_comando

         DISPLAY "TOTAL DE REGISTROS CARGADOS: ",v_conteo
      END IF
   END IF
   
   IF(v_error = 0)THEN
      CALL fn_carga_tabla(p_id_expediente,p_usuario)
            RETURNING v_error
   END IF
   
   RETURN v_error 
END FUNCTION
--END MAIN 

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPE25                                                   #
#Objetivo          => carga datos de restitucion aplicadas                     #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => Junio 22, 2012                                           #
################################################################################
FUNCTION fn_carga_tabla(p_id_expediente,p_usuario)
DEFINE p_id_expediente  LIKE sep_expediente.id_expediente,
       p_usuario        LIKE seg_usuario.usuario_cod,
       v_consulta       STRING,
       v_id_restitucion LIKE sep_restitucion.id_restitucion,
       v_senial         SMALLINT,
       v_estado_destino SMALLINT,
       v_ind            SMALLINT,
       v_diag           CHAR(3),
       v_error          SMALLINT

   WHENEVER ERROR CONTINUE
   LET v_error = 0

   DISPLAY "1"

   LET v_consulta = "\n INSERT INTO sep_aviso_desvinculacion (",
                    "\n  id_expediente,",
                    --"\n  folio,",
                    "\n  trabajador,",
                    "\n  nombre_trabajador,",
                    "\n  num_caso,",
                    "\n  acreditado,",
                    "\n  nombre_acreditado,",
                    "\n  num_credito,",
                    "\n  f_envio_dm,",
                    "\n  f_confirmacion)",
                    "\n VALUES(?,?,?,?,?,?,?,?,?)"
   PREPARE prp_ins_baja_not FROM v_consulta
   DISPLAY SQLCA.SQLCODE
   IF(SQLCA.SQLCODE <> 0)THEN
      LET v_error = -1
   END IF
   DISPLAY "2"

   LET v_consulta = "\n SELECT trabajador,",
                    "\n        nombre_trabajador,",
                    "\n        num_caso,",
                    "\n        acreditado,",
                    "\n        nombre_acreditado,",
                    "\n        num_credito,",
                    "\n        f_envio_dm,",
                    "\n        f_confirmacion",
                    "\n   FROM safre_tmp:tmp_sep_aviso_desvinculacion",
                    "\n  WHERE 1 = 1"
   PREPARE prp_rec_baja_not FROM v_consulta
   DECLARE cur_rec_baja_not CURSOR FOR prp_rec_baja_not
   --LET v_indice = 1
   FOREACH cur_rec_baja_not INTO v_baja_notififcacion.*
      --DISPLAY v_restitucion.*
      DISPLAY "2"
      EXECUTE prp_ins_baja_not USING p_id_expediente,
                                     v_baja_notififcacion.v_trabajador,
                                     v_baja_notififcacion.v_nombre_trabajador,
                                     v_baja_notififcacion.v_num_caso,
                                     v_baja_notififcacion.v_acreditado,
                                     v_baja_notififcacion.v_nombre_acreditado,
                                     v_baja_notififcacion.v_num_credito,
                                     v_baja_notififcacion.v_f_envio_dm,
                                     v_baja_notififcacion.v_f_confirmacion
                                     
      DISPLAY SQLCA.SQLCODE
      IF(SQLCA.SQLCODE <> 0)THEN
         LET v_error = -1
      END IF
   END FOREACH   
   FREE cur_rec_baja_not

   IF(v_error = 0)THEN
      # se actualiza el indicador para el archivo
      UPDATE sep_expediente
         SET ind_baja_notificacion = 2 # Archivo Confirmado
       WHERE id_expediente = p_id_expediente
      DISPLAY SQLCA.SQLCODE
      IF(SQLCA.SQLCODE <> 0)THEN
         LET v_error = -1
      END IF
   END IF

   RETURN v_error
END FUNCTION
