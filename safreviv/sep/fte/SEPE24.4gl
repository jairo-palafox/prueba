--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 18-04-2012
--===============================================================

####################################################################
#Modulo            =>SEPE                                          #
#Programa          =>SEPE24                                        #
#Objetivo          =>Programa de integracion de archivos contabilid# 
#Autor             =>Alexandro Hollmann, EFP                       #
#Fecha inicio      =>14 Junio   2012                               #
####################################################################
DATABASE safre_viv

FUNCTION fn_carga_credito(p_v_usuario,   p_v_archivo,   p_v_expediente)
   DEFINE v_ch_arch            BASE.CHANNEL, -- manejador de archivo
          --v_c_ruta_res_sep     LIKE seg_modulo.ruta_rescate, -- ruta donde se ubica el archivo
          v_s_qryTxt           STRING, -- guarda una sentencia sql a ejecutar
          r_b_valida           SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   --DEFINE r_ruta_bin        LIKE seg_modulo.ruta_bin -- rutal de bin
   DEFINE v_estado          CHAR(1)
   DEFINE p_v_usuario       LIKE seg_usuario.usuario, -- nombre del usuario
          p_v_archivo       VARCHAR(120),--LIKE glo_ctr_archivo.nombre_archivo,
          p_v_expediente    DECIMAL(9,0)
          
   DEFINE v_count_valida    INTEGER

   DEFINE v_r_rpt_res_edo RECORD
             f_reporte       CHAR(10),
             nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo,
             folio           LIKE glo_ctr_archivo.folio,
             tot_integrado   INTEGER,
             tot_marcadas    INTEGER,
             tot_rechazadas  INTEGER,
             tot_infonavit   INTEGER
          END RECORD  

   DEFINE r_tmp_sep_arch   RECORD
             tpo_registro         CHAR(2),--LIKE safre_tmp:tmp_sep_batch_contabilidad.tpo_registro  ,
             cv_reg               CHAR(5),
             cv_cia               CHAR(5),
             cv_mon               CHAR(5),
             cv_delega            CHAR(5),
             cv_ctrl4             CHAR(5),
             id_credito           DECIMAL(10,0),--LIKE safre_tmp:tmp_sep_batch_contabilidad.id_credito    ,
             nss                  CHAR(11),--LIKE safre_tmp:tmp_sep_batch_contabilidad.nss           ,
             bimestre             CHAR(6),--LIKE safre_tmp:tmp_sep_batch_contabilidad.bimestre      ,
             f_envio              CHAR(8),--LIKE safre_tmp:tmp_sep_batch_contabilidad.f_envio       ,
             clave                SMALLINT,--LIKE safre_tmp:tmp_sep_batch_contabilidad.clave         ,
             nrp                  CHAR(10),--LIKE safre_tmp:tmp_sep_batch_contabilidad.nrp           ,
             cv_marca             CHAR(5),
             id_transaccion       INTEGER,--LIKE safre_tmp:tmp_sep_batch_contabilidad.id_transaccion,
             tipo                 SMALLINT,--LIKE safre_tmp:tmp_sep_batch_contabilidad.tipo          ,
             cv_apl_capital       CHAR(10),
             cv_folio_refer       CHAR(5),
             im_aport             DECIMAL(9,0),
             mto_txt              DECIMAL(9,0),--LIKE safre_tmp:tmp_sep_batch_contabilidad.mto_txt       ,
             detalle              VARCHAR(120),--LIKE safre_tmp:tmp_sep_batch_contabilidad.detalle       ,
             no_uti01             VARCHAR(100),
             no_uti02             VARCHAR(100),
             no_uti03             VARCHAR(100),
             no_uti04             VARCHAR(100),
             no_uti05             VARCHAR(100),
             no_uti06             VARCHAR(100),
             no_uti07             VARCHAR(100),
             no_uti08             VARCHAR(100),
             no_uti09             VARCHAR(100),
             no_uti10             VARCHAR(100),
             no_uti11             VARCHAR(100),
             no_uti12             VARCHAR(100),
             no_uti13             VARCHAR(100)
          END RECORD
   
   DEFINE v_pos              INTEGER
   DEFINE r_bandera          SMALLINT   
   --DEFINE v_detalle          LIKE sep_batch_contabilidad.detalle
   DEFINE v_ruta_docto       LIKE seg_modulo.ruta_docto

   DEFINE v_estado_destino   SMALLINT
   DEFINE v_ind              SMALLINT
   DEFINE v_diag             CHAR(3)
   DEFINE v_error SMALLINT
   DEFINE v_comando STRING

   WHENEVER ERROR STOP

   DISPLAY "ARCHIVO A PROCESAR:",p_v_archivo
   DISPLAY "EXPEDIENTE A PROCESAR:",p_v_expediente

   LET v_error = 0
   -- Rutina que define el estado final despues de cualquier accion
   LET v_s_qryTxt = "EXECUTE FUNCTION safre_viv:fn_maquinaria(?,?,?)"
   # Se prepara la ejecucion del stored procedure para la maquinaria de estados
   PREPARE prp_maq_edo_sep FROM v_s_qryTxt
   
   SELECT TRIM(ruta_docto)||"/" INTO v_ruta_docto
     FROM seg_modulo
    WHERE modulo_cod = 'sep'

   # Separa los registros que cominzan con 2 
   LET v_comando = "sed -e '/^2/!d' '"||
                   v_ruta_docto CLIPPED||p_v_archivo CLIPPED||
                   "' >"||v_ruta_docto CLIPPED||p_v_usuario CLIPPED||"."||p_v_archivo CLIPPED
   DISPLAY v_comando 
   RUN v_comando
   DISPLAY "ESTADO SEPARACION:",STATUS

   LET v_ch_arch = BASE.CHANNEL.create()
   CALL v_ch_arch.setDelimiter(",")
   
   --DELETE FROM safre_tmp:tmp_sep_batch_contabilidad
   LET v_s_qryTxt = "\n CREATE TEMP TABLE tmp_sep_batch_contabilidad(",
                    "\n tpo_registro         char(2),",
                    "\n nss                  char(11),",
                    "\n id_credito           decimal(10,0),",
                    "\n bimestre             char(6),",
                    "\n f_envio              char(8),",
                    "\n clave                smallint,",
                    "\n nrp                  char(10),",
                    "\n mto_decimal          decimal(12,2),",
                    "\n mto_txt              decimal(9,0),",
                    "\n id_transaccion       integer,",
                    "\n tipo                 smallint,",
                    "\n detalle              char(120))"
   PREPARE prp_crea_tbl_temp FROM v_s_qryTxt
   EXECUTE prp_crea_tbl_temp 
   IF(SQLCA.SQLCODE <> 0)THEN
      LET v_error = -1
      RETURN v_error
   END IF
   
   CALL v_ch_arch.openFile( v_ruta_docto CLIPPED||p_v_usuario CLIPPED||"."||p_v_archivo CLIPPED, "r" )
   --CALL v_ch_arch.openFile( v_ruta_docto CLIPPED||p_v_archivo, "r" )
   DISPLAY "ESTADO LECTURA ARCHIVO"
   DISPLAY STATUS
   WHILE v_ch_arch.read([r_tmp_sep_arch.*])
      --DISPLAY r_tmp_sep_arch.nss

      INSERT INTO tmp_sep_batch_contabilidad
                  (  tpo_registro  ,
                     nss           ,
                     id_credito    ,
                     bimestre      ,
                     f_envio       ,
                     clave         ,
                     nrp           ,
                     mto_decimal   ,
                     mto_txt       ,
                     id_transaccion,
                     tipo          ,
                     detalle       
                   )
           VALUES (  r_tmp_sep_arch.tpo_registro  ,
                     r_tmp_sep_arch.nss           ,
                     r_tmp_sep_arch.id_credito    ,
                     r_tmp_sep_arch.bimestre      ,
                     r_tmp_sep_arch.f_envio       ,
                     r_tmp_sep_arch.clave         ,
                     r_tmp_sep_arch.nrp           ,
                     r_tmp_sep_arch.mto_txt / 100 , -- a dos decimales
                     r_tmp_sep_arch.mto_txt       ,
                     r_tmp_sep_arch.id_transaccion,
                     r_tmp_sep_arch.tipo          ,
                     r_tmp_sep_arch.detalle       )

      --DISPLAY "ERROR SQL:",SQLCA.SQLCODE
      --DISPLAY "COLUMNA DETALLE:",r_tmp_sep_arch.detalle
      IF(SQLCA.SQLCODE <> 0)THEN
         
         LET v_error = -1
         RETURN v_error
      END IF
   END WHILE   
   
   CALL v_ch_arch.close()

   LET v_s_qryTxt = "INSERT INTO sep_batch_contabilidad",
                     " SELECT seq_sep_batch_contabilidad.NEXTVAL,",
                     "        ",p_v_expediente,"   ,",
                     "        0                    ,", -- folio = NULL
                     "        tpo_registro         ,",
                     "        nss                  ,",
                     "        id_credito           ,",
                     "        bimestre             ,",
                     "        clave                ,",
                     "        nrp                  ,",
                     "        mto_decimal          ,",
                     "        id_transaccion       ,",
                     "        tipo                 ,",
                     "        detalle              ,",
                     "        0                    , ", --ind_envio            
                     "        TO_DATE(f_envio,'%Y%m%d') ", --ind_envio            
                     "   FROM tmp_sep_batch_contabilidad",
                     "  WHERE tpo_registro = 2"
   
   PREPARE EnuInsContabilidad FROM v_s_qryTxt
   EXECUTE EnuInsContabilidad

   IF (SQLCA.SQLERRD[3] == 0)  OR (SQLCA.SQLCODE <> 0 ) THEN
      DISPLAY "ERROR EN ESTRUCTURA DE ARCHIVO, DATOS NO CARGADOS"
      DISPLAY "SQLCODEL:",SQLCA.SQLCODE
      LET v_error = -1
      RETURN v_error
   END IF 

   DISPLAY "SQLCODE:               ",SQLCA.SQLCODE
   DISPLAY "REGISTROS INSERTAEDOS: ",SQLCA.SQLERRD[3]
   
   --EXECUTE prp_maq_edo_sep USING "maq_sep_expediente","60","45" 
   --   INTO v_ind, v_diag, v_estado_destino
   --DISPLAY "ERROR SQL:",SQLCA.SQLCODE
   --IF(SQLCA.SQLCODE <> 0)THEN
   --   LET v_error = -1
   --   RETURN v_error
   --END IF
   
   --IF v_ind = 0 THEN
      
      --UPDATE sep_expediente
       --  SET estado = v_estado_destino
       --WHERE id_expediente = p_v_expediente
       
      --DISPLAY "ERROR SQL:",SQLCA.SQLCODE
      --IF(SQLCA.SQLCODE <> 0)THEN
      --   LET v_error = -1
      --   RETURN v_error
      --END IF
      # se actualiza el indicador para el archivo
      UPDATE sep_expediente
         SET ind_ajuste = 2 # Archivo Confirmado
       WHERE id_expediente = p_v_expediente
--   ELSE
--      LET v_error = 0
      --CALL fn_mensaje("Advertencia","Problemas con la maquinaria de estados","info")
--      DISPLAY "Problemas con la maquinaria de estados"
--   END IF

   RETURN v_error
END FUNCTION

