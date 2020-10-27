--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 27-06-2012
--===============================================================

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPP34                                                   #
#Objetivo          => Programa batch de maquinaria para liquidacion op 28      # 
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => Junio 27, 2012                                           #
################################################################################
DATABASE safre_viv

DEFINE p_usuario_cod LIKE seg_usuario.usuario_cod, # Usuario
       p_pid         LIKE bat_ctr_proceso.pid,     # identificador de proceso
       p_proceso_cod LIKE cat_proceso.proceso_cod, # Código del proceso
       p_opera_cod   LIKE cat_operacion.opera_cod, # Código de la operacion
       p_folio       LIKE glo_ctr_archivo.folio,   # numero de folio
       v_nom_archivo LIKE glo_ctr_archivo.nombre_archivo

MAIN
DEFINE v_consulta STRING,
       v_reg_op28 RECORD
         v_invadido LIKE sep_det_02_op28.invadido,
         v_asociado LIKE sep_det_03_op28.asociado,
         v_estado   LIKE sep_det_02_op28.estado
       END RECORD,
       v_ind           SMALLINT,
       v_id_expediente LIKE sep_expediente.id_expediente,
       v_sql_error     INTEGER,
       v_senial        INTEGER,
       v_diag           CHAR(3),
       v_estado_destino SMALLINT

   WHENEVER ERROR CONTINUE
   
   # Se recuperan los parámetros
   LET p_usuario_cod = ARG_VAL(1)
   LET p_pid         = ARG_VAL(2)
   LET p_proceso_cod = ARG_VAL(3)
   LET p_opera_cod   = ARG_VAL(4)
   LET p_folio       = ARG_VAL(5)
   LET v_nom_archivo = ARG_VAL(6)

   # Batch que actualiza el estado de de sep_expediente

   LET v_senial = 60

   LET v_consulta = "\n SELECT det2.invadido,det3.asociado,det2.estado",
                    "\n   FROM sep_det_02_op28 det2 JOIN sep_det_03_op28 det3",
                    "\n     ON det3.id_det_02_op28 = det2.id_det_02_op28",
                    "\n  WHERE det2.folio = ?"
   PREPARE prp_rec_op28 FROM v_consulta
   DECLARE cur_rec_op28 CURSOR FOR prp_rec_op28
   FOREACH cur_rec_op28 USING p_folio INTO v_reg_op28.*

      # recuepra el id_expediente para actualizar sep_expediente
      LET v_consulta = " EXECUTE FUNCTION sp_sep_consulta_expediente(?,?,?)"
      PREPARE prp_rec_edo FROM v_consulta
      EXECUTE prp_rec_edo USING v_reg_op28.v_invadido,
                                v_reg_op28.v_asociado,
                                v_reg_op28.v_estado
                           INTO v_ind,
                                v_id_expediente,
                                v_sql_error
      IF(v_ind = 1)THEN
         # Ejecuta maquinaria que actualiza estado
         LET v_consulta = "EXECUTE FUNCTION fn_maquinaria_individual(?,?,?,?,?)"
         PREPARE prp_actualiza_edo_maq FROM v_consulta
         EXECUTE prp_actualiza_edo_maq USING "maq_sep_expediente",
                                             v_id_expediente,
                                             "id_expediente",
                                             v_senial,
                                             p_usuario_cod
                                        INTO v_ind,v_diag,v_estado_destino
        IF(v_ind = 0)THEN
           DISPLAY "Expediente con identificador ",v_id_expediente CLIPPED," actualizado"
        END IF

      END IF
   END FOREACH
END MAIN