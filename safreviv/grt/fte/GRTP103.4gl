################################################################################
#Modulo            => GRT                                                      #
#Programa          => GRTP103                                                  #
#Objetivo          => Programa que realiza las validaciones del subproceso     #
#                     (3) Uso de garantía                                      #
#Autor             => Héctor F. Jiménez Lara                                   #
#Fecha inicio      => 20 Octube 2015                                           #
################################################################################
DATABASE safre_viv

   DEFINE p_usuario             LIKE seg_usuario.usuario       -- nombre del usuario
   DEFINE v_opera_cod           LIKE cat_operacion.opera_cod   -- coódigo de la operacion de la etapa
   DEFINE p_v_arch_proceso      VARCHAR(100)                   -- nombre del archivo a integrar
   DEFINE v_s_qry               STRING
   DEFINE p_pid                 DECIMAL(9,0)
   DEFINE v_proceso_cod         LIKE cat_proceso.proceso_cod   -- código del proceso
   
MAIN
   DEFINE v_id_grt_ctr_arch     LIKE grt_ctr_archivo.id_grt_ctr_archivo
   DEFINE v_programa_cod        LIKE seg_programa.programa_cod
   DEFINE v_sp_error            SMALLINT
   DEFINE r_b_valida            SMALLINT 
   DEFINE v_subproceso          SMALLINT 
   DEFINE v_cnt_aceptados       INTEGER
   DEFINE v_cnt_rechazados      INTEGER
   DEFINE v_estado              SMALLINT

   -- se recuperan los parametros que envia el programa lanzador
   LET p_usuario        = ARG_VAL(1)
   LET p_v_arch_proceso = ARG_VAL(2)
   LET p_pid            = ARG_VAL(3)

   LET v_proceso_cod     = 3906
   LET v_opera_cod       = 1
   LET v_programa_cod    = "GRTP103"
   LET v_id_grt_ctr_arch = 0 
   LET p_v_arch_proceso  = p_v_arch_proceso CLIPPED     -- Se eliminan los espacios en blanco
   LET v_subproceso      = 1
   LET v_cnt_aceptados   = 0
   LET v_cnt_rechazados  = 0
   
   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".GRTP103.log")

   DISPLAY "=INICIA GRTP103="
   DISPLAY "   USUARIO    : ",p_usuario
   DISPLAY "   PID        : ",p_pid 

   -- Se obtiene el id del archivo 
   LET v_s_qry = " SELECT id_grt_ctr_archivo
                     FROM grt_ctr_archivo
                    WHERE nom_archivo = ? "

   PREPARE prp_obt_id_arch FROM v_s_qry
   EXECUTE prp_obt_id_arch INTO v_id_grt_ctr_arch 
                          USING p_v_arch_proceso 

   -- Se ejecuta la función que realiza el proceso
   LET v_s_qry = "EXECUTE FUNCTION fn_grt_valida_sp3_solic_ug(?)"

   PREPARE prp_exe_fn_proceso FROM v_s_qry
   EXECUTE prp_exe_fn_proceso USING v_id_grt_ctr_arch 
                               INTO v_sp_error,
                                    v_cnt_aceptados,
                                    v_cnt_rechazados 

   DISPLAY "\n   ACEPTADOS  : ", v_cnt_aceptados
   DISPLAY "   RECHAZADOS : ", v_cnt_rechazados

   -- verifica si ocurrió un error durante el proceos de marcaje
   IF v_sp_error <> 0 THEN
      -- Ocurrió un error, se muestra el error
      DISPLAY "OCURRIÓ UN ERROR EN EL PROCESO DE SOLICITUD DE USO DE GARANTÍA: ",v_sp_error

      -- ocurrió un error y se marca como rechazado la operación
      LET r_b_valida = fn_error_opera(p_pid, v_proceso_cod, v_opera_cod)
      -- se verifica si fue posible finalizar la operacion
      IF r_b_valida <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      CALL fn_finaliza_procesos()

      EXIT PROGRAM
   END IF

   -- Se actualiza el estado del SP002 a Finalizado
   LET v_s_qry = " UPDATE grt_ctr_proceso
                      SET fin_sp3 = 1 
                    WHERE id_grt_ctr_archivo = ?"

   PREPARE prp_upd_edo FROM v_s_qry
   EXECUTE prp_upd_edo USING v_id_grt_ctr_arch

   IF v_estado = 0 THEN

      CALL fn_actualiza_opera_fin(p_pid,
                                  v_proceso_cod,
                                  v_opera_cod) RETURNING v_estado
   ELSE
      --Si ocurrio un error se actualiza el estatus como erroneo
      CALL fn_error_opera(p_pid, v_proceso_cod, v_opera_cod)  RETURNING v_estado
   END IF

   DISPLAY "=FIN="
END MAIN


#Objetivo: Función que finaliza las operaciones del proceso
FUNCTION fn_finaliza_procesos()
   DEFINE v_i_proceso_cod  LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE v_i_opera_cod    LIKE cat_operacion.opera_cod -- codigo de la operacion
   DEFINE v_s_qryTxt       STRING -- se asigna una sentencia sql a ejecutar
   DEFINE v_dt_tiempo      DATETIME YEAR TO SECOND -- variable con fecha y hora
   DEFINE r_b_valida       SMALLINT -- status de registro de las funciones de actualización

   -- se asigna la fecha y hora actual
   LET v_dt_tiempo = CURRENT

   -- se actializa la operación como erronea
   LET v_s_qryTxt = " UPDATE bat_ctr_operacion\n",
                    "    SET fecha_fin   = '",v_dt_tiempo,"',\n",
                    "        estado_cod  = 3\n",
                    "  WHERE pid         = ",p_pid,"\n",
                    "    AND proceso_cod = ",v_proceso_cod,"\n",
                    "    AND opera_cod   = ",v_opera_cod

   PREPARE prp_act_error_opera FROM v_s_qryTxt
   EXECUTE prp_act_error_opera

   -- se actualiza el proceso como erronea
   LET v_s_qryTxt = " UPDATE bat_ctr_proceso\n",
                    "    SET fecha_fin   = TODAY,\n",
                    "        estado_cod  = 3\n",
                    "  WHERE pid         = ",p_pid,"\n",
                    "    AND proceso_cod = ",v_proceso_cod

   PREPARE prp_act_error_proceso FROM v_s_qryTxt
   EXECUTE prp_act_error_proceso

END FUNCTION
