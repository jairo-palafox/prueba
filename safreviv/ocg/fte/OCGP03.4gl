################################################################################
#Modulo            => OCG                                                      #
#Programa          => OCGP103                                                  #
#Objetivo          => Programa que realiza las validaciones del subproceso     #
#                     (3) Uso de garantía                                      #
#Autor             => Héctor F. Jiménez Lara                                   #
#Modificado        => José Eduardo Ventura 10/10/2016                          #
#Fecha inicio      => 20 Octube 2015                                           #
################################################################################
DATABASE safre_viv

   GLOBALS "OCGW03.inc"

   DEFINE p_usuario             LIKE seg_usuario.usuario       -- nombre del usuario
   DEFINE v_opera_cod           LIKE cat_operacion.opera_cod   -- coódigo de la operacion de la etapa
   DEFINE p_v_arch_proceso      VARCHAR(100)                   -- nombre del archivo a integrar
   DEFINE v_s_qry               STRING
   DEFINE p_pid                 DECIMAL(9,0)
   DEFINE v_proceso_cod         LIKE cat_proceso.proceso_cod   -- código del proceso

--- variables para consulta de relación laboral en WS de TRM
   DEFINE v_nss                 CHAR(11)
   DEFINE v_ws_status      SMALLINT -- estatus de ejecución del ws
   DEFINE v_indice         INTEGER
   DEFINE v_fecha_f        DATE
   DEFINE v_fecha_c        STRING
   DEFINE v_nrp            VARCHAR(100)
   DEFINE v_f_alta         DATE
   DEFINE v_f_alta_format  STRING
   DEFINE v_delegacion     DATE
   DEFINE v_nom_delegacion VARCHAR(100)
   DEFINE v_r_nss          CHAR(11)
   DEFINE v_idx            SMALLINT
   
MAIN

   DEFINE v_id_ocg_ctr_arch     LIKE ocg_ctr_archivo.id_ocg_ctr_archivo
   DEFINE v_programa_cod        LIKE seg_programa.programa_cod
   DEFINE v_sp_error            SMALLINT
   DEFINE r_b_valida            SMALLINT 
   DEFINE v_subproceso          SMALLINT 
   DEFINE v_cnt_aceptados       INTEGER
   DEFINE v_cnt_rechazados      INTEGER
   DEFINE v_estado              SMALLINT
   DEFINE v_ruta                VARCHAR(150)
   DEFINE v_usuario             CHAR(40)
   DEFINE v_password            CHAR(20)
   DEFINE v_intentos            SMALLINT
   DEFINE v_tot_rch_sp3_val     INTEGER

   -- se recuperan los parametros que envia el programa lanzador
   LET p_usuario        = ARG_VAL(1)
   LET p_v_arch_proceso = ARG_VAL(2)
   LET p_pid            = ARG_VAL(3)

   LET v_proceso_cod     = 3906
   LET v_opera_cod       = 1
   LET v_programa_cod    = "OCGP03"
   LET v_id_ocg_ctr_arch = 0 
   LET p_v_arch_proceso  = p_v_arch_proceso CLIPPED     -- Se eliminan los espacios en blanco
   LET v_subproceso      = 1
   LET v_cnt_aceptados   = 0
   LET v_cnt_rechazados  = 0
   
   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".OCGP03.log")

   DISPLAY "=INICIA OCGP03="
   DISPLAY "   USUARIO    : ",p_usuario
   DISPLAY "   PID        : ",p_pid 

   -- Se obtiene el id del archivo 
   LET v_s_qry = " SELECT id_ocg_ctr_archivo
                     FROM ocg_ctr_archivo
                    WHERE nom_archivo = ? "

   PREPARE prp_obt_id_arch FROM v_s_qry
   EXECUTE prp_obt_id_arch INTO v_id_ocg_ctr_arch 
                          USING p_v_arch_proceso
{
   --se ejecuta función para consulta de relación laboral en WS de TRM
   CALL fn_crea_temporal()

   ---cve_cliente = ocg_1 ruta para consulta de relación laboral en TRM
   LET v_s_qry = "SELECT ruta_servidor, 
                         usuario, 
                         password, 
                         num_reintento 
                    FROM wsv_cliente 
                   WHERE cve_cliente = 'ocg_1' "

   PREPARE prp_crl FROM v_s_qry
   EXECUTE prp_crl INTO v_ruta,
                        v_usuario,
                        v_password,
                        v_intentos

   LET v_ruta = v_ruta CLIPPED

   LET v_s_qry = " SELECT nss
                     FROM safre_tmp:tmp_rec_det_ocg43
                    WHERE subproceso = 3 "

   PREPARE prp_c_tmp FROM v_s_qry
   DECLARE cur_c_tmp CURSOR FOR prp_c_tmp

   DISPLAY "ruta WS : ",v_ruta

   FOREACH cur_c_tmp INTO v_nss
      LET ns1consultarTrabajador.nss = v_nss
   
      -- se invoca la consulta
      CALL consultarTrabajador_g(v_ruta) RETURNING v_ws_status
   
      -- si el webservice NO se ejecuto correctamente
      IF ( v_ws_status <> 0 ) THEN      
   
         DISPLAY "ERROR al invocar webservice de consulta de relación laboral"
         DISPLAY "CODE       : ", wsError.code
         DISPLAY "CODENS     : ", wsError.codeNS
         DISPLAY "DESRIPTION : ", wsError.description
         DISPLAY "ACTION     : ", wsError.action
   	  
   	  -- se devuelve el codigo de error del WS y fecha nula
         RETURN wsError.code
         CONTINUE FOREACH
      END IF
   
      FOR v_indice = 1 TO ns1consultarTrabajadorResponse.estatusLaboralTrabajador.relacionLab.getLength()
         --se formatea la fecha
         LET v_fecha_c = ns1consultarTrabajadorResponse.estatusLaboralTrabajador.relacionLab[v_indice].fechaBaja
         LET v_fecha_c = v_fecha_c.subString(6,7),"/",v_fecha_c.subString(9,10),"/",v_fecha_c.subString(1,4)
         LET v_fecha_f = DATE(v_fecha_c)
   
         --consulta fecha alta
         LET v_f_alta_format = ns1consultarTrabajadorResponse.estatusLaboralTrabajador.relacionLab[v_indice].fechaAlta
         LET v_f_alta_format = v_f_alta_format.subString(6,7),"/",v_f_alta_format.subString(9,10),"/",v_f_alta_format.subString(1,4)
         LET v_f_alta = DATE(v_f_alta_format)
   
         LET v_r_nss          = ns1consultarTrabajadorResponse.estatusLaboralTrabajador.nss
         LET v_delegacion     = ns1consultarTrabajadorResponse.estatusLaboralTrabajador.relacionLab[v_indice].delegacion
         LET v_nom_delegacion = ns1consultarTrabajadorResponse.estatusLaboralTrabajador.relacionLab[v_indice].nomDelegacion
         LET v_nrp            = ns1consultarTrabajadorResponse.estatusLaboralTrabajador.relacionLab[v_indice].nrp
   
         LET v_s_qry = "INSERT INTO safre_tmp:tmp_relacion_laboral values(?,?,?);"
   
         PREPARE prp_ins_tmp FROM v_s_qry
         EXECUTE prp_ins_tmp USING v_r_nss,
                                   v_f_alta,
                                   v_fecha_f
   
      END FOR

      LET v_idx = v_idx + 1
   END FOREACH
}

   SLEEP 25

   -- Se ejecuta la función que realiza el proceso
   LET v_s_qry = "EXECUTE FUNCTION fn_ocg_valida_sp3_solic_ug(?)"

   PREPARE prp_exe_fn_proceso FROM v_s_qry
   EXECUTE prp_exe_fn_proceso USING v_id_ocg_ctr_arch 
                               INTO v_sp_error,
                                    v_cnt_aceptados,
                                    v_cnt_rechazados 

   LET v_s_qry = "SELECT tot_sp3_rch_val
                    FROM safre_tmp:tmp_ocg_cifras;"

   PREPARE prp_rch_sp3 FROM v_s_qry
   EXECUTE prp_rch_sp3 INTO v_tot_rch_sp3_val

   DISPLAY "\n=> TOTAL ACEPTADOS:", v_cnt_aceptados
   DISPLAY "=> TOTAL RECHAZOS OPERATIVOS:", v_cnt_rechazados
   DISPLAY "=> TOTAL RECHAZOS VALIDACIÓN:",v_tot_rch_sp3_val

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
   LET v_s_qry = " UPDATE ocg_ctr_proceso
                      SET fin_sp3 = 1 
                    WHERE id_ocg_ctr_archivo = ?"

   PREPARE prp_upd_edo FROM v_s_qry
   EXECUTE prp_upd_edo USING v_id_ocg_ctr_arch

   IF v_estado = 0 THEN

       UPDATE tmp_ctr_subproceso
                        SET sp003 = 1
                      WHERE id_ocg_ctr_arch = v_id_ocg_ctr_arch

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

FUNCTION fn_crea_temporal()
      DATABASE safre_tmp
      LET v_s_qry = "DROP TABLE IF EXISTS tmp_relacion_laboral; \n
                     CREATE TABLE tmp_relacion_laboral(
                        nss     char(11),
                        f_alta  date,
                        f_baja  date
                        );"

      PREPARE prp_Crea_t FROM v_s_qry
      EXECUTE prp_Crea_t

      IF sqlca.sqlcode = 0 THEN
        -- DISPLAY "Se crea la tabla temporal de relaciones laborales" 
      END IF
      DATABASE safre_viv
END FUNCTION 
