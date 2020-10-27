--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

#########################################################################
#Modulo            =>ACR                                                #
#Programa          =>ACRP24                                             #
#Objetivo          =>Programa que genera un registro en la tabla de     #
#                   control del proceso en el modulo de Valida archivo  #
#                   de Confirmación Devolución de Saldos                #
#Autor             =>Daniel Buendia, EFP                                #
#Fecha inicio      =>01 Febrero 2012                                    #
#########################################################################

DATABASE safre_viv

DEFINE p_v_usuario       LIKE seg_usuario.usuario, -- nombre del usuario
       p_d_pid           LIKE bat_ctr_proceso.pid, -- pid
       p_i_proceso_cod   LIKE cat_proceso.proceso_cod, -- codigo del proceso
       p_i_opera_cod     LIKE cat_operacion.opera_cod, -- codigo de la operacion de la etapa
       p_d_folio         LIKE glo_ctr_archivo.folio, -- numero de folio
       p_v_arch_proceso  VARCHAR(100) -- nombre del archivo a integrar

MAIN
   DEFINE v_i_estado_cod    LIKE bat_ctr_operacion.estado_cod, -- estado del proceso
          v_i_estado        LIKE dse_ctr_archivo.estado, -- estado del registro de control
          v_i_tot_registros LIKE cre_ctr_archivo.tot_registros, -- total de registros procesados
          v_s_qryTxt        STRING, -- guarda una sentencia SQL a ejecutar
          r_b_estatus_proc  SMALLINT, -- estatus de retorno del Store
          r_b_valida        SMALLINT -- status de registro de las funciones de actualización

   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario      = ARG_VAL(1)
   LET p_d_pid          = ARG_VAL(2)
   LET p_i_proceso_cod  = ARG_VAL(3)
   LET p_i_opera_cod    = ARG_VAL(4)
   LET p_d_folio        = ARG_VAL(5)
   LET p_v_arch_proceso = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".ACRP24.log")

   DISPLAY "=INICIA ACRP24="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " FOLIO         : ",p_d_folio
   DISPLAY " ARCHIVO       : ",p_v_arch_proceso

   -- si no viene el nombre del archivo, invocar la función que busca el nombre
   IF p_v_arch_proceso = "NA" OR p_v_arch_proceso = "N/A" THEN
     -- se invoca la función que obtiene el nombre del archivo
     LET p_v_arch_proceso = fn_recupera_arch_cargado(p_i_proceso_cod, p_i_opera_cod)
   END IF
   DISPLAY " ARCHIVO       : ",p_v_arch_proceso

   -- se valida el pid
   IF p_d_pid = 0 THEN
      -- se invoca la función que obtiene el máximo pid
      LET p_d_pid = fn_max_pid(p_i_proceso_cod, p_i_opera_cod)
   END IF

   -- se crea la sentencia sql que verifica el estatus del proceso de carga
   LET v_s_qryTxt = " SELECT estado_cod\n",
                    "   FROM bat_ctr_operacion\n",
                    "  WHERE pid = ",p_d_pid,"\n",
                    "    AND proceso_cod = ",p_i_proceso_cod,"\n",
                    "    AND opera_cod = ",p_i_opera_cod

   PREPARE prp_slc_estado_cod FROM v_s_qryTxt
   EXECUTE prp_slc_estado_cod INTO v_i_estado_cod

   -- verifica si el estado fue diferente de 'valido'
   IF v_i_estado_cod = 4 THEN
      LET v_i_estado = 10
   ELSE
      DISPLAY " ERROR: NO SE PROCESARON TODOS LOS REGISTROS CORRECTAMENTE"
      LET v_i_estado = 30
   END IF

   -- se invoca la funcion que inserta el registro en la tabla de control de archivos
   LET v_s_qryTxt = "EXECUTE FUNCTION safre_viv:fn_insrt_dse_ctr_arch_confdevsdos(?,?,?,?)"

   PREPARE prp_procd_ins_dse_ctr_arch FROM v_s_qryTxt
   EXECUTE prp_procd_ins_dse_ctr_arch USING p_v_arch_proceso, p_d_pid,
                                            p_v_usuario, v_i_estado
                                       INTO r_b_estatus_proc, v_i_tot_registros

   -- se verifica si el archivo fue marcado como valido
   IF r_b_estatus_proc <> 0 THEN
      -- archivo rechazado. muestra mensaje del error
      CALL fn_display(r_b_estatus_proc)

      -- se invoca función que finaliza las operaciones del proceso
      CALL fn_finaliza_procesos()

      -- se actualiza al esta a glo ctr archivo como Reversado
      CALL fn_act_edo_archivo(p_v_arch_proceso, p_d_folio, 3, p_v_usuario) RETURNING r_b_valida
      DISPLAY " RETORNO DE LA ACTUALIZACIÓN: " ,r_b_valida
   ELSE
      DISPLAY " TOTAL DE REGISTROS ENCABEZADO:    1"
      DISPLAY " TOTAL DE REGISTROS DETALLE   : ",v_i_tot_registros
      DISPLAY " TOTAL DE REGISTROS SUMARIO   :    1"
      DISPLAY " SE PROCESO LA VALIDACIÓN CORRECTAMENTE"
   END IF
   DISPLAY "=FIN="
END MAIN

#Objetivo: Función que muestra el display correspondiente al codigo de error
FUNCTION fn_display(v_b_estatus_proc)
   DEFINE v_b_estatus_proc   SMALLINT

   CASE v_b_estatus_proc
      WHEN 1
         DISPLAY " ERROR: EL ARCHIVO NO CONTIENE DETALLE"
      WHEN 2
         DISPLAY " ERROR: EL ARCHIVO NO CONTIENE SUMARIO O EXISTE MÁS DE UNO"
      WHEN 3
         DISPLAY " ERROR: EL ARCHIVO NO CONTIENE ENCABEZADO O EXISTE MÁS DE UNO"
      WHEN 4
         DISPLAY " ERROR: EL TOTAL EN SUMARIO NO COINCIDE CON EL TOTAL DE REGISTROS EN ARCHIVO"
      WHEN 5
         DISPLAY " ERROR: EL ARCHIVO CONTIENE REGISTROS CON NSS NO CATALOGADOS"
      WHEN 6
         DISPLAY " ERROR: LA OPERACIÓN DEL ARCHIVO NO CORRESPONDE CON EL PROCESO"
      WHEN 7
         DISPLAY " ERROR: EL TIPO DE TRANSFERENCIA ES DIFERENTE AL REQUERIDO"
   END CASE
END FUNCTION

#Objetivo: Función que finaliza las operaciones del proceso
FUNCTION fn_finaliza_procesos()
   DEFINE v_i_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
          v_i_opera_cod    LIKE cat_operacion.opera_cod, -- codigo de la operacion
          v_s_qryTxt       STRING, -- se asigna una sentencia sql a ejecutar
          v_dt_tiempo      DATETIME YEAR TO SECOND, -- variable con fecha y hora
          r_b_valida       SMALLINT -- status de registro de las funciones de actualización

   -- se asigna la fecha y hora actual
   LET v_dt_tiempo = CURRENT

   -- se actializa la operación como erronea
   LET v_s_qryTxt = " UPDATE bat_ctr_operacion\n",
                    "    SET fecha_fin   = '",v_dt_tiempo,"',\n",
                    "        estado_cod  = 3\n",
                    "  WHERE pid         = ",p_d_pid,"\n",
                    "    AND proceso_cod = ",p_i_proceso_cod,"\n",
                    "    AND opera_cod   = ",p_i_opera_cod

   PREPARE prp_act_error_opera FROM v_s_qryTxt
   EXECUTE prp_act_error_opera

   -- se actializa el proceso como erronea
   LET v_s_qryTxt = " UPDATE bat_ctr_proceso\n",
                    "    SET fecha_fin   = TODAY,\n",
                    "        estado_cod  = 3\n",
                    "  WHERE pid         = ",p_d_pid,"\n",
                    "    AND proceso_cod = ",p_i_proceso_cod

   PREPARE prp_act_error_proceso FROM v_s_qryTxt
   EXECUTE prp_act_error_proceso

END FUNCTION
