--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

################################################################################
#Modulo            =>ACR                                                       #
#Programa          =>ACRP22                                                    #
#Objetivo          =>Programa que genera un registro en la tabla de            #
#                   control del proceso. Proceso Devoluciones Saldo            #
#Autor             =>Daniel Buendia, EFP                                       #
#Fecha inicio      =>01 Febrero 2012                                           #
#Modificaciones                                                                #
#Autor             =>H�ctor Jim�nez                                            #
#Descripci�n       =>Se cambia la validaci�n para que cuando existan nss       #
#                    no catalogados, continue el proceso                       #
################################################################################

DATABASE safre_viv

   DEFINE p_v_usuario       LIKE seg_usuario.usuario -- nombre del usuario
   DEFINE p_d_pid           LIKE bat_ctr_proceso.pid -- pid
   DEFINE p_i_proceso_cod   LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE p_i_opera_cod     LIKE cat_operacion.opera_cod -- codigo de la operacion de la etapa
   DEFINE p_d_folio         LIKE glo_ctr_archivo.folio   -- numero de folio
   DEFINE p_v_arch_proceso  VARCHAR(100) -- nombre del archivo a integrar

MAIN
   DEFINE v_i_estado_cod    LIKE bat_ctr_operacion.estado_cod -- estado del proceso
   DEFINE v_i_estado        LIKE cre_ctr_archivo.estado -- estado del registro de control
   DEFINE v_i_tot_registros LIKE cre_ctr_archivo.tot_registros -- total de registros procesados
   DEFINE v_s_qryTxt        STRING    -- guarda una sentencia SQL a ejecutar
   DEFINE r_si_cod_error    SMALLINT  -- codigo de error en caso de excepci�n
   DEFINE r_b_estatus_proc  SMALLINT  -- estatus de retorno del Store
   DEFINE r_b_valida        SMALLINT  -- status de registro de las funciones de actualizaci�n

   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario      = ARG_VAL(1)
   LET p_d_pid          = ARG_VAL(2)
   LET p_i_proceso_cod  = ARG_VAL(3)
   LET p_i_opera_cod    = ARG_VAL(4)
   LET p_d_folio        = ARG_VAL(5)
   LET p_v_arch_proceso = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".ACRP22.log")

   DISPLAY "=INICIA ACRP22="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"

   -- si no viene el nombre del archivo, invocar la funci�n que busca el nombre
   IF p_v_arch_proceso = "NA" OR p_v_arch_proceso = "N/A" THEN
     -- se invoca la funci�n que obtiene el nombre del archivo
     LET p_v_arch_proceso = fn_recupera_arch_cargado(p_i_proceso_cod, p_i_opera_cod)
   END IF
   DISPLAY " ARCHIVO       : ",p_v_arch_proceso

   -- se valida el pid
   IF p_d_pid = 0 THEN
      -- se invoca la funci�n que obtiene el m�ximo pid
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

   -- Se ejecuta al SP que crea la tabla temporal
   DATABASE safre_tmp
   LET v_s_qryTxt = "EXECUTE PROCEDURE sp_nss_no_catalogados_acr()"

   PREPARE prp_cre_tmp_rech FROM v_s_qryTxt
   EXECUTE prp_cre_tmp_rech

   DATABASE safre_viv
   -- se invoca la funcion que inserta el registro en la tabla de control de archivos
   LET v_s_qryTxt = "EXECUTE FUNCTION fn_insrt_dse_ctr_arch_devsdos(?,?,?,?)"

   PREPARE prp_procd_ins_dse_ctr_arch FROM v_s_qryTxt
   EXECUTE prp_procd_ins_dse_ctr_arch USING p_v_arch_proceso, p_d_pid,
                                            p_v_usuario, v_i_estado
                                       INTO r_si_cod_error, r_b_estatus_proc, v_i_tot_registros

   -- se verifica si el archivo fue marcado como valido
   IF r_si_cod_error <> 0 THEN
      DISPLAY "ERROR: Ocurri� un error en el proceso: ",r_si_cod_error
      -- se invoca funci�n que finaliza las operaciones del proceso
      CALL fn_finaliza_procesos()

      -- se actualiza al esta a glo ctr archivo como Reversado
      CALL fn_act_edo_archivo(p_v_arch_proceso, p_d_folio, 3, p_v_usuario) RETURNING r_b_valida
      DISPLAY " RETORNO DE LA ACTUALIZACI�N: " ,r_b_valida
   END IF

   -- se verifica si el archivo fue marcado como valido
   IF r_b_estatus_proc <> 0 AND r_b_estatus_proc <> 5 THEN
      -- archivo rechazado. muestra mensaje del error
      CALL fn_display(r_b_estatus_proc)

      -- se invoca funci�n que finaliza las operaciones del proceso
      CALL fn_finaliza_procesos()

      -- se actualiza al esta a glo ctr archivo como Reversado
      CALL fn_act_edo_archivo(p_v_arch_proceso, p_d_folio, 3, p_v_usuario) RETURNING r_b_valida
      DISPLAY " RETORNO DE LA ACTUALIZACI�N: " ,r_b_valida
   ELSE
      IF r_b_estatus_proc = 5 THEN
         CALL fn_genera_archivo_no_catalogados(p_v_arch_proceso)
      END IF
      DISPLAY " TOTAL DE REGISTROS ENCABEZADO:    1"
      DISPLAY " TOTAL DE REGISTROS DETALLE   : ",v_i_tot_registros
      DISPLAY " TOTAL DE REGISTROS SUMARIO   :    1"
      DISPLAY " SE PROCESO LA VALIDACI�N CORRECTAMENTE"
   END IF
   DISPLAY "=FIN="
END MAIN

#Objetivo: Funci�n que muestra el display correspondiente al codigo de error
FUNCTION fn_display(v_b_estatus_proc)
   DEFINE v_b_estatus_proc   SMALLINT

   CASE v_b_estatus_proc
      WHEN 1
         DISPLAY " ERROR: EL ARCHIVO NO CONTIENE DETALLE"
      WHEN 2
         DISPLAY " ERROR: EL ARCHIVO NO CONTIENE SUMARIO O EXISTE M�S DE UNO"
      WHEN 3
         DISPLAY " ERROR: EL ARCHIVO NO CONTIENE ENCABEZADO O EXISTE M�S DE UNO"
      WHEN 4
         DISPLAY " ERROR: EL TOTAL EN SUMARIO NO COINCIDE CON EL TOTAL DE REGISTROS EN ARCHIVO"
      --WHEN 5
         --CALL fn_genera_archivo_no_catalogados()
         --DISPLAY " ERROR: EL ARCHIVO CONTIENE REGISTROS CON NSS NO CATALOGADOS"
      WHEN 6
         DISPLAY " ERROR: LA OPERACI�N DEL ARCHIVO NO CORRESPONDE CON EL PROCESO"
      WHEN 7
         DISPLAY " ERROR: EL TIPO DE TRANSFERENCIA ES DIFERENTE AL REQUERIDO"
   END CASE
END FUNCTION

#Objetivo: Funci�n que finaliza las operaciones del proceso
FUNCTION fn_finaliza_procesos()
   DEFINE v_i_proceso_cod  LIKE cat_proceso.proceso_cod -- c�digo del proceso
   DEFINE v_i_opera_cod    LIKE cat_operacion.opera_cod -- c�digo de la operacion
   DEFINE v_s_qryTxt       STRING -- se asigna una sentencia sql a ejecutar
   DEFINE v_dt_tiempo      DATETIME YEAR TO SECOND -- variable con fecha y hora
   DEFINE r_b_valida       SMALLINT -- status de registro de las funciones de actualizaci�n

   -- se asigna la fecha y hora actual
   LET v_dt_tiempo = CURRENT

   -- se actializa la operaci�n como erronea
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

FUNCTION fn_genera_archivo_no_catalogados(v_arch_proceso)
   DEFINE v_f_format              base.StringBuffer
   DEFINE v_arch_format           base.StringBuffer
   DEFINE v_ruta_envio            CHAR(40)
   DEFINE v_nss                   CHAR (11)
   DEFINE v_fecha                 DATETIME YEAR TO DAY
   DEFINE v_cnt_rch               SMALLINT
   DEFINE v_cnt_nocat             SMALLINT
   DEFINE v_qry                   STRING
   DEFINE v_arch_proceso          VARCHAR(100)
   DEFINE v_nom_archivo_salida    VARCHAR(100)

   LET v_cnt_rch = 0
   --Obtiene nombre de archivo sin exensi�n
   LET v_arch_format = base.StringBuffer.create()
   CALL v_arch_format.append(v_arch_proceso)
   CALL v_arch_format.replace(".dac","",0)

   -- Se obtiene la ruta envio
   LET v_qry = "SELECT ruta_envio
                  FROM seg_modulo
                 WHERE modulo_cod = 'acr'"
   PREPARE prp_r_envio FROM v_qry
   EXECUTE prp_r_envio INTO v_ruta_envio

   -- Se obtiene el total de nss no catalogados
   LET v_qry = "SELECT COUNT(*)
                  FROM safre_tmp:tmp_nss_no_catalogados_acr"

   PREPARE prp_no_catalog FROM v_qry
   EXECUTE prp_no_catalog INTO v_cnt_nocat

   LET v_qry = "SELECT nss 
                  FROM safre_tmp:tmp_nss_no_catalogados_acr"
   PREPARE prp_nss_no FROM v_qry

   DISPLAY " \n EXISTEN " || v_cnt_nocat CLIPPED ||" NSS NO CATALOGADOS"
   DECLARE cur_no_catalogados CURSOR FOR  prp_nss_no
   FOREACH cur_no_catalogados INTO v_nss
      DISPLAY "  ",v_nss
   END FOREACH

   DISPLAY " \n INICIA GENERACI�N DE ARCHIVO CON NSS NO CATALOGADOS"

   LET v_fecha = TODAY
   LET v_f_format = base.StringBuffer.create()

   CALL v_f_format.append(v_fecha)
   CALL v_f_format.replace("-","",0)

   -- se arma el nombre del archivo de salida
   --LET v_nom_archivo_salida = "nss_no_catalogados_ssv_"|| v_f_format.toString() CLIPPED ||".rdac"
   LET v_nom_archivo_salida = v_arch_format.toString() CLIPPED ||"_"|| v_f_format.toString() CLIPPED ||".rdac"

   UNLOAD TO v_ruta_envio CLIPPED || "/" || v_nom_archivo_salida DELIMITER ' '
   SELECT  nss,
           saldo_vivienda97 / 100,
           saldo_vivienda92 / 100
     FROM safre_tmp:tmp_nss_no_catalogados_acr

   DISPLAY " ARCHIVO NSS NO CATALOGADOS   : ",v_nom_archivo_salida
   DISPLAY "\n"

END FUNCTION