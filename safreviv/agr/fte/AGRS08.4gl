--===============================================================
-- Versión: 1.0.0
-- Fecha ultima modificación:
--===============================================================

#####################################################################
#Modulo            => AGR                                           #
#Programa          => AGRS08                                        #
#Objetivo          => Programa que ejecuta el proceso de generación #
#                     de extractor de acreditados                   #
#Autor             => Daniel Buendia, EFP                           #
#Fecha inicio      => 17 Junio 2013                                 #
#####################################################################

DATABASE safre_viv

GLOBALS "AGRG01.4gl"

   DEFINE m_v_usuario               LIKE seg_usuario.usuario -- nombre del usuario
   DEFINE m_d_pid                   LIKE bat_ctr_proceso.pid -- pid
   DEFINE m_i_proceso_cod           LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE m_i_opera_cod             LIKE cat_operacion.opera_cod -- codigo de la operacion de la etapa
   DEFINE m_d_folio                 LIKE glo_ctr_archivo.folio -- numero de folio
   DEFINE m_v_arch_proceso          VARCHAR(100) -- nombre del archivo a integrar

MAIN

   DEFINE v_c_ruta_envio            LIKE seg_modulo.ruta_envio -- ruta donde se colocara el archivo
   DEFINE v_v_ruta_nomarch_sal      VARCHAR(150) -- ruta y nombre del archivo de salida
   DEFINE v_v_ruta_nomarch_pre      VARCHAR(150) -- ruta y nombre del archivo de salida
   DEFINE v_v_ruta_nomarch_cp       VARCHAR(100) -- ruta y nombre del archivo de salida (copia)
   DEFINE v_v_ruta_nomarch_sql      VARCHAR(100) -- ruta y nombre del archivo SQL del UNLOAD
   DEFINE v_c_extension             LIKE cat_operacion.extension -- extensión del archivo
   DEFINE v_s_comando               STRING -- contiene al comando a correr
   DEFINE v_s_registro              STRING -- registro a insertar
   DEFINE v_ch_arch_salida          BASE.CHANNEL -- manejador de apuntador hacia archivo
   DEFINE v_ch_arch_sql_unl         BASE.CHANNEL -- manejador de apuntador hacia archivo
   DEFINE v_i_cont_regs_arch        INTEGER -- total de registros en el archivo
   DEFINE v_c_programa_cod          LIKE cat_operacion.programa_cod -- nombre del programa
   DEFINE v_s_qryTxt                STRING -- guarda una sentencia sql a ejecutar
   DEFINE v_manejador_rpt           OM.SaxDocumentHandler -- Contenedor de Documentos para el reporte
   DEFINE v_total_ssv92             DECIMAL(18,2) -- monto total en pesos sar 92 (vigente)
   DEFINE v_total_ssv97             DECIMAL(18,2) -- monto total en pesos sar 97 (vigente)
   DEFINE v_c_marca_proc_aux        CHAR(1) -- marca procesar (auxiliar)
   DEFINE v_v_nom_reporte           VARCHAR(80) -- nombre del reporte
   DEFINE v_c_ruta_listado          LIKE seg_modulo.ruta_listados -- ruta donde se colocara el archivo    
   DEFINE r_b_valida                SMALLINT -- booleana que indica si el proceso se puede ejecutar o no
   DEFINE r_si_cod_error            SMALLINT -- en caso de error contiene el código
   DEFINE r_i_isam_error            INTEGER -- isam error
   DEFINE r_v_msj_error             VARCHAR(250) -- mensaje de error

   DEFINE v_arch_aux                STRING

   -- se recuperan los parametros que envia el programa lanzador
   LET m_v_usuario      = ARG_VAL(1)
   LET m_d_pid          = ARG_VAL(2)
   LET m_i_proceso_cod  = ARG_VAL(3)
   LET m_i_opera_cod    = ARG_VAL(4)
   LET m_d_folio        = ARG_VAL(5)
   LET m_v_arch_proceso = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(m_v_usuario CLIPPED|| ".AGRS08.log")

   DISPLAY "=INICIA AGRS08="
   DISPLAY " USUARIO       : ",m_v_usuario
   DISPLAY " PID           : ",m_d_pid
   DISPLAY " FOLIO         : ",m_d_folio USING "#########&"

   -- se inicializan variables
   LET v_total_ssv92 = 0
   LET v_total_ssv97 = 0

   DISPLAY " SE CREAN TABLAS TEMPORALES"
   -- se declara la base de datos temporal
   DATABASE safre_tmp

   -- se elimina la tabla en dado caso de que exista
   LET v_s_qryTxt = "EXECUTE PROCEDURE sp_cre_tbl_extraccion_acred()"

   PREPARE prp_extr_cred FROM v_s_qryTxt
   EXECUTE prp_extr_cred

   -- regresa a la base de datos safre viv
   DATABASE safre_viv

   DISPLAY " SE EJECUTA LA EXTRACCIÓN DE CUENTAS"

   -- se crean las sentencias que ejecutan las funciones para información de los créditos
   LET v_s_qryTxt = "EXECUTE FUNCTION fn_cre_inf_acred()"

   PREPARE prp_extr_inf FROM v_s_qryTxt
   EXECUTE prp_extr_inf INTO r_si_cod_error,
                             r_i_isam_error,
                             r_v_msj_error

   IF r_si_cod_error <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      DISPLAY " ERROR EN EL PROCESO DE EXTRACCIÓN DE CRÉDITOS"
      DISPLAY "  COD ERR    : ",r_si_cod_error
      DISPLAY "  ISAM ERR   : ",r_i_isam_error
      DISPLAY "  MENSAJE ERR: ",r_v_msj_error

      -- se invoca la función que deja la operación en estado ERRONEA
      LET r_b_valida = fn_error_opera(m_d_pid, m_i_proceso_cod, m_i_opera_cod)

      -- se verifica si fue posible marcar la operacion como Erronea
      IF r_b_valida <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF

   ---DATABASE safre_tmp

   ---LET v_s_qryTxt = "EXECUTE PROCEDURE sp_crea_idx_ext_acred()"

   ---PREPARE prp_act_stac FROM v_s_qryTxt
   ---EXECUTE prp_act_stac

   ---DATABASE safre_viv

   DISPLAY " SE EJECUTA LA EXTRACCIÓN DE SALDOS"
   -- se crean las sentencias que ejecutan las funciones para saldos de los créditos

   LET v_s_qryTxt = " EXECUTE FUNCTION fn_cre_extraccion_acred()"

   PREPARE prp_extracc_acred FROM v_s_qryTxt
   EXECUTE prp_extracc_acred INTO r_si_cod_error,
                                  r_i_isam_error,
                                  r_v_msj_error

   IF r_si_cod_error <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      DISPLAY " ERROR EN EL PROCESO DE EXTRACCIÓN DE CRÉDITOS"
      DISPLAY "  COD ERR     : ",r_si_cod_error
      DISPLAY "  ISAM ERR    : ",r_i_isam_error
      DISPLAY "  MENSAJE ERR : ",r_v_msj_error

      -- se invoca la función que deja la operación en estado ERRONEA
      LET r_b_valida = fn_error_opera(m_d_pid, m_i_proceso_cod, m_i_opera_cod)

      -- se verifica si fue posible marcar la operacion como Erronea
      IF r_b_valida <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF

   -- se obtienen la ruta envio y listados del modulo
   LET v_s_qryTxt = " SELECT ruta_envio, ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'agr'"

   PREPARE prp_slc_ruta_envio FROM v_s_qryTxt
   EXECUTE prp_slc_ruta_envio INTO v_c_ruta_envio, v_c_ruta_listado

   -- se obtiene la fecha de liquidación del nombre del archivo
   LET v_v_ruta_nomarch_sal = v_c_ruta_envio CLIPPED || "/" || m_v_arch_proceso CLIPPED

   -- se carga la configuración del reporte
   IF fgl_report_loadCurrentSettings("AGRS081.4rp") THEN
      -- se obtiene el nombrel del programa correspondiente
      LET v_c_programa_cod = fn_obten_nom_programa(m_i_proceso_cod , m_i_opera_cod)

      -- se crea el nombre del reporte
      LET v_v_nom_reporte = m_v_usuario CLIPPED,"-",
                            v_c_programa_cod CLIPPED,"-",
                            m_d_pid USING "&&&&&","-",
                            m_i_proceso_cod USING "&&&&&","-",
                            m_i_opera_cod USING "&&&&&",".pdf"

      -- se indica en donde se guardará el reporte
      CALL fgl_report_setOutputFileName(v_c_ruta_listado CLIPPED||"/"||v_v_nom_reporte)

      -- sin indica que no es necesario el preview
      CALL fgl_report_selectPreview(0)

      -- se asigna la configuración en el menejador del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()
   ELSE
      DISPLAY "ERROR: No fue posible abrir la plantilla del reporte"

      EXIT PROGRAM
   END IF

   -- se inicia el reporte
   START REPORT reporte_archivo_salida TO XML HANDLER v_manejador_rpt

   -- se inicializan variables
   LET v_i_cont_regs_arch = 0

   DISPLAY " SE CREA ARCHIVO DE EXTRACCIÓN"

   -- se crea el manejador de archivo
   LET v_ch_arch_sql_unl = base.Channel.create()

   -- se crea la ruta del archivo
   LET v_v_ruta_nomarch_sql = v_c_ruta_envio CLIPPED || "/extr_cred_unl.sql"

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_sql_unl.openFile(v_v_ruta_nomarch_sql, "w" )
   CALL v_ch_arch_sql_unl.setDelimiter("")

   -- se crea la ruta de archivo de salida (no depurado)
   LET v_v_ruta_nomarch_pre = v_c_ruta_envio CLIPPED || "/extr_cred_pre.aec"

   -- se escribe el registro (detalle) en el archivo
   LET v_s_registro =  "UNLOAD TO ",v_v_ruta_nomarch_pre CLIPPED
   CALL v_ch_arch_sql_unl.write([v_s_registro])
   CALL v_ch_arch_sql_unl.write([" SELECT nss,"])
   CALL v_ch_arch_sql_unl.write(["        TO_CHAR(tpo_credito,'&&&'),"])
   CALL v_ch_arch_sql_unl.write(["        TO_CHAR(num_credito,'&&&&&&&&&&'),"])
   CALL v_ch_arch_sql_unl.write(["        TO_CHAR(f_ini_credito,'%Y%m%d'),"])
   CALL v_ch_arch_sql_unl.write(["        NVL(TO_CHAR(f_liq_credito,'%Y%m%d'),'        '),"])
   CALL v_ch_arch_sql_unl.write(["        marca_safre_saci,"])
   CALL v_ch_arch_sql_unl.write(["        marca_procesar,"])
   CALL v_ch_arch_sql_unl.write(["        RPAD(TRIM(NVL(rfc,'*************')),13,'*') ,"]) 
   CALL v_ch_arch_sql_unl.write(["        RPAD(TRIM(NVL(curp,'******************')),18,'*') ,"])
   CALL v_ch_arch_sql_unl.write(["        RPAD(TRIM(paterno),40,'*') ,"])
   CALL v_ch_arch_sql_unl.write(["        RPAD(TRIM(materno),40,'*') ,"])
   CALL v_ch_arch_sql_unl.write(["        RPAD(TRIM(nombre),40,'*') ,"])
   CALL v_ch_arch_sql_unl.write(["        TO_CHAR(saldo_ssv97*100,'&&&&&&&&&&&&&&&'),"])
   CALL v_ch_arch_sql_unl.write(["        TO_CHAR(saldo_ssv92*100,'&&&&&&&&&&&&&&&')"])
   CALL v_ch_arch_sql_unl.write(["   FROM safre_tmp:tmp_extrac_acred"])
   
   -- se cierra el manejador de lectura
   CALL v_ch_arch_sql_unl.close()

   DISPLAY " ARCHIVO GENERADO ",m_v_arch_proceso
   -- se crea comando que elimina el delimitador
   LET v_s_comando = "dbaccess safre_viv ",v_v_ruta_nomarch_sql

   -- se ejecuta el comando armado
   RUN v_s_comando

   IF STATUS THEN
      DISPLAY "ERROR EN LA GENERACIÓN DEL ARCHIVO: ",v_s_comando
      -- se invoca la función que deja la operación en estado ERRONEA
      LET r_b_valida = fn_error_opera(m_d_pid, m_i_proceso_cod, m_i_opera_cod)

      -- se verifica si fue posible marcar la operacion como Erronea
      IF r_b_valida <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF
--JEVB (se asigno una variable "v_arch_aux" para concatenar la ruta de envio al nombre del archivo "arch_aux.aec" 22/01/2016)
   LET v_arch_aux = v_c_ruta_envio CLIPPED || "/arch_aux.aec"
   -- Se cambian los asteriscos por espacios 
   LET v_s_comando = "sed 's/*/ /g' " || v_v_ruta_nomarch_pre || " > "|| v_arch_aux
   RUN v_s_comando

   -- Se opia el contenido del archivo auxiliar al original 
   LET v_s_comando = "mv  " ||v_arch_aux||" "|| v_v_ruta_nomarch_pre
   RUN v_s_comando
{
   -- Se elimina el archivo auxiliar
   LET v_s_comando = "rm "|| v_arch_aux
   RUN v_s_comando
   }
--JEVB
   DISPLAY " SE DEPURA ARCHIVO DE EXTRACCIÓN"
   -- se crea comando que elimina el delimitador
   LET v_s_comando = "sed 's/|//g' ",v_v_ruta_nomarch_pre," > ",v_v_ruta_nomarch_sal

   -- se ejecuta el comando armado
   RUN v_s_comando

   IF STATUS THEN
      DISPLAY "ERROR EN LA GENERACIÓN DEL ARCHIVO: ",v_s_comando
      -- se invoca la función que deja la operación en estado ERRONEA
      LET r_b_valida = fn_error_opera(m_d_pid, m_i_proceso_cod, m_i_opera_cod)

      -- se verifica si fue posible marcar la operacion como Erronea
      IF r_b_valida <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF

   -- se obtienen los importes de los creditos vigentes
   LET v_s_qryTxt = " SELECT SUM(saldo_ssv92), SUM(saldo_ssv97), COUNT(*)\n",
                    "   FROM safre_tmp:tmp_extrac_acred\n",
                    "  WHERE estado_cred = 1"

   PREPARE prp_extra_vigentes FROM v_s_qryTxt
   EXECUTE prp_extra_vigentes INTO v_total_ssv92, v_total_ssv97, v_i_cont_regs_arch

   -- salida del reporte
   OUTPUT TO REPORT reporte_archivo_salida(v_total_ssv92,v_total_ssv97, v_i_cont_regs_arch, 1)

   LET v_i_cont_regs_arch = 0

   -- se obtienen los importes de los creditos liquidados
   LET v_s_qryTxt = " SELECT SUM(saldo_ssv92), SUM(saldo_ssv97), COUNT(*)\n",
                    "   FROM safre_tmp:tmp_extrac_acred\n",
                    "  WHERE estado_cred = 2"

   PREPARE prp_extra_liquidados FROM v_s_qryTxt
   EXECUTE prp_extra_liquidados INTO v_total_ssv92, v_total_ssv97, v_i_cont_regs_arch

   -- salida del reporte
   OUTPUT TO REPORT reporte_archivo_salida(v_total_ssv92,v_total_ssv97, v_i_cont_regs_arch, 2)

   LET v_i_cont_regs_arch = 0

   -- se obtienen los importes de los creditos cancelados
   LET v_s_qryTxt = " SELECT SUM(saldo_ssv92), SUM(saldo_ssv97), COUNT(*)\n",
                    "   FROM safre_tmp:tmp_extrac_acred\n",
                    "  WHERE estado_cred = 5"

   PREPARE prp_extra_cancelados FROM v_s_qryTxt
   EXECUTE prp_extra_cancelados INTO v_total_ssv92, v_total_ssv97, v_i_cont_regs_arch

   -- salida del reporte
   OUTPUT TO REPORT reporte_archivo_salida(v_total_ssv92,v_total_ssv97, v_i_cont_regs_arch, 5)

   LET v_i_cont_regs_arch = 0

----

   --Finaliza el reporte
   FINISH REPORT reporte_archivo_salida

   -- se cierra el manejador de lectura
   #CALL v_ch_arch_salida.close()

   -- se invoca la función que deja la operación en estado Finalizado
   LET r_b_valida = fn_actualiza_opera_fin(m_d_pid, m_i_proceso_cod, m_i_opera_cod)

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      --EXIT PROGRAM
   END IF

   DISPLAY "=FIN="

END MAIN

#Objetivo: Genera el reporte de Rechazos
REPORT reporte_archivo_salida(p_suma_ssv92, p_suma_ssv97, p_count_reg, p_tpo_credito)

   DEFINE p_suma_ssv92    DECIMAL(18,2)
   DEFINE p_suma_ssv97    DECIMAL(18,2)
   DEFINE p_count_reg     INTEGER
   DEFINE p_tpo_credito   SMALLINT
   DEFINE v_tpo_credito   VARCHAR(10)
   DEFINE v_total_ssv92   DECIMAL(18,2)
   DEFINE v_total_ssv97   DECIMAL(18,2)
   DEFINE v_total_regs    INTEGER
   DEFINE v_fecha_reporte DATE

   FORMAT

   FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY

      PRINTX m_v_usuario
      PRINTX m_d_folio
      PRINTX v_fecha_reporte USING "dd-mm-yyyy"

   ON EVERY ROW
      IF p_tpo_credito = 1 THEN
         LET v_tpo_credito = "VIGENTE"
      ELSE
         IF p_tpo_credito = 2 THEN
            LET v_tpo_credito = "LIQUIDADO"
         ELSE
            LET v_tpo_credito = "CANCELADO"
         END IF
      END IF

      PRINTX m_v_arch_proceso
      PRINTX p_suma_ssv92
      PRINTX p_suma_ssv97
      PRINTX p_count_reg
      PRINTX v_tpo_credito

   ON LAST ROW
      LET v_total_ssv92 = SUM(p_suma_ssv92)
      LET v_total_ssv97 = SUM(p_suma_ssv97)
      LET v_total_regs = SUM(p_count_reg)

      PRINTX v_total_ssv92
      PRINTX v_total_ssv97
      PRINTX v_total_regs

END REPORT
