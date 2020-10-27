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

   LET v_c_ruta_envio = "/safreviv_int/agr/envio"
   -- se crea el manejador de archivo
   LET v_ch_arch_sql_unl = base.Channel.create()

   -- se crea la ruta del archivo
   LET v_v_ruta_nomarch_sql = v_c_ruta_envio CLIPPED || "/extr_cred_unl_PRUEBA.sql"

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_sql_unl.openFile(v_v_ruta_nomarch_sql, "w" )
   CALL v_ch_arch_sql_unl.setDelimiter("")

   -- se crea la ruta de archivo de salida (no depurado)
   LET v_v_ruta_nomarch_pre = v_c_ruta_envio CLIPPED || "/extr_cred_prePRUEBA.aec"

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

   LET v_s_comando = "sed 's/*/ /g' " || v_v_ruta_nomarch_pre || " > arch_aux.aec"
   RUN v_s_comando

   LET v_s_comando = "mv arch_aux.aec " || v_v_ruta_nomarch_pre
   RUN v_s_comando

   LET v_s_comando = "rm arch_aux.aec"
   RUN v_s_comando
{
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
}
END MAIN
