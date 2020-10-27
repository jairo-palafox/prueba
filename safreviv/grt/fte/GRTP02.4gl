--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

#########################################################################
#Modulo:           =>GRT                                                #
#Programa:         =>GRTP02                                             #
#Objetivo:         =>Programa para generar deudor y marcas de los       #
#                    registros aceptados en el paso de integración de   #
#                    recurrente del módulo Solic. de Saldo en Garantía  #
#Autor:            =>Daniel Buendia, EFP                                #
#Fecha inicio:     =>21 Abril 2012                                      #
#########################################################################

DATABASE safre_viv

MAIN
   DEFINE p_v_usuario         LIKE seg_usuario.usuario, -- nombre del usuario
          p_d_pid             LIKE bat_ctr_proceso.pid, -- pid
          p_i_proceso_cod     LIKE cat_proceso.proceso_cod, -- codigo del proceso
          p_i_opera_cod       LIKE cat_operacion.opera_cod, -- codigo de la operacion
          p_d_folio           LIKE cre_ctr_archivo.folio_archivo, -- numero de folio
          p_v_arch_proceso    VARCHAR(100), -- nombre del archivo a integrar
          p_d_id_cre_ctr_arch LIKE cre_ctr_archivo.id_cre_ctr_archivo, -- identificador de la tabla de control
          v_c_programa_cod    LIKE bat_ctr_operacion.programa_cod, -- nombre del programa
          r_c_ruta_bin_cta    LIKE seg_modulo.ruta_bin, -- ruta bin cta
          r_c_ruta_list_cta   LIKE seg_modulo.ruta_bin, -- ruta listados cta
          v_c_ruta_list_bat   LIKE seg_modulo.ruta_listados, -- ruta listados de bat
          v_r_cre_acreditado  RECORD LIKE cre_acreditado.*, -- registro de cre acreditado
          v_r_cta_marca_ws    RECORD LIKE cta_marca_ws.*, -- registro de la tabla de Web Service
          v_i_marca_ws        LIKE cat_tipo_credito.marca_prc, -- marca del web service
          v_i_tot_regs        INTEGER, -- total de registros marcados
          v_i_edo_marcaje     SMALLINT, -- estado del marcaje
          v_s_comando         STRING, -- contiene al comando a correr
          v_s_qryTxt          STRING, -- guarda una sentencia SQL a ejecutar
          r_b_valida          SMALLINT -- booleana que indica si el proceso se puede ejecutar o no
        
   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario         = ARG_VAL(1)
   LET p_d_pid             = ARG_VAL(2)
   LET p_i_proceso_cod     = ARG_VAL(3)
   LET p_i_opera_cod       = ARG_VAL(4)
   LET p_d_folio           = ARG_VAL(5)
   LET p_v_arch_proceso    = ARG_VAL(6)
   LET p_d_id_cre_ctr_arch = ARG_VAL(7)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".GRTP02.log")

   DISPLAY "=INICIA GRTP02="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"
   DISPLAY " ARCHIVO:      : ",p_v_arch_proceso
   DISPLAY " ID CTR ARCHIVO: ",p_d_id_cre_ctr_arch

   -- se inicializan variables
   LET v_c_programa_cod = "GRTP02"

   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(p_d_pid,p_i_proceso_cod,p_i_opera_cod) RETURNING r_b_valida

   -- se verifica si la operación fue o no valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se invoca la función que deja la operación en estado Procesando
   LET r_b_valida = fn_actualiza_opera_ini(p_d_pid,p_i_proceso_cod,p_i_opera_cod,
                                           p_d_folio, v_c_programa_cod,
                                           p_v_arch_proceso, p_v_usuario)

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

{ Ya no se inserta en cta credito en este proceso, esto se hará cuando llegue confirmación de saldos
   DISPLAY " INSERTA CTA CRÉDITO"
   -- se crea sentencia que ejecuta PROCEDURE que inserta los registros integrados en cta crédito
   LET v_s_qryTxt = "EXECUTE PROCEDURE sp_inserta_cta_credito(?,?)"

   PREPARE prp_insrt_cta_credito FROM v_s_qryTxt
   EXECUTE prp_insrt_cta_credito USING p_i_proceso_cod, p_d_id_cre_ctr_arch
}

   DISPLAY " SE PROCESA LA MARCA"
   -- se crea sentencia que ejecuta PROCEDURE que inserta los registros integrados en cta crédito
   LET v_s_qryTxt = "EXECUTE FUNCTION fn_procesa_marca_cuenta(?,?,?,?)"

   PREPARE prp_procesa_marca_cuenta FROM v_s_qryTxt
   EXECUTE prp_procesa_marca_cuenta USING p_v_usuario,
                                          p_d_folio,
                                          p_d_id_cre_ctr_arch,
                                          p_i_proceso_cod
                                     INTO v_i_edo_marcaje

   -- verifica si ocurrió un error durante el proceos de marcaje
   IF v_i_edo_marcaje <> 0 THEN
      -- Ocurrió un error, se muestra el error
      DISPLAY "OCURRIÓ UN ERROR EN EL PROCESO DE MARCAJE: ",v_i_edo_marcaje

      -- ocurrió un error y se marca como rechazado la operación
      LET r_b_valida = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

      EXIT PROGRAM
   END IF

{
   -- se realiza el conteo de los registros marcados
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM sfr_marca_activa\n",
                    "  WHERE id_derechohabiente IN (\n",
                    "        SELECT id_derechohabiente\n",
                    "          FROM cre_acreditado\n",
                    "         WHERE id_cre_ctr_archivo = ",p_d_id_cre_ctr_arch,")"

   PREPARE prp_cuenta_marcas FROM v_s_qryTxt
   EXECUTE prp_cuenta_marcas INTO v_i_tot_regs

}

   -- se realiza el conteo de los registros marcados
   SELECT COUNT(*)
     INTO v_i_tot_regs
     FROM sfr_marca_activa s
    INNER JOIN cre_acreditado c
       ON s.id_derechohabiente = c.id_derechohabiente
      AND s.n_referencia       = c.id_cre_acreditado
      AND c.id_cre_ctr_archivo = p_d_id_cre_ctr_arch
    INNER JOIN cat_tipo_credito t
       ON s.marca = t.marca_inf

   DISPLAY "TOTAL DE REGISTROS MARCADOS: ",v_i_tot_regs

   -- se consultan los registros de cre acreditado
  {
   LET v_s_qryTxt = " SELECT *\n",
                    "   FROM cre_acreditado\n",
                    "  WHERE id_cre_ctr_archivo = ",p_d_id_cre_ctr_arch
   }

   LET v_s_qryTxt = " SELECT c.*, a.nss\n",
                    "   FROM cre_acreditado c, afi_derechohabiente a\n",
                    "  WHERE c.id_cre_ctr_archivo = ",p_d_id_cre_ctr_arch,"\n",
                    "    AND c.estado IN(20,152)\n",
                    "    AND c.id_derechohabiente = a.id_derechohabiente"

   PREPARE prp_cre_acreditado FROM v_s_qryTxt
   DECLARE cur_cre_acreditado CURSOR FOR prp_cre_acreditado

   -- se cre sentencia que obtiene la marca correspondiente al tipo de credito
   LET v_s_qryTxt = " SELECT FIRST 1 marca_prc\n",
                    "   FROM cat_tipo_credito\n",
                    "  WHERE tpo_credito = ?\n",
                    "  ORDER BY f_actualiza DESC"

   PREPARE prp_slctFrst_marcaPrc FROM v_s_qryTxt

   FOREACH cur_cre_acreditado INTO v_r_cre_acreditado.*
      EXECUTE prp_slctFrst_marcaPrc USING v_r_cre_acreditado.tpo_credito
                                     INTO v_i_marca_ws

      -- se verifica si ya existe el registro en cta marca ws
      SELECT COUNT(*)
        INTO v_i_tot_regs
        FROM cta_marca_ws
       WHERE id_derechohabiente = v_r_cre_acreditado.id_derechohabiente

      -- se verifica si se encontraron registros con el mismo derechohabiente
      IF v_i_tot_regs > 0 THEN
         -- se elimina el registro existente
         DELETE
           FROM cta_marca_ws
          WHERE id_derechohabiente = v_r_cre_acreditado.id_derechohabiente
      END IF

      -- se asignan los valores del registro a insertar en la tabla de Web services
      LET v_r_cta_marca_ws.id_derechohabiente = v_r_cre_acreditado.id_derechohabiente
      LET v_r_cta_marca_ws.id_origen          = v_r_cre_acreditado.id_cre_acreditado
      LET v_r_cta_marca_ws.modulo_cod         = "16" -- GRT
      LET v_r_cta_marca_ws.tpo_credito        = v_r_cre_acreditado.tpo_credito
      LET v_r_cta_marca_ws.marca              = v_i_marca_ws
      LET v_r_cta_marca_ws.f_solicita         = TODAY
      LET v_r_cta_marca_ws.intento            = 1
      LET v_r_cta_marca_ws.cod_result_op      = NULL
      LET v_r_cta_marca_ws.diagnostico        = NULL
      LET v_r_cta_marca_ws.situacion          = 2
      LET v_r_cta_marca_ws.num_credito        = v_r_cre_acreditado.num_credito
      LET v_r_cta_marca_ws.f_infonavit        = v_r_cre_acreditado.f_otorga
      LET v_r_cta_marca_ws.marca_procesar     = "02" -- 'grt' => 02 (Créditos en Garantía)
      LET v_r_cta_marca_ws.folio_archivo      = p_d_folio
      LET v_r_cta_marca_ws.usuario            = p_v_usuario

      -- se guarda el registro en la tabla del web service (cta marca ws)
      INSERT INTO cta_marca_ws VALUES (v_r_cta_marca_ws.*)
   END FOREACH

{  -- Esta parte ya no se ejecuta (2012/10/15) ya que se creo un programa de carga de marca/desmarca
   -- se obtiene la ruta bin y de listados del módulo
   CALL fn_rutas("cta") RETURNING r_c_ruta_bin_cta, r_c_ruta_list_cta

   -- se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    " WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat INTO v_c_ruta_list_bat

   -- se crea el comando que ejecuta el modulo de marcaje
   LET v_s_comando = " fglrun ",r_c_ruta_bin_cta CLIPPED,"/CTAW10 ",
                                           p_i_proceso_cod," 1>> ",
                                           v_c_ruta_list_bat CLIPPED,
                                           "/nohup:",p_d_pid USING "&&&&&",":",
                                           p_i_proceso_cod USING "&&&&&",":",
                                           p_i_opera_cod USING "&&&&&",
                                           " 2>&1"

   --DISPLAY v_s_comando
   RUN v_s_comando
}
   -- se invoca la función que deja la operación en estado Finalizado
   LET r_b_valida = fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      -- ocurrió un error y se marca como rechazado la operación
      LET r_b_valida = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

      --EXIT PROGRAM
   END IF

   DISPLAY "=FIN="

   END MAIN
