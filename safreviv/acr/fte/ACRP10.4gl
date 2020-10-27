--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

#########################################################################
#Modulo:           =>ACR                                                #
#Programa:         =>ACRP10                                             #
#Objetivo:         =>Programa para generar deudor y marcas de los       #
#                    registros aceptados en el paso de integraci�n de   #
#                    recurrente del m�dulo Transferencia de Acreditados #
#Autor:            =>Daniel Buendia, EFP                                #
#Fecha inicio:     =>26 Marzo 2012                                      #
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
         r_c_ruta_bin_cta    LIKE seg_modulo.ruta_bin, -- ruta del bin de cta
         r_c_ruta_list_cta   LIKE seg_modulo.ruta_bin, -- ruta listados cta
         v_c_ruta_list_bat   LIKE seg_modulo.ruta_listados, -- ruta listados de bat
         v_r_cre_acreditado  RECORD LIKE cre_acreditado.*, -- registro de cre acreditado
         v_r_cta_marca_ws    RECORD LIKE cta_marca_ws.*, -- registro del web service
         v_i_marca_ws        LIKE cat_tipo_credito.marca_prc, -- marca procesar
         v_i_tot_regs        INTEGER, -- total de registros marcados
         v_i_edo_marcaje     SMALLINT,
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
   CALL STARTLOG(p_v_usuario CLIPPED|| ".ACRP10.log")

   DISPLAY "=INICIA ACRP10="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"
   DISPLAY " ARCHIVO:      : ",p_v_arch_proceso
   DISPLAY " ID CTR ARCHIVO: ",p_d_id_cre_ctr_arch

   -- se inicializan variables
   LET v_c_programa_cod = "ACRP10"

   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(p_d_pid,p_i_proceso_cod,p_i_opera_cod) RETURNING r_b_valida

   -- se verifica si la operaci�n fue o no valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se invoca la funci�n que deja la operaci�n en estado Procesando
   LET r_b_valida = fn_actualiza_opera_ini(p_d_pid,p_i_proceso_cod,p_i_opera_cod,
                                           p_d_folio, v_c_programa_cod,
                                           p_v_arch_proceso, p_v_usuario)

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   DISPLAY " INSERTA CTA CR�DITO"
   -- se crea sentencia que ejecuta PROCEDURE que inserta los registros integrados en cta cr�dito
   LET v_s_qryTxt = "EXECUTE PROCEDURE safre_viv:sp_inserta_cta_credito(?,?)"

   PREPARE prp_insrt_ctaCredito FROM v_s_qryTxt
   EXECUTE prp_insrt_ctaCredito USING p_i_proceso_cod, p_d_id_cre_ctr_arch

   DISPLAY " SE PROCESA LA MARCA"
   -- se crea sentencia que ejecuta PROCEDURE que inserta los registros integrados en cta cr�dito
   LET v_s_qryTxt = "EXECUTE FUNCTION safre_viv:fn_procesa_marca_cuenta(?,?,?,?)"

   PREPARE prp_procesa_marca_cuenta FROM v_s_qryTxt
   EXECUTE prp_procesa_marca_cuenta USING p_v_usuario,
                                          p_d_folio,
                                          p_d_id_cre_ctr_arch,
                                          p_i_proceso_cod
                                     INTO v_i_edo_marcaje

   -- verifica si ocurri� un error durante el proceos de marcaje
   IF v_i_edo_marcaje <> 0 THEN
      -- Ocurri� un error, se muestra el error
      DISPLAY "OCURRI� UN ERROR EN EL PROCESO DE MARCAJE: ",v_i_edo_marcaje

      -- ocurri� un error y se marca como rechazado la operaci�n
      LET r_b_valida = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

      EXIT PROGRAM
   END IF

   -- se leen  los registro integrados para insertarlos en la tabla relacionada con el WS
   LET v_s_qryTxt = " SELECT *\n",
                    "   FROM cre_acreditado\n",
                    "  WHERE id_cre_ctr_archivo = ",p_d_id_cre_ctr_arch

   PREPARE prp_selct_creAcred FROM v_s_qryTxt
   DECLARE cur_selct_creAcred CURSOR FOR prp_selct_creAcred

   -- se cre sentencia que obtiene la marca correspondiente al tipo de credito
   LET v_s_qryTxt = " SELECT FIRST 1 marca_prc\n",
                    "   FROM cat_tipo_credito\n",
                    "  WHERE tpo_credito = ?\n",
                    "    AND f_actualiza <= ?\n",
                    "  ORDER BY f_actualiza DESC"

   PREPARE prp_slctFrst_marcaPrc FROM v_s_qryTxt

   FOREACH cur_selct_creAcred INTO v_r_cre_acreditado.*
      EXECUTE prp_slctFrst_marcaPrc USING v_r_cre_acreditado.tpo_credito, v_r_cre_acreditado.f_otorga
                                     INTO v_i_marca_ws

      -- se verifica si ya existe el registro en cta marca ws
      SELECT COUNT(*)
        INTO v_i_tot_regs
        FROM cta_marca_ws
       WHERE id_derechohabiente = v_r_cre_acreditado.id_derechohabiente

      -- se verifica si se encontraron registros con el mismo derechohabiente
      IF v_i_tot_regs > 0 THEN
         -- se elimina el registro existente
         DELETE FROM cta_marca_ws WHERE id_derechohabiente = v_r_cre_acreditado.id_derechohabiente
      END IF

      -- se asignan los valores del registro a insertar en la tabla de Web services
      LET v_r_cta_marca_ws.id_derechohabiente = v_r_cre_acreditado.id_derechohabiente
      LET v_r_cta_marca_ws.id_origen          = v_r_cre_acreditado.id_cre_acreditado
      LET v_r_cta_marca_ws.modulo_cod         = "03" -- ACR
      LET v_r_cta_marca_ws.tpo_credito        = v_r_cre_acreditado.tpo_credito
      LET v_r_cta_marca_ws.marca              = v_i_marca_ws
      LET v_r_cta_marca_ws.f_solicita         = TODAY
      LET v_r_cta_marca_ws.intento            = 1
      LET v_r_cta_marca_ws.cod_result_op      = NULL
      LET v_r_cta_marca_ws.diagnostico        = NULL
      LET v_r_cta_marca_ws.situacion          = 1
      LET v_r_cta_marca_ws.num_credito        = v_r_cre_acreditado.num_credito
      LET v_r_cta_marca_ws.f_infonavit        = v_r_cre_acreditado.f_otorga
      LET v_r_cta_marca_ws.marca_procesar     = "01" -- 'acr' => 01 (Cr�dito Tradicional)
      LET v_r_cta_marca_ws.folio_archivo      = p_d_folio
      LET v_r_cta_marca_ws.usuario            = p_v_usuario

      -- se guarda el registro en la tabla del web service (cta marca ws)
     INSERT INTO cta_marca_ws VALUES (v_r_cta_marca_ws.*)
   END FOREACH

   -- se invoca la funci�n que deja la operaci�n en estado Finalizado
   LET r_b_valida = fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      -- ocurri� un error y se marca como rechazado la operaci�n
      LET r_b_valida = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

      --EXIT PROGRAM
   END IF
   DISPLAY "=FIN="
END MAIN
