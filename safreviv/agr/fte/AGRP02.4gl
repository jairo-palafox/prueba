--===============================================================
-- Versión: 1.0.0
-- Fecha última modificación:
--===============================================================

###############################################################################
#Modulo:           => AGR                                                     #
#Programa:         => AGRP02                                                  #
#Objetivo:         => Programa para generar deudor y marcas de los            #
#                     registros aceptados en el paso de integración de        #
#                     recurrente del módulo Anualidades Garantizadas          #
#Autor:            => Daniel Buendia, EFP                                     #
#Fecha inicio:     => 24 Abril 2012                                           #
#Modifica:         => Mauro Muñiz Caballero                                   #
#Fecha modif:      => 26 de agosto de 2015                                    #
#                     Confirmación de solicitud de marca a Procesar           #
#                     de los créditos "Manos a la obra"                       #
#Modifica:         => Mauro Muñiz Caballero                                   #
#Fecha modif:      => 9 de noviembre de 2015                                  #
#Adecuación        => Eliminación de adelantos y adecuación con homologación  #
#                     de marcas (microflujo)                                  #
###############################################################################

DATABASE safre_viv

GLOBALS

  DEFINE p_v_usuario         LIKE seg_usuario.usuario -- nombre del usuario
  DEFINE p_d_pid             LIKE bat_ctr_proceso.pid -- pid
  DEFINE p_i_proceso_cod     LIKE cat_proceso.proceso_cod -- codigo del proceso
  DEFINE p_i_opera_cod       LIKE cat_operacion.opera_cod -- codigo de la operacion
  DEFINE p_d_folio           LIKE cre_ctr_archivo.folio_archivo -- numero de folio
  DEFINE p_v_arch_proceso    VARCHAR(100) -- nombre del archivo a integrar
  DEFINE p_d_id_cre_ctr_arch LIKE cre_ctr_archivo.id_cre_ctr_archivo -- identificador de la tabla de control
  DEFINE v_c_programa_cod    LIKE bat_ctr_operacion.programa_cod -- nombre del programa
  DEFINE r_c_ruta_bin_cta    LIKE seg_modulo.ruta_bin -- ruta del bin de cta
  DEFINE r_c_ruta_list_cta   LIKE seg_modulo.ruta_bin -- ruta listados cta
  DEFINE v_c_ruta_list_bat   LIKE seg_modulo.ruta_listados -- ruta listados de bat
  --DEFINE v_r_cre_acreditado  RECORD LIKE cre_acreditado.* -- registro de cre acreditado

  DEFINE v_r_cre_acreditado  RECORD
     id_cre_acreditado decimal(9,0),
     id_cre_ctr_archivo decimal(9,0),
     folio_liquida decimal(9,0),
     id_derechohabiente decimal(9,0),
     tpo_originacion smallint,
     tpo_credito smallint,
     tpo_registro char(2),
     num_credito decimal(10,0),
     sdo_deudor decimal(12,2),
     f_otorga date,
     f_culmina date,
     edo_credito smallint,
     tpo_dscto smallint,
     valor_dscto decimal(12,4),
     nrp char(11),
     f_ini_dscto date,
     nss_liberado char(11),
     f_gen_arh date,
     sdo_credito decimal(12,2),
     f_prox_liq date,
     f_desde date,
     f_hasta date,
     tpo_rch smallint,
     edo_procesar smallint,
     estado smallint
  END RECORD

  DEFINE v_r_cta_marca_ws    RECORD LIKE cta_marca_ws.* -- registro del web service
  DEFINE v_i_marca_ws        LIKE cat_tipo_credito.marca_prc -- marca procesar
  DEFINE v_si_id_deudor      LIKE cat_tipo_credito.id_deudor -- identificador de deudor
  DEFINE v_si_situacion      LIKE cta_marca_ws.situacion -- situación del WS
  DEFINE v_i_tot_regs        INTEGER -- total de registros marcados
  DEFINE v_i_edo_marcaje     SMALLINT
  DEFINE v_s_comando         STRING -- contiene al comando a correr
  DEFINE v_s_qryTxt          STRING -- guarda una sentencia SQL a ejecutar
  DEFINE r_b_valida          SMALLINT -- booleana que indica si el proceso se puede ejecutar o no
  DEFINE p_marca             SMALLINT
  DEFINE v_nss               CHAR(11)
  DEFINE v_marca             SMALLINT
  DEFINE v_f_marca           DATE

END GLOBALS

MAIN

   -- se recuperan los parámetros que envía el programa lanzador
   LET p_v_usuario         = ARG_VAL(1)
   LET p_d_pid             = ARG_VAL(2)
   LET p_i_proceso_cod     = ARG_VAL(3)
   LET p_i_opera_cod       = ARG_VAL(4)
   LET p_d_folio           = ARG_VAL(5)
   LET p_v_arch_proceso    = ARG_VAL(6)
   LET p_d_id_cre_ctr_arch = ARG_VAL(7)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".AGRP02.log")

   DISPLAY "=INICIA AGRP02="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"
   DISPLAY " ARCHIVO:      : ",p_v_arch_proceso
   DISPLAY " ID CTR ARCHIVO: ",p_d_id_cre_ctr_arch

   -- se inicializan variables
   LET v_c_programa_cod = "AGRP02"
   LET v_si_situacion   = 2
   LET p_marca          = 234

   -- se invoca la función que valida la operación
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

   DISPLAY " INSERTA CTA CRÉDITO"
   -- se crea sentencia que ejecuta PROCEDURE que inserta los registros integrados en cta crédito
   LET v_s_qryTxt = "EXECUTE PROCEDURE sp_inserta_cta_credito(?,?)"

   PREPARE prp_insrt_ctaCredito FROM v_s_qryTxt
   EXECUTE prp_insrt_ctaCredito USING p_i_proceso_cod, p_d_id_cre_ctr_arch

   DISPLAY " SE PROCESA LA MARCA"
   -- se crea sentencia que ejecuta PROCEDURE que inserta los registros integrados en cta crédito
   LET v_s_qryTxt = "EXECUTE FUNCTION fn_procesa_marca_cuenta(?,?,?,?)"

   PREPARE prp_procesa_marca_cuenta FROM v_s_qryTxt
   EXECUTE prp_procesa_marca_cuenta USING p_v_usuario,
                                          p_d_folio,
                                          p_d_id_cre_ctr_arch,
                                          p_i_proceso_cod
                                     INTO v_i_edo_marcaje

   DISPLAY "Estado de la ejecución de la función de marcaje : ",v_i_edo_marcaje

   -- verifica si ocurrió un error durante el proceos de marcaje
   IF v_i_edo_marcaje <> 0 THEN
      -- Ocurrió un error, se muestra el error
      DISPLAY "OCURRIÓ UN ERROR EN EL PROCESO DE MARCAJE: ",v_i_edo_marcaje

      -- ocurrió un error y se marca como rechazado la operación
      LET r_b_valida = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

      EXIT PROGRAM
   END IF

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

   -- se leen  los registros integrados para insertarlos en la tabla relacionada con el WS
   LET v_s_qryTxt = " SELECT c.*, a.nss\n",
                    "   FROM cre_acreditado c, afi_derechohabiente a\n",
                    "  WHERE c.id_cre_ctr_archivo = ",p_d_id_cre_ctr_arch,"\n",
                    "    AND c.estado IN(20,152)\n",
                    "    AND c.id_derechohabiente = a.id_derechohabiente"

   PREPARE prp_selct_creAcred FROM v_s_qryTxt
   DECLARE cur_selct_creAcred CURSOR FOR prp_selct_creAcred

   -- se cre sentencia que obtiene la marca correspondiente al tipo de credito
   LET v_s_qryTxt = " SELECT FIRST 1 id_deudor, marca_prc\n",
                    "   FROM cat_tipo_credito\n",
                    "  WHERE tpo_credito = ?\n",
                    "    AND f_actualiza <= ?\n",
                    "  ORDER BY f_actualiza DESC"

   PREPARE prp_slctFrst_marcaPrc FROM v_s_qryTxt

DISPLAY  v_r_cre_acreditado.*

   FOREACH cur_selct_creAcred INTO v_r_cre_acreditado.*, v_nss
      EXECUTE prp_slctFrst_marcaPrc USING v_r_cre_acreditado.tpo_credito, v_r_cre_acreditado.f_otorga
                                     INTO v_si_id_deudor, v_i_marca_ws

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

      -- se valida si el tipo de credito es diferente de 17-CRÉDITO ESTADOS MUNICIPIOS
      -- y diferente a 19-MEJORATIV
      -- si se incluye crédito 20-MANOS A LA OBRA
      IF v_r_cre_acreditado.tpo_credito <> 17 AND 
         v_r_cre_acreditado.tpo_credito <> 19 THEN

         LET v_s_qryTxt = "EXECUTE FUNCTION fn_verifica_marca(?,?)"

         PREPARE prp_verifica_marca FROM v_s_qryTxt
         EXECUTE prp_verifica_marca USING v_nss,
                                          p_marca
                                     INTO v_nss, v_marca, v_f_marca

         IF v_marca = 2 THEN
            -- se asignan los valores del registro a insertar en la tabla de Web services
            LET v_r_cta_marca_ws.id_derechohabiente = v_r_cre_acreditado.id_derechohabiente
            LET v_r_cta_marca_ws.id_origen          = v_r_cre_acreditado.id_cre_acreditado
            LET v_r_cta_marca_ws.modulo_cod         = "04" -- AGR
            LET v_r_cta_marca_ws.tpo_credito        = v_r_cre_acreditado.tpo_credito
            LET v_r_cta_marca_ws.marca              = v_i_marca_ws
            LET v_r_cta_marca_ws.f_solicita         = TODAY
            LET v_r_cta_marca_ws.intento            = 2
            LET v_r_cta_marca_ws.cod_result_op      = NULL
            LET v_r_cta_marca_ws.diagnostico        = NULL
            LET v_r_cta_marca_ws.situacion          = v_si_situacion
            LET v_r_cta_marca_ws.num_credito        = v_r_cre_acreditado.num_credito
            LET v_r_cta_marca_ws.f_infonavit        = v_r_cre_acreditado.f_otorga
            LET v_r_cta_marca_ws.marca_procesar     = "04" -- 'agr' => 04 (Anualidades Garantizadas)
            LET v_r_cta_marca_ws.folio_archivo      = p_d_folio
            LET v_r_cta_marca_ws.usuario            = p_v_usuario

            -- se guarda el registro en la tabla del web service (cta marca ws)
            INSERT INTO cta_marca_ws VALUES (v_r_cta_marca_ws.*)
         END IF
      END IF
   END FOREACH

   CLOSE cur_selct_creAcred
   FREE cur_selct_creAcred

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
