--===============================================================
-- Versión: 1.0.
-- Fecha ultima modificación:
--===============================================================

##########################################################################
#Modulo:           => GRT                                                #
#Programa:         => GRTP32                                             #
#Objetivo:         => Programa para solicitud desmarca créditos          #
#                     garantía 43bis                                     #
#Autor:            => Mauro Muñiz Caballero, EFP                         #
#Fecha inicio:     => 8 de noviembre de 2016                             #
#Modifica          => Edgar Damian Estrada Rivera                        #
#Fecha Mod.        => 10 de julio de 2017                                #
##########################################################################
DATABASE safre_viv

MAIN

   DEFINE p_v_usuario               LIKE seg_usuario.usuario -- nombre del usuario
   DEFINE p_d_pid                   LIKE bat_ctr_proceso.pid -- pid
   DEFINE p_i_proceso_cod           LIKE cat_proceso.proceso_cod -- código del proceso
   DEFINE p_i_opera_cod             LIKE cat_operacion.opera_cod -- código de la operación
   DEFINE p_d_folio                 LIKE cre_ctr_archivo.folio_archivo -- numero de folio
   DEFINE p_v_arch_proceso          VARCHAR(100) -- nombre del archivo a integrar
   DEFINE p_d_id_cre_ctr_arch       LIKE cre_ctr_archivo.id_cre_ctr_archivo -- identificador de la tabla de control
   DEFINE v_c_programa_cod          LIKE bat_ctr_operacion.programa_cod -- nombre del programa
   DEFINE v_i_edo_marcaje           SMALLINT -- estado del marcaje
   DEFINE v_s_qryTxt                STRING -- guarda una sentencia SQL a ejecutar
   DEFINE r_b_valida                SMALLINT -- booleana que indica si el proceso se puede ejecutar o no
   DEFINE v_id_cre_ctr_archivo      DECIMAL(9,0) --valor de inserción
   DEFINE v_folio_archivo           DECIMAL(9,0) -- valor de inserción
   DEFINE v_execute_fn              STRING  -- variable para iniciar la ejecucion de fn_verifica_id_archivo_grt
   DEFINE v_query                   STRING -- variable para guardar cadena para hacer query (actualiza tablas)
   DEFINE cont                      INTEGER -- contador para valor de f_formalización
   DEFINE cont_reg                  INTEGER -- almacena valor de registro
   DEFINE v_query_sit               STRING -- variable que guarda la sentencia para contar registro situacion 60
   DEFINE v_query_proc              STRING -- variable que guarda la sentencia para ejecutar fn_procesa_marca_cuenta
   DEFINE v_aceptada                INTEGER -- variable del estado retorno "aceptado"
   DEFINE v_rechazada               INTEGER -- variable del estado retorno "rechazado"
   DEFINE p_i_tpo_originacion       SMALLINT 
   DEFINE v_ax_error                SMALLINT
   DEFINE v_det_sit                 INTEGER -- variable que determina la nueva situación
   DEFINE v_cont_liq                INTEGER -- contador para cambio de situación ocg liquidación
   DEFINE v_cont_flz                INTEGER -- contador para cambio de situación ocg_formalización
   DEFINE v_cont_tram               INTEGER -- contador para cambio de situación ocg tramite
   DEFINE v_cont_acr                INTEGER -- contador para cambio de situación ocg acreditado
   DEFINE v_id_cre_acreditado     DECIMAL (9,0) -- variable que retorna el id_acreditado de la función
   
   DEFINE v_arr_ocg RECORD -- Datos de registro
      id_ocg_liquidacion          DECIMAL(9,0),
      id_derechohabiente          DECIMAL(9,0),
      id_ocg_formalizacion        DECIMAL(9,0),
      id_ocg_detalle              DECIMAL(9,0),
      id_ocg_tramite              DECIMAL(9,0),
      f_liquida_credito           DATE
   END RECORD

   -- se recuperan los parámetros que envía el programa lanzador
   LET p_v_usuario         = ARG_VAL(1)
   LET p_d_pid             = ARG_VAL(2)
   LET p_i_proceso_cod     = ARG_VAL(3)
   LET p_i_opera_cod       = ARG_VAL(4)
   LET p_d_folio           = ARG_VAL(5)
   LET p_v_arch_proceso    = ARG_VAL(6)
   LET p_d_id_cre_ctr_arch = ARG_VAL(7)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".GRTP32.log")

   DISPLAY " = INICIA GRTP32 = "
   DISPLAY " USUARIO       : " ,p_v_usuario
   DISPLAY " PID           : " ,p_d_pid
   DISPLAY " PROCESO       : " ,p_i_proceso_cod
   DISPLAY " OP_COD        : " ,p_i_opera_cod
   DISPLAY " FOLIO         : " ,p_d_folio USING "#########&"
   DISPLAY " ARCHIVO       : " ,p_v_arch_proceso
   DISPLAY " ID CTR ARCHIVO: " ,p_d_id_cre_ctr_arch

   -- se inicializan variables
   LET v_c_programa_cod = "GRTP32"
   LET v_det_sit        =  150
   LET v_aceptada   = 0
   LET v_rechazada  = 0
   LET p_i_tpo_originacion = 2

   -- se invoca la función que deja la operación en estado Procesando
   LET r_b_valida = fn_actualiza_opera_ini( p_d_pid,
                                            p_i_proceso_cod,
                                            p_i_opera_cod,
                                            p_d_folio,
                                            v_c_programa_cod,
                                            p_v_arch_proceso,
                                            p_v_usuario )
      DISPLAY "VALIDA", r_b_valida

   -- se verifica si fue posible finalizar la operación
   IF r_b_valida <> 0 THEN
   -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)
      EXIT PROGRAM
   END IF

   -- contador para el registro situación 140
   LET v_query_sit = "SELECT count(*)          ",
                     " FROM ocg_liquidacion l, ",
                     "      ocg_detalle d      ",
                     " WHERE l.situacion = 140 ",
                     " AND l.id_ocg_detalle  =  d.id_ocg_detalle",
                     " AND d.subproceso = 5    "

   PREPARE prp_count_sit140 FROM v_query_sit
   EXECUTE prp_count_sit140 INTO cont_reg

      DISPLAY "total de registros recuperados sit 140:",cont_reg

   -- recuperará registro de control de archivos.

   LET v_execute_fn = "EXECUTE FUNCTION fn_verifica_id_archivo_grt(?)"

   PREPARE prp_fn_verifica_archivo   FROM v_execute_fn
   EXECUTE prp_fn_verifica_archivo   USING p_i_proceso_cod
                                     INTO  v_id_cre_ctr_archivo,
                                           v_folio_archivo

       DISPLAY "PROCESO ",p_i_proceso_cod
       DISPLAY "ID_CRE_ARCH ",v_id_cre_ctr_archivo
       DISPLAY "FOLIO_ARCH ",v_folio_archivo

    LET v_query = "SELECT     l.id_ocg_liquidacion,   ",
                            " l.id_derechohabiente,   ",
                            " l.id_ocg_formalizacion, ",
                            " l.id_ocg_detalle,       ",
                            " l.id_ocg_tramite,       ",
                            " a.f_liquida_credito     ",
                    " FROM   ocg_liquidacion       l, ",
                            " ocg_formalizacion    f, ",
                            " ocg_detalle          d, ",
                            " ocg_acreditado       a  ",
                    " WHERE l.situacion             = 140 ",
                      " AND l.id_ocg_formalizacion  = f.id_ocg_formalizacion ",
                      " AND l.id_ocg_formalizacion  = a.id_ocg_formalizacion ",
                      " AND l.id_ocg_detalle        = d.id_ocg_detalle",
                      " AND d.subproceso            = 5"

   PREPARE prp_query FROM v_query
   DECLARE crs_query CURSOR FOR prp_query

   -- si el valor de fecha liquidación crédito es nulo asignar el valor TODAY
   LET cont = 1

   FOREACH crs_query INTO v_arr_ocg.*
      IF(v_arr_ocg.f_liquida_credito IS NULL) THEN
         LET v_arr_ocg.f_liquida_credito = TODAY
      END IF

         DISPLAY "FECHA_LIQUIDA ",v_arr_ocg.f_liquida_credito
         DISPLAY "ID_DERECHO_HABIENTE ", v_arr_ocg.id_derechohabiente
         DISPLAY "ID_LIQUIDA ",v_arr_ocg.id_ocg_liquidacion
         DISPLAY "ID_CRE_ARCH ",v_id_cre_ctr_archivo
         DISPLAY "FOLIO ",p_d_folio

      LET v_query_proc = "EXECUTE FUNCTION fn_grt_solicitud_desmarca(?,?,?,?,?,?)"

      PREPARE prp_procesa_sol_desmarca FROM v_query_proc
      EXECUTE prp_procesa_sol_desmarca USING p_v_usuario,
                                             p_d_folio,
                                             v_arr_ocg.id_derechohabiente,
                                             v_id_cre_ctr_archivo,
                                             v_arr_ocg.f_liquida_credito,
                                             v_arr_ocg.id_ocg_liquidacion
                                        INTO v_i_edo_marcaje,
                                             v_aceptada,
                                             v_rechazada,
                                             v_id_cre_acreditado

      IF v_aceptada = 1 THEN

         DISPLAY "SE SOLICITA DESMARCA",v_aceptada
         DISPLAY "id_ctrl_archivo ",v_id_cre_ctr_archivo
         DISPLAY "id_cre_archivo", v_id_cre_acreditado
         -- * desmarca cuenta
         -- * inserta registro en cta marca ws
         LET v_s_qryTxt = "EXECUTE FUNCTION fn_procesa_desmarca(?,?,?,?)"

         PREPARE prp_procesa_desmarca FROM v_s_qryTxt
         EXECUTE prp_procesa_desmarca USING p_v_usuario,
                                            p_d_folio,
                                            p_i_tpo_originacion,
                                            p_i_proceso_cod
                                        INTO v_ax_error

         IF v_ax_error <> 0 THEN
            -- en caso de error se muestra un mensaje a usuario y no continua
            DISPLAY "ERROR EN EL PROCESO DE DESMARCA: ",v_ax_error

            -- se invoca la función que deja la operación en estado ERRONEA
            LET r_b_valida = fn_error_opera(p_d_pid, 
                                            p_i_proceso_cod,
                                            p_i_opera_cod)
           -- CONTINUE FOREACH
         END IF
      END IF


   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)
    EXIT PROGRAM
   END IF

      -- se actualiza la información de OCG
      IF (v_arr_ocg.id_ocg_liquidacion IS NOT NULL) THEN
          UPDATE ocg_liquidacion
              SET situacion        = v_det_sit
          WHERE id_ocg_liquidacion = v_arr_ocg.id_ocg_liquidacion
              LET v_cont_liq = v_cont_liq + SQLCA.SQLERRD[3]
      END IF

      IF (v_arr_ocg.id_ocg_formalizacion IS NOT NULL) THEN
          UPDATE ocg_formalizacion
             SET situacion           = v_det_sit
          WHERE id_ocg_formalizacion = v_arr_ocg.id_ocg_formalizacion
              LET v_cont_flz = v_cont_flz + SQLCA.SQLERRD[3]
      END IF

      IF (v_arr_ocg.id_ocg_tramite IS NOT NULL) THEN
          UPDATE ocg_tramite
             SET situacion     = v_det_sit
          WHERE id_ocg_tramite = v_arr_ocg.id_ocg_tramite
              LET v_cont_tram  = v_cont_tram + SQLCA.SQLERRD[3]
      END IF

      IF (v_arr_ocg.id_ocg_formalizacion IS NOT NULL) THEN
          UPDATE ocg_acreditado
             SET situacion              = v_det_sit,
                 f_solic_desmarca_prcr  = TODAY
          WHERE id_ocg_formalizacion   = v_arr_ocg.id_ocg_formalizacion
              LET v_cont_acr = v_cont_acr + SQLCA.SQLERRD[3]
      END IF

     LET cont = cont + 1
    END FOREACH
     
   -- se invoca la función que deja la operación en estado Finalizado
   LET r_b_valida = fn_actualiza_opera_fin(p_d_pid,
                                           p_i_proceso_cod,
                                           p_i_opera_cod)

   -- se verifica si fue posible finalizar la operación
   IF r_b_valida <> 0 THEN

   -- en caso de error se muestra un mensaje a usuario y no continua
   CALL fn_desplega_inc_operacion(r_b_valida)

   -- ocurrió un error y se marca como rechazado la operación
   LET r_b_valida = fn_error_opera(p_d_pid,
                                      p_i_proceso_cod,
                                      p_i_opera_cod)

     EXIT PROGRAM
   END IF

   DISPLAY "=FIN="

END MAIN
