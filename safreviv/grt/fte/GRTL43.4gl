--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

################################################################################
#Modulo       => GRT                                                           #
#Programa     => GRTL43                                                        #
#Objetivo     => Programa que lanza la liquidacion de solicitud de saldo en    #
#                garantia del modulo creditos en garantia 43 bis               #
#Fecha inicio => Junio 20, 2012                                                #
#Autor        =>                                                               #
#Modificó     =>                                                               #
################################################################################

DATABASE safre_viv
GLOBALS "GRTG01.4gl"

GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
   DEFINE p_usuario_cod              LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
          p_tipo_ejecucion           SMALLINT, -- forma como ejecutara el programa
          p_s_titulo                 STRING, -- titulo de la ventana
          v_opcion_fun               SMALLINT, -- opcion para la funcion general        
          v_folio_liquida            LIKE cta_movimiento.folio_liquida,                    
          v_c_programa_cod           LIKE bat_ctr_operacion.programa_cod, -- nombre del programa
          v_s_qryTxt                 STRING, -- guarda una sentencia SQL a ejecutar        
          v_tpo_originacion          LIKE cre_acreditado.tpo_originacion, -- tipo de originación          
          v_v_nom_archivo            LIKE bat_ctr_operacion.nom_archivo, -- nombre del archivo
          v_i_cuenta_regs            INTEGER, -- contador de registros
          r_b_valida                 SMALLINT -- booleana que indica si el proceso se puede ejecutar o no
   DEFINE r_c_ruta_bin, r_ruta_listados STRING
   DEFINE v_c_ruta_list_bat    LIKE seg_modulo.ruta_listados -- ruta listados de bat

   -- se recuperan los valores enviados como parametro
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".GRTL43.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   -- se inicializan las variables
   LET g_proceso_cod         = g_proc_cod_grt_liquidacion -- liquidación solicitud de saldo en garantia
   LET g_opera_cod           = 2 -- preliquidación fc y/o av
   LET v_opcion_fun          = 2 -- ejecutar liquidacion
   LET v_folio_liquida       = 0   
   LET v_c_programa_cod      = "GRTL43"
   LET v_v_nom_archivo       = "NA"
   LET v_tpo_originacion     = 2 -- creditos en garantia

   {SELECT MAX(pid)
     INTO g_pid
     FROM bat_ctr_proceso
    WHERE proceso_cod = g_proceso_cod
      AND estado_cod = 2

   -- en caso de error continuar con el programa (en caso de no estar creada la tabla de preliquidación)
   WHENEVER ERROR CONTINUE

   -- se valida que exista información en la tabla de preliquidación
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM cre_sg_preliquida"

   PREPARE prp_cnt_sgPreliq FROM v_s_qryTxt
   EXECUTE prp_cnt_sgPreliq INTO v_i_cuenta_regs

   IF v_i_cuenta_regs = 0 OR SQLCA.SQLCODE <> 0 THEN
      -- se muestra mensaje a usuario y no continua
      CALL fn_mensaje("Aviso","No se encontró información preliquidada","stop")

      EXIT PROGRAM
   END IF

   WHENEVER ERROR STOP

   -- se valida que exista información en la tabla de preliquidación
   LET v_s_qryTxt = " SELECT FIRST 1 folio_liquida\n",
                    "   FROM cre_sg_preliquida"

   PREPARE prp_sclt_folLiq FROM v_s_qryTxt
   EXECUTE prp_sclt_folLiq INTO v_folio_liquida

   IF v_folio_liquida = 0 OR v_folio_liquida IS NULL THEN
      -- se muestra mensaje a usuario y no continua
      CALL fn_mensaje("Aviso","No se encontró folio de liquidación","stop")

      EXIT PROGRAM
   END IF

   -- se valida que el folio de liquidación no haya sido ya liquidado
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM cta_movimiento\n",
                    "  WHERE folio_liquida = ",v_folio_liquida

   PREPARE prp_cnt_ctaMov FROM v_s_qryTxt
   EXECUTE prp_cnt_ctaMov INTO v_i_cuenta_regs

   IF v_i_cuenta_regs > 0 THEN
      -- se muestra mensaje a usuario y no continua
      CALL fn_mensaje("Aviso","No se encontró información a liquidar","stop")

      EXIT PROGRAM
   END IF

   -- se invoca la funcion que valida la operacion
   LET r_b_valida = fn_valida_operacion(g_pid, g_proceso_cod, g_opera_cod)

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

{   -- se crea la sentencia sql que ejecuta la funcion que genera el pid
   LET g_pid = fn_genera_pid(g_proceso_cod, g_opera_cod, p_usuario_cod)   

   -- se invoca la funcion que inicializa el proceso
   LET r_b_valida = fn_inicializa_proceso(g_pid, g_proceso_cod, g_opera_cod,
                                          v_folio_liquida, v_c_programa_cod,
                                          v_v_nom_archivo, p_usuario_cod)

   -- se verifica si fue posible inicializar el proceso
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se invoca la función que deja la operación en estado Procesando
   LET r_b_valida = fn_actualiza_opera_ini(g_pid, g_proceso_cod, g_opera_cod,
                                           v_folio_liquida, v_c_programa_cod,
                                           v_v_nom_archivo, p_usuario_cod)

   -- se verifica si no fue posible inicializar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se invoca la función que deja la operación en estado Finalizado
   LET r_b_valida = fn_actualiza_opera_fin(g_pid, g_proceso_cod, g_opera_cod)

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se asigna la operación siguiente
   LET g_opera_cod = 2 -- liquidación fc y/o av

   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(g_pid, g_proceso_cod, g_opera_cod) RETURNING r_b_valida

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF}
   
   -- se invoca la funcion que obtiene el folio de liquidación
   DISPLAY "PROCESO COD : ",g_proceso_cod
   DISPLAY "OPERA COD : ",g_opera_cod
   --LET v_folio_liquida = fn_obten_folio(g_proceso_cod, g_opera_cod)
   {SELECT MAX(folio)
     INTO v_folio_liquida
     FROM glo_folio
    WHERE proceso_cod = g_proceso_cod}

   DISPLAY "FOLIO : ",v_folio_liquida

   -- se obtiene la ruta bin y de listados del módulo
   CALL fn_rutas("grt") RETURNING r_c_ruta_bin, r_ruta_listados

   -- se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat1 FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat1 INTO v_c_ruta_list_bat

   -- se obtiene el maximo pid del proceso y opera cod
   LET g_pid = fn_max_pid(g_proceso_cod,g_opera_cod)

   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod) RETURNING r_b_valida

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      DISPLAY "ERROR VALIDA: ",r_b_valida
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF


   -- se invoca la funcion para ejecutar la liquidacion
   CALL fn_liquida(p_usuario_cod, g_proceso_cod, g_opera_cod, v_opcion_fun)
END MAIN
