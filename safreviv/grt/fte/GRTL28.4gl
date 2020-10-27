--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:  26/04/2012
--===============================================================

################################################################################
#Modulo       => GRT                                                           #
#Programa     => GRTL28                                                        #
#Objetivo     => Programa que ejecuta el procedure que realiza la liquidacion  #
#                para lel modulo de uso de garantía                            #
#Autor        =>Daniel Buendia, EFP                                            #
#Fecha inicio => Abril 26, 2012                                                #
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
          v_v_nom_archivo            LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo en proceso
          v_folio_liquida            LIKE cta_movimiento.folio_liquida,
          v_i_opera_cod_ant          LIKE cat_operacion.opera_cod, -- codigo de operacion anterior
          v_i_estado                 SMALLINT, --estado al que se va a actualizar
          v_c_programa_cod           LIKE bat_ctr_operacion.programa_cod, -- nombre del programa
          v_s_qryTxt                 STRING, -- guarda una sentencia SQL a ejecutar
          v_i_opera_cod_folio        LIKE cat_operacion.opera_cod, -- codigo de operacion para el folio
          v_tpo_originacion          LIKE cre_acreditado.tpo_originacion, -- tipo de originación
          v_marca_entra              SMALLINT, --marca de entrada
          v_estado_marca             SMALLINT, --estado de la marca
          v_marca_causa              SMALLINT, --marca causa
          v_c_ruta_list_bat          LIKE seg_modulo.ruta_listados, -- ruta listados de bat
          r_c_ruta_bin               LIKE seg_modulo.ruta_bin, -- ruta del bin del módulo
          r_ruta_listados            LIKE seg_modulo.ruta_listados, -- ruta de listados del módulo
          r_b_valida                 SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   -- se recuperan los valores enviados como parametro
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".GRTL28.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   -- se inicializan las variables
   LET g_proceso_cod         = g_proc_cod_grt_uso_liquida -- liquidación de uso en garantía
   LET g_opera_cod           = 2 -- liquida saldo uso 43bis
   LET v_opcion_fun          = 2 -- ejecutar liquidacion
   LET v_i_opera_cod_ant     = 1 -- código de operación anterior (Preliquidación)
   LET v_v_nom_archivo       = "N/A" -- no aplica para este proceso
   LET v_folio_liquida       = 0
   LET v_i_estado            = 140 -- liquidado
   LET v_c_programa_cod      = "GRTL20"
   LET v_i_opera_cod_folio   = 1 -- preliquida uso de garantía
   LET v_tpo_originacion     = 2 -- uso de garantía
   LET v_marca_entra         = 223 --marca para uso de garantía  
   LET v_estado_marca        = 0 
   LET v_marca_causa         = 0 

   -- se invoca la funcion que obtiene el folio de liquidación
   LET v_folio_liquida = fn_obten_folio(g_proceso_cod, v_i_opera_cod_folio)

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
