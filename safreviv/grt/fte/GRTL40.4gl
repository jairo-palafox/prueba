--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

################################################################################
#Modulo       => GRT                                                           #
#Programa     => GRTL40                                                        #
#Objetivo     => Programa que ejecuta el procedure que realiza la liquidacion  #
#                de DSE para el módulo de Créditos en Garantía 43 bis          #
#Autor        => Daniel Buendia, EFP                                           #
#Fecha inicio => 30 Mayo 2012                                                  #
################################################################################

DATABASE safre_viv
GLOBALS "GRTG01.4gl"

GLOBALS
DEFINE g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
   DEFINE p_usuario_cod              LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
          p_tipo_ejecucion           SMALLINT, -- forma como ejecutara el programa
          p_s_titulo                 STRING, -- titulo de la ventana
          v_v_nom_archivo            LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo en proceso
          v_c_ruta_list_bat          LIKE seg_modulo.ruta_listados, -- ruta listados de bat
          v_d_pid                    DECIMAL(9,0), -- identificador del proceso
          v_folio_liquida            LIKE cta_movimiento.folio_liquida,
          v_i_opera_cod_ant          LIKE cat_operacion.opera_cod, -- codigo de operacion anterior
          v_i_estado                 SMALLINT, --estado al que se va a actualizar
          v_c_programa_cod           LIKE bat_ctr_operacion.programa_cod, -- nombre del programa
          v_c_tpo_transferencia      LIKE dse_agrupa_devolucion.tpo_transferencia, -- tipo de trasnferencia
          v_s_qryTxt                 STRING, -- guarda una sentencia SQL a ejecutar
          r_b_valida                 SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   -- se recupera la clave de usuario desde parametro argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".GRTL40.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   -- se inicializan las variables
   LET g_proceso_cod = g_proc_cod_grt_liquida_dse -- liquidacion devolución saldos exc grt
   LET g_opera_cod   = 2 -- liquida devolución de saldos
   LET v_i_opera_cod_ant = 1 -- código de operación anterior (Preliquidación)
   LET v_v_nom_archivo = "N/A"
   LET v_d_pid = 0
   LET v_folio_liquida = 0
   LET v_i_estado = 140 -- liquidado
   LET v_c_programa_cod = "GRTL40"
   LET v_c_tpo_transferencia = 19 -- 19-Créditos en Garantía 43 bis

   -- se invoca la funcion que obtiene el folio de liquidación
   LET v_folio_liquida = fn_obten_folio(g_proceso_cod, v_i_opera_cod_ant)

   -- se obtiene el maximo pid del proceso y opera cod
   LET v_d_pid = fn_max_pid(g_proceso_cod,g_opera_cod)

   -- se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat1 FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat1 INTO v_c_ruta_list_bat

   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(v_d_pid,g_proceso_cod,g_opera_cod) RETURNING r_b_valida

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se invoca la funcion para enviar la liquidacion
   CALL fn_liquida(p_usuario_cod, g_proceso_cod, g_opera_cod, g_opera_cod)
END MAIN
