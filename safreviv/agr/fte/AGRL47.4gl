--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

#########################################################################
#Modulo       => AGR                                                    #
#Programa     => AGRL47                                                 #
#Objetivo     => Programa que ejecuta la función general de liquidacion #
#                para el proceso de Uso de Garantia Estados y Municipios#
#Autor        => Daniel Buendia, EFP                                    #
#Fecha inicio => 01 Agosto 2013                                         #
#########################################################################

DATABASE safre_viv
GLOBALS "AGRG01.4gl"

DEFINE m_d_pid           LIKE bat_ctr_proceso.pid, --  ID del proceso
       m_si_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       m_si_opera_cod    LIKE cat_operacion.opera_cod -- codigo de operacion

MAIN
   DEFINE p_usuario_cod              LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
          p_tipo_ejecucion           SMALLINT, -- forma como ejecutara el programa
          p_s_titulo                 STRING, -- titulo de la ventana
          v_opcion_fun               SMALLINT, -- opcion para la funcion general
          v_folio_liquida            LIKE cta_movimiento.folio_liquida,
          v_i_opera_cod_folio        LIKE cat_operacion.opera_cod, -- codigo de operacion para el folio
          r_b_valida                 SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   -- se recuperan los valores enviados como parametro
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".AGRL47.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   -- se inicializan las variables
   LET m_si_proceso_cod      = g_proc_cod_agr_liq_gtia_ed_mc -- liquidación de uso garantía estado y municipio
   LET m_si_opera_cod        = 2 -- liquidación estado y municipio
   LET v_opcion_fun          = 2 -- ejecutar liquidacion
   LET v_folio_liquida       = 0
   LET v_i_opera_cod_folio   = 1 -- preliquida saldo
   
   -- se invoca la funcion que obtiene el folio de liquidación
   LET v_folio_liquida = fn_obten_folio(m_si_proceso_cod, v_i_opera_cod_folio)

   -- se obtiene el maximo pid del proceso y opera cod
   LET m_d_pid = fn_max_pid(m_si_proceso_cod, m_si_opera_cod)

   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(m_d_pid, m_si_proceso_cod, m_si_opera_cod) RETURNING r_b_valida

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se invoca la funcion para ejecutar la liquidacion
   CALL fn_liquida(p_usuario_cod, m_si_proceso_cod, m_si_opera_cod, v_opcion_fun)
END MAIN
