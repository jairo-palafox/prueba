--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 18/04/2012
--===============================================================

#########################################################################################
#Modulo       => PAG                                                                    #
#Programa     => PAGL02                                                                 #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la liquidacion    #
#                para registro de pago                                                  #
#Fecha inicio => Enero 10, 2012                                                         #
#########################################################################################
DATABASE safre_viv
GLOBALS "PAGG01.4gl"  ---archivo de variables globales proceso_cod, opera_cod
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       --g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       --g_opera_cod    LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_operacion    SMALLINT -- 1 CONSULTA; 2 LIQUIDA
END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING -- titulo de la ventana

   --DEFINE v_tabla_preliq      CHAR(30)
   --DEFINE v_folio             DECIMAL(9,0)
   --DEFINE v_query             STRING
   --DEFINE v_cve_proceso       SMALLINT
   
   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".PAGL02.log")

   -- se asigna proceso y operacion
   --LET g_proceso_cod = 1401 -- registro de pagos
   --LET g_opera_cod   = 4 -- LIQUIDACION REGISTRO PAGOS
   LET g_operacion   = 2 -- liquidar
   
   CALL fn_liquida(p_usuario_cod, g_proceso_cod_pag_registro_pagos_LQINFO, g_opera_cod_pag_liquidacion, g_operacion)
   
END MAIN