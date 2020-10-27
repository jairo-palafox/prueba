################################################################################
#Modulo        => ACL
#Programa      => ACLG02
#Objetivo      => Definicion de variables y constantes globales para el modulo de aclaratorio
#Fecha Inicio  => 18 JULIO 2012
#Autor         => Hilda Rivas
################################################################################
#Registro de modificaciones
#Autor         Fecha        Descripción
#Eneas Armas   27/12/2013   Se agrega constante de proceso 108
#
DATABASE safre_viv

GLOBALS
-- codigos de los procesos aclaratorios
   CONSTANT 
      g_proceso_cod_acl_registro_pagos         SMALLINT = 101 , -- registro de pagos en aclaratorio
      g_proceso_cod_acl_reg_pag_sin_cambio     SMALLINT = 102 , -- registro pagos aclaratorio sin cambio NSS
      g_proceso_cod_acl_reg_pag_cambio         SMALLINT = 103 , -- registro pagos aclaratorio cambio NSS
      g_proceso_cod_acl_reg_pag_cambio_nombre  SMALLINT = 107 , -- registro pagos aclaratorio cambio nombre
      g_proceso_cod_acl_salida_manual          SMALLINT = 105 ,  -- salida manual aclaratorio
      g_proceso_cod_acl_extractor_aclaraciones SMALLINT = 108 ,  -- salida manual archivo
	    g_proceso_cod_acl_act_sin_causal         SMALLINT = 109 ,  -- actualizacion de aclaraciones sin causal
      g_proceso_cod_acl_ext_pend               SMALLINT = 111 ,  -- extractor acl pendientes y sin conciliar
      g_proceso_cod_acl_rechazos               SMALLINT = 112 ,  -- generación de archivo de rechazos (acl blancos únicos)
      g_proceso_cod_acl_rechazos_dups          SMALLINT = 113 ,  -- generación de archivo de rechazos (acl blancos duplicados)
      
      -- etapas de las operaciones de los diferentes procesos
      g_opera_cod_carga                       SMALLINT = 1 , -- seleccion aclaratorio
      g_opera_cod_integracion                 SMALLINT = 2 , -- integracion
      g_opera_cod_preliquidacion              SMALLINT = 3 , -- preliquidacion aclaratorio
      g_opera_cod_liquidacion                 SMALLINT = 4 , -- liquidacion aclaratorio 
	    g_opera_cod_gen_arch_salida_manual      SMALLINT = 5 , -- generar archivo salida manual ACL
	  
	    -- etapas de la actualizacion de aclaraciones sin causal
	    g_opera_cod_busqueda                    SMALLINT = 1, -- busqueda de las aclaraciones
	    g_opera_cod_actualizacion               SMALLINT = 2  -- actualizacion de las aclaraciones
                                                             
                                                             
END GLOBALS                                                  