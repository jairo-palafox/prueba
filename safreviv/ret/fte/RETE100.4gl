--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => RETE                                                                    #
#Programa     => RETE100                                                                 #
#Objetivo     => Programa que ejecuta el programa de carga inicial de Retiros SPES      #
#Fecha inicio => Abril 16, 2012                                                         #
#########################################################################################
IMPORT os
DATABASE safre_viv
GLOBALS "RETG01.4gl"
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod -- codigo de operacion

DEFINE g_ruta_rescate      STRING,--LIKE seg_modulo.ruta_rescate,
       g_usuario           LIKE seg_modulo.usuario,
       g_reg_archivo       INTEGER,
       g_reg_rechazados    INTEGER,
       g_reg_aceptados     INTEGER,
       g_detalle_monitoreo STRING,
       g_archivo_monitoreo STRING,
       g_ruta_listados     STRING,
       g_reg_tab_cargados  INTEGER
      
END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING -- titulo de la ventana
DEFINE p_proceso          LIKE cat_proceso.proceso_cod,
       p_operacion        LIKE cat_operacion.opera_cod,
       p_pid              DECIMAL(9,0),
       p_usuario          LIKE seg_usuario.usuario_cod,
       p_programa         LIKE bat_ctr_operacion.programa_cod,
       p_folio            DECIMAL(9,0),
       v_layout           LIKE cat_operacion.layout_cod,
       v_extension        LIKE cat_operacion.extension,
       v_ruta_ejecutable  LIKE seg_modulo.ruta_bin,
       v_proceso_desc     LIKE cat_proceso.proceso_desc,
       v_opera_desc       LIKE cat_operacion.opera_desc,
       v_nom_archivo      STRING,
       v_nom_arch_opera   VARCHAR(40),
       --v_nom_arch_opera   LIKE bat_ctr_operacion.nom_archivo,
       v_bnd_ejecuta      BOOLEAN,
       r_bnd_carga        BOOLEAN,
       v_aux_pid          STRING,
       v_aux_proc         STRING,
       v_aux_opera        STRING,
       v_comando          STRING,
       v_bnd_continua     BOOLEAN,
       v_cb_archivo       ui.ComboBox,
       v_ventana          ui.Window,
       v_forma_actual     ui.Form,
       r_resultado_opera  SMALLINT,
       v_fecha_inicio     DATETIME YEAR TO SECOND

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   LET g_usuario = p_usuario_cod
   
   -- se asigna proceso y operacion
   LET g_proceso_cod = g_proceso_cod_ret_carga_spes -- carga inicial de retiros SPESS
   LET g_opera_cod   = g_opera_cod_ret_carga_spes_carga -- operacion de carga

   CALL STARTLOG(p_usuario_cod CLIPPED||".RETE100.log")
   
   -- se verifica si se puede iniciar la operacion
   CALL fn_valida_operacion(0, g_proceso_cod, g_opera_cod) RETURNING r_resultado_opera

   -- si se puede lanzar la carga
   IF ( r_resultado_opera = 0 ) THEN
   
     --  se genera el pid para el proceso
     CALL fn_genera_pid(p_proceso,p_operacion,p_usuario)
                        RETURNING p_pid
                        
     -- se obtiene el folio
     CALL fn_genera_folio(g_proceso_cod, g_opera_cod, p_usuario)
                         RETURNING p_folio

     DISPLAY "proceso y operacion: ", g_proceso_cod, g_opera_cod

     CALL fn_carga_archivo(p_pid,g_proceso_cod,g_opera_cod,2,"RETE100",
                                 "NA",p_usuario_cod, true)
                                 RETURNING r_resultado_opera
   ELSE
     DISPLAY "No se puede inicia la carga"
   END IF
END MAIN
