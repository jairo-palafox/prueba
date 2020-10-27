--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 19/04/2013
--==============================================================================

################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DAE                                                      #
#Programa          => DAEP02                                                   #
#Objetivo          => Programa que ejecuta el stored procedure que realiza la  #
#                     preliquidación de Devolución de Amortizaciones Excedentes#
#Fecha inicio      => 19/04/2012                                               #
################################################################################

--LANZADOR: DAEL03

DATABASE safre_viv
GLOBALS "DAEG01.4gl"
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_pid            LIKE bat_ctr_operacion.pid, -- PID del proceso
       p_proceso_cod    LIKE bat_ctr_operacion.proceso_cod, -- codigo del proceso
       p_opera_cod      LIKE bat_ctr_operacion.opera_cod, -- codigo de la operacion
       p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_folio          LIKE deo_preliquida.folio_liquida, -- folio de la operacion
       p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo, -- nombre dle archivo
       r_bnd_fin_oper SMALLINT,
      --
       v_layout            LIKE cat_operacion.layout_cod,
       v_ruta_rescate      STRING,
       v_usuario           LIKE seg_modulo.usuario,
       v_proceso_desc      LIKE cat_proceso.proceso_desc,
       v_extension         LIKE cat_operacion.extension,
       v_opera_desc        LIKE cat_operacion.opera_desc,
       v_ruta_listados     LIKE seg_modulo.ruta_listados,
       v_ejecuta           SMALLINT,
       p_folio_dictamen    DECIMAL(9,0),
       v_folio_dictamen    DECIMAL(9,0),
       v_tot_sel_aceptados  INTEGER,
       v_tot_sel_rechazados INTEGER  
   -- se recuperan los parametros que envia el programa lanzador
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   LET p_folio_dictamen = ARG_VAL(7)
   
   CALL fn_recupera_inf_proceso(p_proceso_cod, p_opera_cod) 
                               RETURNING v_proceso_desc,
                                         v_extension, 
                                         v_opera_desc,
                                         v_layout, 
                                         v_ruta_rescate,
                                         v_ruta_listados,
                                         v_usuario
      
   -- se asigna proceso y operacion
   LET g_pid         = p_pid      
   LET g_proceso_cod = 2402
   LET g_opera_cod   = 1
   LET v_ejecuta     = 1
   LET v_folio_dictamen = p_folio_dictamen
   -- se obtiene el PID del proceso
   SELECT MAX(pid)
   INTO   g_pid
   FROM   bat_ctr_proceso
   WHERE  proceso_cod = g_proceso_cod

   SELECT COUNT(*)
   INTO   v_tot_sel_aceptados
   FROM   dae_det_solicitud
   WHERE  folio_dictamen = v_folio_dictamen
   AND    resul_opera = "01";

   SELECT COUNT(*)
   INTO   v_tot_sel_rechazados
   FROM   dae_det_solicitud
   WHERE  folio_dictamen = v_folio_dictamen
   AND    resul_opera = "02";

   UPDATE bat_ctr_operacion
   SET    folio = v_folio_dictamen
   WHERE  pid = g_pid
   AND    opera_cod = g_opera_cod

   DISPLAY "# # # # # # # # # # # # # # # # # # # # # # # # # #"
   DISPLAY "#    El dictamen se terminó completamente."
   DISPLAY "#    "
   DISPLAY "#    Folio Dictamen   : ", v_folio_dictamen
   DISPLAY "#    "
   DISPLAY "#    Total Aceptados  : ",v_tot_sel_aceptados
   DISPLAY "#    "
   DISPLAY "#    Total Rechazados : ",v_tot_sel_rechazados
   DISPLAY "#    "
   DISPLAY "# # # # # # # # # # # # # # # # # # # # # # # # # #"   


   -- se invoca la finalizacion de la operacion
   CALL fn_actualiza_opera_fin(g_pid, g_proceso_cod, g_opera_cod)
   RETURNING r_bnd_fin_oper         

   IF v_tot_sel_aceptados = 0 THEN 
      CALL fn_finaliza_proceso()
   END IF    
   --LET p_titulo = "Finalización de proceso - " || v_proceso_desc CLIPPED || " - DICTAMEN"    
--
   --CALL fn_correo_proceso(g_pid,g_proceso_cod,g_opera_cod,
                          --"/ds/safreviv/ret/bin/correo.txt", -- no lleva archivo adjunto
                          --p_titulo,
                          --p_mensaje)

END MAIN

#OBJETIVO: Finaliza todo el proceso en caso de que no se encuentre con ningún registro Aceptado
FUNCTION fn_finaliza_proceso()

   UPDATE bat_ctr_operacion
   SET  estado_cod = 4,
        fecha_ini = CURRENT,
        fecha_fin = CURRENT
   WHERE pid = g_pid
   AND proceso_cod = 2402
   AND   opera_cod IN (2,3) ;

   UPDATE bat_ctr_proceso
   SET    estado_cod = 4,
          fecha_fin = CURRENT
   WHERE  pid = g_pid
   AND    proceso_cod = 2402; 
          
END FUNCTION