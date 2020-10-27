--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 09/Mar/2016
--==============================================================================

################################################################################
#Proyecto     => SAFRE VIVIENDA                                                #
#Propietario  => E.F.P.                                                        #
#Modulo       => DAE                                                           #
#Programa     => DAEL30                                                        #
#Objetivo     => Lanzador de validación del archivo de Ajuste Individual       #
#                Amortizaciones Excedentes                                     #
#Fecha inicio => 09/Mar/2016                                                   #
################################################################################

DATABASE safre_viv
GLOBALS "DAEG01.4gl"
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid,     --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod  -- codigo de operacion

DEFINE g_reg_modulo   RECORD
        ruta_bin         CHAR(40),
        ruta_rescate     CHAR(40),
        ruta_listados    CHAR(40)
       END RECORD,
       seg_modulo_bat RECORD
        ruta_listados    CHAR(40)
       END RECORD
END GLOBALS

MAIN
DEFINE p_usuario        LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT,                     -- forma como ejecutara el programa
       p_titulo         STRING,                       -- titulo de la ventana
       r_bnd_fin_oper   SMALLINT,
       r_bnd_valida_op  SMALLINT,
       v_mensaje        STRING
 
   -- se recupera la clave de usuario desde parametro 
   LET p_usuario        = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo         = ARG_VAL(3)

   LET r_bnd_fin_oper = 0
   
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_titulo   IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_titulo  )
   END IF
   
   -- se asigna proceso y operacion
   LET g_proceso_cod = 2406 --Ajuste Amortización Excedente Individual
   LET g_opera_cod   = 1    --Validación de archivo
   
   -- Valida operacion para verificar si se puede continuar.
   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod)
   RETURNING r_bnd_valida_op
   DISPLAY r_bnd_valida_op
   IF ( r_bnd_valida_op = 0 ) THEN
      IF (fn_carga_archivo(g_pid,g_proceso_cod,g_opera_cod,2,"DAEL30","",p_usuario, TRUE) = TRUE)THEN
      END IF
   ELSE
      CALL fn_recupera_inconsis_opera(r_bnd_valida_op) RETURNING v_mensaje
      DISPLAY v_mensaje
      CALL fn_mensaje("Atención", v_mensaje, "stop")    
   END IF
END MAIN