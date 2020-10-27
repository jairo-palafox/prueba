--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 26/Oct/2016
--===============================================================

################################################################################
#Modulo       => DPE                                                           #
#Programa     => DPEL01                                                        #
#Objetivo     => Invoca la carga de datos para devolucion por pagos indebidos  #
#                o en exceso de pago                                           #
#Fecha inicio => Octubre 26, 2016                                              #
################################################################################
GLOBALS "DPEG01.4gl"
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion

DEFINE g_reg_modulo   RECORD
        ruta_bin         CHAR(40),
        ruta_rescate     CHAR(40),
        ruta_listados    CHAR(40)
       END RECORD,
       seg_modulo_bat RECORD
        ruta_listados    CHAR(40)
       END RECORD
END GLOBALS

-- GLOE01 - que define la funcion de carga

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING, -- titulo de la ventana
       r_bnd_fin_oper   SMALLINT,
       v_rest_valida    SMALLINT,
       v_s_comando      STRING,
       v_nombre_archivo STRING,
       v_mensaje        STRING
 
   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   LET r_bnd_fin_oper = 0
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   -- se asigna proceso y operacion
   LET g_proceso_cod = 1001
   LET g_opera_cod   = 1
     
   -- Valida operacion para verificar si se puede continuar.
   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod)
                            RETURNING v_rest_valida
   IF ( v_rest_valida = 0 ) THEN

      LET v_nombre_archivo = "NA"
   
      SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
      INTO   g_reg_modulo.*
      FROM   seg_modulo s
      WHERE  s.modulo_cod = 'dpe'

      SELECT b.ruta_listados
      INTO   seg_modulo_bat.ruta_listados
      FROM   seg_modulo b
      WHERE  b.modulo_cod = 'bat'

      LET v_s_comando = " fglrun " ,g_reg_modulo.ruta_bin CLIPPED,"/DPEP09 ",
                                   p_usuario_cod CLIPPED, " ",
                                   0 , " " ,
                                   g_proceso_cod , " " ,
                                   g_opera_cod ," ",
                                   'NA', " ",                                                          
                                   " 1>",seg_modulo_bat.ruta_listados CLIPPED,
                                   "/nohup:",0 USING "&&&&&",":",
                                   g_proceso_cod USING "&&&&&",":",
                                   g_opera_cod   USING "&&&&&" ,
                                   " 2>&1 &"
      DISPLAY v_s_comando 
         -- Inicio operacion.       
         IF (fn_carga_archivo(g_pid,g_proceso_cod,g_opera_cod,2,"DPEL01",
                              v_s_comando,p_usuario_cod, TRUE) = TRUE)THEN
         END IF
   ELSE
      CALL fn_recupera_inconsis_opera(r_bnd_fin_oper) RETURNING v_mensaje
   END IF

END MAIN