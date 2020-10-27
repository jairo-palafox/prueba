--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 
--===============================================================
#########################################################################################
#Modulo       => DEO                                                                    #
#Programa     => DEOE01                                                                 #
#Objetivo     => Invoca la carga de datos para devolucion por errores de operacion      #
#Fecha inicio => Diciembre 28, 2011                                                     #
#########################################################################################
DATABASE safre_viv
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS
GLOBALS "DEOG01.4gl"

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING -- titulo de la ventana
       --
       ,r_bnd_fin_oper SMALLINT
       ,v_rest_valida  SMALLINT

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
   LET g_proceso_cod = g_proceso_cod_deo -- devolucion por errores de operacion
   LET g_opera_cod   = g_opera_cod_deo_carga -- recepcion de archivo
   
   -- se obtiene el PID del proceso
   SELECT MAX(pid)
   INTO g_pid
   FROM
    bat_ctr_proceso
   WHERE
    proceso_cod = g_proceso_cod
   AND
    estado_cod = 2

   -- Valida operacion para verificar si se puede continuar.
   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod)
                            RETURNING v_rest_valida
                            
   IF ( v_rest_valida = 0 ) THEN
      -- Inicio operacion

            -- Inicia proceso de carga de archivo.
            IF (fn_carga_archivo(g_pid,g_proceso_cod,g_opera_cod,2,"DEOE01",
                                 "",p_usuario_cod , true) = TRUE)THEN
               
               #CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
               #                  RETURNING r_bnd_fin_oper
               
            END IF
   ELSE
      CALL fn_mues_desc_valida(v_rest_valida)
      MENU
         COMMAND "Cerrar"
            EXIT MENU
      END MENU
   END IF

END MAIN

{

 Obtiene la descripcion del error de la validacion y
  la muestra en mensaje para suario.
}
FUNCTION fn_mues_desc_valida(p_resultado_opera)
  DEFINE p_resultado_opera SMALLINT
        ,v_descripcion LIKE cat_bat_parametro_salida.descripcion
  

   -- Obtiene la descripción resultado de la validacion
   SELECT descripcion
     INTO v_descripcion
     FROM cat_bat_parametro_salida
    WHERE cod_salida = p_resultado_opera

   -- Muestra el mensaje encontrado
   CALL fn_mensaje("Atención",v_descripcion CLIPPED,"information")

END FUNCTION -- fn_mues_desc_valida
