--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 18/04/2012
--===============================================================

#########################################################################################
#Modulo       => PAG                                                                    #
#Programa     => PAGL51                                                                 #
#Objetivo     => Programa lanzador de las rutinas de carga de archivo                   #
#                de registro de pagos de fondo anterior                                 #
#Fecha inicio => Febrero 01, 2013                                                       #
#########################################################################################
DATABASE safre_viv

GLOBALS "PAGG01.4gl"  ---archivo de variables globales proceso_cod, opera_cod

GLOBALS
DEFINE g_pid                       LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_num_folio                 DECIMAL(9)
END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion  INTEGER,
       p_cad_ventana    STRING
   
   {
    Se recuperan los parametros recibidos
   Clave de usuario
   Tipo de ejecucion (en línea o batch)
   Cadena que identifica al programa (lo que aparecería como título de la ventana)
   }
   LET p_usuario_cod   = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_cad_ventana   = ARG_VAL(3)

   CALL fn_carga_archivo_fondo_anterior(p_usuario_cod, p_tpo_ejecucion, p_cad_ventana)
END MAIN

{ ==========================================================================
Clave:  fn_carga_archivo_fondo_anterior
Nombre: fn_carga_archivo_fondo_anterior
Fecha creacion: 01 de Febrero de 2013
Autor: Ivan Vega

Narrativa del proceso que realiza:
Ejecuta la funcion general de carga para carga archivos de registro
de pagos de fondo anterior

Parametros de Entrada:
-

Parámetros de salida;
-

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

============================================================================}
FUNCTION fn_carga_archivo_fondo_anterior(p_usuario_cod, p_tpo_ejecucion, p_cad_ventana)
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, --Clave de usuario
       p_tpo_ejecucion   INTEGER,   --Tipo de ejecucion
       p_cad_ventana     STRING,    --Cadena de la ventana
       v_comando         STRING,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_ruta_listados   LIKE seg_modulo.ruta_listados,
       v_nom_archivo     STRING,
       v_cadena_pid      VARCHAR(5),
       v_cadena_proc     VARCHAR(5),
       r_bnd_carga       BOOLEAN,
       v_estatus         SMALLINT,
       v_ruta_vacia      LIKE seg_modulo.ruta_listados

   -- se asigna el titulo a la ventana
   CALL ui.Interface.setText(p_cad_ventana)

   
   -- se verifica si se puede lanzar la carga
   LET v_estatus = fn_valida_operacion(0, g_proceso_cod_pag_registro_pagos_fa, g_opera_cod_pag_carga)
   
   IF ( v_estatus = 0 ) THEN

      -- se invoca la funcion general de carga con inicio de proceso
      CALL fn_carga_archivo(g_pid, g_proceso_cod_pag_registro_pagos_fa, g_opera_cod_pag_carga, p_tpo_ejecucion,"PAGL51",NULL, p_usuario_cod, TRUE)
           RETURNING r_bnd_carga;

   ELSE
      -- no se puede iniciar la carga
      CALL fn_muestra_inc_operacion(v_estatus)
   END IF
   
END FUNCTION
