--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 10/04/2013
--==============================================================================

################################################################################
#Modulo       => PAG                                                           #
#Programa     => PAGL61                                                        #
#Objetivo     => Programa lanzador de las rutinas de carga de archivo          #
#                de aportaciones voluntarias                                   #
#Fecha inicio => 10 Abril de 2013                                              #
################################################################################
DATABASE safre_viv

GLOBALS "PAGG01.4gl"  # archivo de variables globales proceso_cod, opera_cod


DEFINE g_pid LIKE bat_ctr_proceso.pid

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion  INTEGER,
       p_cad_ventana    STRING
   
   # Se recuperan los parametros recibidos al programa
   LET p_usuario_cod   = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_cad_ventana   = ARG_VAL(3)

   CALL fn_carga_archivo_garantia_em(p_usuario_cod, p_tpo_ejecucion, p_cad_ventana)
END MAIN

{ ==========================================================================
Clave:  fn_carga_archivo_aportacion_voluntaria
Nombre: fn_carga_archivo_aportacion_voluntaria
Fecha creacion: 10 de abril de 2013
Autor: Ivan Vega

Narrativa del proceso que realiza:
Ejecuta la funcion general de carga para carga archivos de registro
de pagos de aportacion voluntaria

Parametros de Entrada:
-

Parámetros de salida;
-

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

============================================================================}
FUNCTION fn_carga_archivo_garantia_em(p_usuario_cod, p_tpo_ejecucion, p_cad_ventana)
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, --Clave de usuario
       p_tpo_ejecucion   INTEGER,   --Tipo de ejecucion
       p_cad_ventana     STRING,    --Cadena de la ventana
       r_bnd_carga       BOOLEAN,
       v_estatus         SMALLINT,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_comando         STRING

   # se asigna el titulo a la ventana
   CALL ui.Interface.setText(p_cad_ventana)
   
   # se verifica si se puede lanzar la carga
   LET v_estatus = fn_valida_operacion(0, g_proceso_cod_pag_registro_pagos_av, g_opera_cod_pag_carga)
   
   IF ( v_estatus = 0 ) THEN
      LET g_pid = 0

      SELECT ruta_bin
        INTO v_ruta_ejecutable
        FROM seg_modulo
       WHERE modulo_cod = "pag"
       
      # se invoca la carga de archivo de registro de pagos
      LET v_comando = "fglrun ",v_ruta_ejecutable CLIPPED,"/PAGX61.42r ",
                                                            p_usuario_cod," ",
                                                            g_pid," ",
                                                            g_proceso_cod_pag_registro_pagos_av," ",
                                                            g_opera_cod_pag_carga," ",
                                                            0," ", # folio
                                                            "NA"   # archivo

      # se invoca la funcion general de carga con inicio de proceso
      CALL fn_carga_archivo(g_pid, 
                            g_proceso_cod_pag_registro_pagos_av, 
                            g_opera_cod_pag_carga, 
                            p_tpo_ejecucion,
                            "PAGL61",
                            v_comando, 
                            p_usuario_cod, 
                            TRUE)   RETURNING r_bnd_carga

   ELSE
      # muestra error en pantalla
      CALL fn_muestra_inc_operacion(v_estatus)
   END IF
   
END FUNCTION
