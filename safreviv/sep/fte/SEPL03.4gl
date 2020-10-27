--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 
--===============================================================

#########################################################################################
#Modulo       => SEP                                                                    #
#Programa     => SEPL03                                                                 #
#Objetivo     => Programa lanzador de las rutinas de carga                              #
#Fecha inicio => Mayo 02, 2012                                                          #
#Autor        => Alexandro Hollmann, EFP                                                #
#########################################################################################
DATABASE safre_viv
GLOBALS
DEFINE g_pid                       LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod               LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod_carga           LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion  INTEGER,
       p_cad_ventana    STRING
   
   LET p_usuario_cod   = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_cad_ventana   = ARG_VAL(3)

   CALL STARTLOG(p_usuario_cod CLIPPED||".SEPL03.log")
   CALL fn_carga_archivo_sep(p_usuario_cod, p_tpo_ejecucion, p_cad_ventana)
END MAIN

{==========================================================================
Clave:  fn_carga_archivo_sep
Nombre: fn_carga_archivo_sep
Fecha creacion: 02/05/2012
Autor: Alexandro Hollmann, EFP
Narrativa del proceso que realiza:
Efectua la validación de estructura de layout de respuesta de validación de 
Sustentabilidad.

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

============================================================================}
FUNCTION fn_carga_archivo_sep(p_usuario_cod, p_tpo_ejecucion, p_cad_ventana)
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, --Clave de usuario
       p_tpo_ejecucion   INTEGER,
       p_cad_ventana     STRING,    --Cadena de la ventana
       lsi_tpoProceso    SMALLINT,
       lsi_tpoOperacion  SMALLINT,
       v_comando         STRING,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_ruta_listados   LIKE seg_modulo.ruta_listados,
       r_bnd_carga       BOOLEAN
      ,v_estatus         SMALLINT
      ,v_ruta_vacia      LIKE seg_modulo.ruta_listados,
       v_inicia_proceso  BOOLEAN
   --Inicializacion de variables
   INITIALIZE lsi_tpoProceso, lsi_tpoOperacion TO NULL

   -- se dan las variables de proceso y operacion
   # HCRG se modifica el proceso a peticion de usuario
   LET g_proceso_cod               = 2237 -- validación de archivo para operacion 28
   LET g_opera_cod_carga           = 1  -- CARGA ARCHIVO
   
   --Se asigna el título de la ventana
   IF ( p_cad_ventana IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_cad_ventana)
   END IF

   -- se verifica si ya se termino el proceso anterior, si no,
   -- no se permite capturar nuevos saldos
   LET v_estatus = fn_valida_operacion(0,g_proceso_cod,g_opera_cod_carga)
   IF ( v_estatus = 0 ) THEN
      SELECT ruta_bin
        INTO v_ruta_ejecutable
        FROM seg_modulo
       WHERE modulo_cod = "sep"

      LET g_pid = 0
      -- se invoca la carga de archivo de registro de pagos
{      
      LET v_comando = "fglrun ",v_ruta_ejecutable CLIPPED,"/SEPX30.42r ",
                                                          p_usuario_cod," ",
                                                          g_pid," ",
                                                          g_proceso_cod," ",
                                                          g_opera_cod_carga," ",
                                                          0," ", # folio
                                                          "NA" # archivo
}                                                          
      CALL fn_display_proceso(0,"VALIDA ARCHIVO - OPERACION 40")
      LET v_inicia_proceso = 1 # la carga de archivo inicia el proceso
      CALL fn_carga_archivo(g_pid, g_proceso_cod, g_opera_cod_carga, p_tpo_ejecucion,"SEPL03",v_comando, p_usuario_cod, v_inicia_proceso)
           RETURNING r_bnd_carga;
      CALL fn_display_proceso(1,"VALIDA ARCHIVO - OPERACION 40")
   ELSE
      CALL fn_muestra_inc_operacion(v_estatus)
   END IF

END FUNCTION
