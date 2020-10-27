--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 11-04-2012
--===============================================================

#########################################################################################
#Modulo       => MDT                                                                    #
#Programa     => MDTL11                                                                 #
#Objetivo     => Programa lanzador de las rutinas de carga                              #
#Fecha inicio => FEBRERO 16, 2012                                                       #
#Autor        => Francisco López                                                        #
#########################################################################################
DATABASE safre_viv
GLOBALS
DEFINE g_pid                       LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod               LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod_carga           LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_num_folio                 DECIMAL(9)
END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion  INTEGER,
       p_cad_ventana    STRING
   
   LET p_usuario_cod   = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_cad_ventana   = ARG_VAL(3)

   CALL fn_carga_archivo_mdt(p_usuario_cod, p_tpo_ejecucion, p_cad_ventana)
END MAIN

{ ==========================================================================
Clave:  fn_carga_archivo
Nombre: fn_carga_archivo
Fecha creacion: 16/02/2012
Autor: Francisco López
Narrativa del proceso que realiza:
Efectua la validación de estructura de layout de respuesta de validación de 
Sustentabilidad.

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

============================================================================}
FUNCTION fn_carga_archivo_mdt(p_usuario_cod, p_tpo_ejecucion, p_cad_ventana)
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, --Clave de usuario
       p_tpo_ejecucion   INTEGER,
       p_cad_ventana     STRING,    --Cadena de la ventana
       lsi_tpoProceso    SMALLINT,
       lsi_tpoOperacion  SMALLINT,
       v_comando         STRING,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_ruta_listados   LIKE seg_modulo.ruta_listados,
       v_nom_archivo     STRING
      ,r_bnd_carga       BOOLEAN
      ,v_estatus         SMALLINT
      ,v_ruta_vacia      LIKE seg_modulo.ruta_listados,
       v_inicia_proceso  BOOLEAN
   --Inicializacion de variables
   INITIALIZE lsi_tpoProceso, lsi_tpoOperacion TO NULL

   -- se dan las variables de proceso y operacion
   --LET g_proceso_cod               = 48 -- validación de Sustentabilidad de Mandatos
   # HCRG se modifica el proceso a peticion de usuario
   LET g_proceso_cod               = 1306 -- validación de Sustentabilidad de Mandatos
   LET g_opera_cod_carga           = 1  -- CARGA ARCHIVO
   
   --Se asigna el título de la ventana
   IF ( p_cad_ventana IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_cad_ventana)
   END IF

   -- se verifica si ya se termino el proceso anterior, si no,
   -- no se permite capturar nuevos saldos
   LET v_estatus = fn_valida_operacion(0,g_proceso_cod,g_opera_cod_carga)
   IF ( v_estatus = 0 ) THEN
      -- se invoca la carga de archivo de registro de pagos
      --Se genera el pid
      {CALL fn_genera_pid(g_proceso_cod
                        ,g_opera_cod_carga
                        ,p_usuario_cod)
                        RETURNING g_pid}

      --sE OBTIENEN las rutas de los ejecutables
      CALL fn_rutas("mdt") RETURNING v_ruta_ejecutable, v_ruta_vacia
      CALL fn_rutas("bat") RETURNING v_ruta_vacia, v_ruta_listados

      --Obtiene el nombre del archivo
      LET v_nom_archivo = "NA"
      LET g_num_folio = 0
      
      LET v_comando = "N/A"
      --Se da de alta el proceso de carga y colo todos los demas procesos como listo
      {CALL fn_inicializa_proceso(g_pid
                                 ,g_proceso_cod
                                 ,g_opera_cod_carga
                                 ,0
                                 ,"MDTL11"
                                 ,""
                                 ,p_usuario_cod)
                                 RETURNING v_estatus}

      CALL fn_display_proceso(0,"VALIDA ARCHIVO - SOLICITUD SUSTENTABILIDAD")
      LET v_inicia_proceso = 1 # la carga de archivo inicia el proceso
      -- jdym CALL fn_carga_archivo(g_pid, g_proceso_cod, g_opera_cod_carga, p_tpo_ejecucion,"PAGL10",v_comando, p_usuario_cod)
      CALL fn_carga_archivo(g_pid, g_proceso_cod, g_opera_cod_carga, p_tpo_ejecucion,"MDTL11",v_comando, p_usuario_cod, v_inicia_proceso)
           RETURNING r_bnd_carga;

      CALL fn_display_proceso(1,"VALIDA ARCHIVO - SOLICITUD SUSTENTABILIDAD")
   ELSE
      CALL fn_muestra_inc_operacion(v_estatus)
   END IF

END FUNCTION
