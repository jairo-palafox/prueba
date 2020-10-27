--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 18/04/2012
--===============================================================

#########################################################################################
#Modulo       => PAG                                                                    #
#Programa     => PAGL10                                                                 #
#Objetivo     => Programa lanzador de las rutinas de carga, integración y               #
#                preliquidacion de recibos de pagos                                     #
#Fecha inicio => Diciembre 28, 2011                                                     #
#########################################################################################
DATABASE safre_viv
GLOBALS "PAGG01.4gl"  ---archivo de variables globales proceso_cod, opera_cod
GLOBALS
DEFINE g_pid                       LIKE bat_ctr_proceso.pid, --  ID del proceso
       --g_proceso_cod               LIKE cat_proceso.proceso_cod, -- codigo del proceso
       --g_opera_cod_carga           LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_opera_cod_integracion     LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_opera_cod_preliquidacion  LIKE cat_operacion.opera_cod,-- codigo de operacion
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

   CALL fn_carga_archivo_pag(p_usuario_cod, p_tpo_ejecucion, p_cad_ventana)
END MAIN

{ ==========================================================================
Clave:  fn_carga_archivo_pag
Nombre: fn_carga_archivo_pag
Fecha creacion: 29 de Diciembre de 2011
Autor: David Miguel Garibay Rivera
Narrativa del proceso que realiza:
Esta función carga un archivo del proceso de "Registro de Pagos"

Parametros de Entrada:
-

Parámetros de salida;
-

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

============================================================================}
FUNCTION fn_carga_archivo_pag(p_usuario_cod, p_tpo_ejecucion, p_cad_ventana)
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, --Clave de usuario
       p_tpo_ejecucion   INTEGER,   --Tipo de ejecucion
                               -- 1 En linea
                               -- 2 Batch
       p_cad_ventana     STRING,    --Cadena de la ventana
       lsi_tpoProceso    SMALLINT,
       lsi_tpoOperacion  SMALLINT,
       v_comando         STRING,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_ruta_listados   LIKE seg_modulo.ruta_listados,
       v_nom_archivo     STRING,
       v_cadena_pid      VARCHAR(5),
       v_cadena_proc     VARCHAR(5),
       --v_cadena_opera    VARCHAR(5)
       r_bnd_carga       BOOLEAN,
       v_estatus         SMALLINT,
       v_ruta_vacia      LIKE seg_modulo.ruta_listados
   --Inicializacion de variables
   INITIALIZE lsi_tpoProceso, lsi_tpoOperacion TO NULL

   -- se dan las variables de proceso y operacion
   --LET g_proceso_cod               = 1401 -- liquidacion de pagos
   --LET g_opera_cod_carga           = 1 -- CARGA ARCHIVO REGISTRO PAGOS
   --LET g_opera_cod_integracion     = 2 -- Paso de información a las tablas históricas
   LET g_opera_cod_preliquidacion  = 3 -- Preliquidacion
   

--   OPEN WINDOW w_cargapagos WITH FORM "PAGL101"
   
   --Se asigna el tçitulo de la ventana
   IF ( p_cad_ventana IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_cad_ventana)
   END IF


   -- se verifica si ya se termino el proceso anterior, si no,
   -- no se permite capturar nuevos saldos
   LET v_estatus = fn_valida_operacion(0,g_proceso_cod_pag_registro_pagos_LQINFO,g_opera_cod_pag_carga)
   IF ( v_estatus = 0 ) THEN
      -- se invoca la carga de archivo de registro de pagos
      --Se genera el pid
      {CALL fn_genera_pid(g_proceso_cod
                        ,g_opera_cod_carga
                        ,p_usuario_cod)
                        RETURNING g_pid
}
      --sE OBTIENEN las rutas de los ejecutables
      CALL fn_rutas("pag") RETURNING v_ruta_ejecutable, v_ruta_vacia
      CALL fn_rutas("bat") RETURNING v_ruta_vacia, v_ruta_listados

      --Obtiene el nombre del archivo
      --LET v_nom_archivo = "NMRFREC.DSPRM.LQINFO"
      LET v_nom_archivo = "NA"
      LET g_num_folio = 0
      
      -- se crean las cadenas para el nombre del archivo log
      LET v_cadena_pid   = g_pid USING "&&&&&"
      LET v_cadena_proc  = g_proceso_cod_pag_registro_pagos_LQINFO USING "&&&&&"
      --LET v_cadena_opera = g_opera_cod_integracion USING "&&&&&" 
      
      --Se invoca el store procedure que almacena la información en las
      --tablas históricas
      {LET v_comando = "nohup fglrun "
                     ,v_ruta_ejecutable CLIPPED
                     ,"/PAGP10.42r "
                     ,p_usuario_cod, " "
                     ,g_pid, " "
                     ,g_proceso_cod," "
                     ,g_opera_cod_integracion," "
                     ,g_num_folio, " "
                     ,v_nom_archivo
                     ," 1>", v_ruta_listados CLIPPED
                     ,"/nohup:",v_cadena_pid,":"
                     ,v_cadena_proc,":"
                     ,v_cadena_opera ," 2>&1 &"
      }
      LET v_comando = "N/A"
      --Se da de alta el proceso de carga y colo todos los demas procesos como listo
      {CALL fn_inicializa_proceso(g_pid
                                 ,g_proceso_cod
                                 ,g_opera_cod_carga
                                 ,0
                                 ,"PAGL10"
                                 ,""
                                 ,p_usuario_cod)
                                 RETURNING v_estatus
                                 }

      --Se invoca la función que lee el archivo y 
      --guarda la información en las tablas temporales
      --El primer parametro pertenece a la tabla "cat_proceso"
      --   1 Liquidacion  de pagos
      --   2 Recaudacion SAR 92
      --   3 Recaudación solo INFONAVIT
      --El segundo  parametro pertenece a la tabla "cat_operaciones",
      --y depende del valor recibido en el primer parámetro.
      --
      --El tercer parámtero indica si el proceso debe ejecutarse 
      --   1 Proceso en línea
      --   2 Batch
      CALL fn_display_proceso(0,"CARGA")

      LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/PAGS02.42r ", --saci2018-13
                                                p_usuario_cod," ",              --saci2018-13
                                                g_pid," ",                      --saci2018-13
                                                1401," ",                       --saci2018-13
                                                g_opera_cod_integracion," ",    --saci2018-13
                                                g_num_folio," ",                --saci2018-13
                                                v_nom_archivo                   --saci2018-13
      
      CALL fn_carga_archivo(g_pid, g_proceso_cod_pag_registro_pagos_LQINFO, g_opera_cod_pag_carga, p_tpo_ejecucion,"PAGL10",v_comando, p_usuario_cod, TRUE)
           RETURNING r_bnd_carga;

      CALL fn_display_proceso(1,"CARGA")
   ELSE
      CALL fn_muestra_inc_operacion(v_estatus)
   END IF

--   CLOSE WINDOW w_cargapagos
   
END FUNCTION

{ ==========================================================================
Clave:  fn_llena_combo_proceso
Nombre: fn_llena_combo_proceso
Fecha creacion: 05 de enero de 2012
Autor: David Miguel Garibay Rivera
Narrativa del proceso que realiza:
Esta función carga los datos del combo que muestra los procesos existentes
para el "Resgistro de Pagos"

Parametros de Entrada:
-

Parámetros de salida;
-

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

============================================================================}
FUNCTION fn_llena_combo_proceso(cb)
DEFINE 
   cb            ui.ComboBox, --
   li_contador   INTEGER,
   li_procesoCod INTEGER,     --Número de proceso
   lv_descrip    VARCHAR(40), --Descripcion del proceso
   ls_qryProceso STRING       --Esta variable permite extraer la información 
                              --de la tabla cat_proceso

   --Se indica el nombre del objeto al cual hace refeencia
   --el combo en la forma(.per)
   --LET cb = ui.ComboBox.forName("cmb_proceso")

   --Limpiar el combo
   CALL cb.clear()
   LET li_contador = 0

   --Verifica la existencia de procesos para el "Registro de pagos"
   LET ls_qryProceso = "SELECT COUNT(*)\n",
                       "  FROM cat_proceso\n",
                       " WHERE modulo_cod = 'pag'"

   PREPARE prp_conteoProc FROM ls_qryProceso
   EXECUTE prp_conteoProc INTO li_contador

   --Si existen procesos para el "Registro de pagos"
   IF (li_contador > 0) THEN
      LET ls_qryProceso = "SELECT proceso_cod, proceso_desc\n",
                          "  FROM cat_proceso\n",
                          " WHERE modulo_cod = 'pag'"

      PREPARE prp_procesos FROM ls_qryProceso
      DECLARE lcur_procesos CURSOR FOR prp_procesos 
      FOREACH lcur_procesos INTO li_procesoCod,lv_descrip 
         CALL cb.addItem(li_procesoCod,lv_descrip)
      END FOREACH
   END IF

END FUNCTION

{ ==========================================================================
Clave:  fn_llena_combo_operacion
Nombre: fn_llena_combo_operacion
Fecha creacion: 05 de Enero de 2011
Autor: David Miguel Garibay Rivera
Narrativa del proceso que realiza:
Esta función carga los datos del combo que muestra las operaciones existentes
para el "Resgistro de Pagos"

Parametros de Entrada:
-

Parámetros de salida;
-

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

============================================================================}
FUNCTION fn_llena_combo_operacion(lsi_tpoProceso)
DEFINE 
   lsi_tpoProceso  SMALLINT,
   cb              ui.ComboBox, --
   li_contador     INTEGER,
   li_operacionCod INTEGER,    --Número de proceso
   lv_descrip      VARCHAR(40),--Descripcion del proceso
   ls_qryProceso   STRING      --Esta variable permite extraer la información 
                               --de la tabla cat_proceso

   --Se indica el nombre del objeto al cual hace refeencia
   --el combo en la forma(.per)
   LET cb = ui.ComboBox.forName("formonly.cmb_operacion")

   --Limpiar el combo
   CALL cb.clear()
   LET li_contador = 0

   --Verifica la existencia de procesos para el "Registro de pagos"
   LET ls_qryProceso = "SELECT COUNT(*)\n",
                       "  FROM cat_operacion\n",
                       " WHERE proceso_cod = ?"

   PREPARE prp_conteoOperacion FROM ls_qryProceso
   EXECUTE prp_conteoOperacion USING lsi_tpoProceso
                                INTO li_contador

   --Si existen procesos para el "Registro de pagos"
   IF (li_contador > 0) THEN
      LET ls_qryProceso = "SELECT opera_cod, opera_desc\n",
                          "  FROM cat_operacion\n",
                          " WHERE proceso_cod = ?"

      PREPARE prp_operaciones FROM ls_qryProceso
      DECLARE lcur_operaciones CURSOR FOR prp_operaciones 
      FOREACH lcur_operaciones USING lsi_tpoProceso
                             INTO li_operacionCod,lv_descrip 
         CALL cb.addItem(li_operacionCod,lv_descrip)
      END FOREACH
   END IF

END FUNCTION



