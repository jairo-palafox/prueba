-----------------------------------------------------------------------------------------
-- Modulo        => PAG                                                                    
-- Programa      => PAGL90
-- Objetivo      => Lanzador de carga de archivo CambiaVit
-- Autor         => GERARDO ALFONSO VEGA PAREDES                                           
-- Fecha inicio  => 21 de Mayo de 2018
-- Requerimiento => plasgac-43
-----------------------------------------------------------------------------------------
-- Modificación => 
-- Fehca        => 
-- Autor        => 
-- Clave cambio => 
-----------------------------------------------------------------------------------------

DATABASE safre_viv

GLOBALS "PAGG01.4gl"  ---archivo de variables globales proceso_cod, opera_cod

GLOBALS

   DEFINE g_pid                   LIKE bat_ctr_proceso.pid,
          g_opera_cod_integracion LIKE cat_operacion.opera_cod,
          g_num_folio             DECIMAL(9)
          
END GLOBALS

MAIN
   DEFINE p_usuario_cod   LIKE seg_usuario.usuario_cod,
          p_tpo_ejecucion INTEGER,
          p_cad_ventana   STRING
   
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

FUNCTION fn_carga_archivo_pag(p_usuario_cod, p_tpo_ejecucion, p_cad_ventana)

   DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, --Clave de usuario
          p_tpo_ejecucion   INTEGER,   --Tipo de ejecucion (1 En linea / 2 Batch)
          p_cad_ventana     STRING,    --Cadena de la ventana
          lsi_tpoProceso    SMALLINT,
          lsi_tpoOperacion  SMALLINT,
          v_comando         STRING,
          v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
          v_ruta_listados   LIKE seg_modulo.ruta_listados,
          v_nom_archivo     STRING,
          v_cadena_pid      VARCHAR(5),
          v_cadena_proc     VARCHAR(5),
          r_bnd_carga       BOOLEAN,
          v_estatus         SMALLINT,
          v_ruta_vacia      LIKE seg_modulo.ruta_listados
      
   --Inicializacion de variables
   INITIALIZE lsi_tpoProceso, lsi_tpoOperacion TO NULL

   --Se asigna el tçitulo de la ventana
   IF p_cad_ventana IS NOT NULL THEN
      CALL ui.Interface.setText(p_cad_ventana)
   END IF

   -- se verifica si ya se termino el proceso anterior, si no,
   -- no se permite capturar nuevos saldos
   LET v_estatus = fn_valida_operacion(0,g_proceso_reg_pag_svt,g_opera_cod_pag_carga)
   IF v_estatus = 0 THEN

      --Se obtienen las rutas de los ejecutables
      CALL fn_rutas("pag") RETURNING v_ruta_ejecutable, v_ruta_vacia
      CALL fn_rutas("bat") RETURNING v_ruta_vacia, v_ruta_listados

      --Obtiene el nombre del archivo
      LET v_nom_archivo = "NA"
      LET g_num_folio = 0
      
      -- se crean las cadenas para el nombre del archivo log
      LET v_cadena_pid   = g_pid USING "&&&&&"
      LET v_cadena_proc  = g_proceso_reg_pag_svt USING "&&&&&"

      --Se invoca el store procedure que almacena la información en las
      --tablas históricas

      LET v_comando = "N/A"

      --Se invoca la función que lee el archivo y 
      --guarda la información en las tablas temporales
      --El primer parametro pertenece a la tabla "cat_proceso"
      --   1 Liquidacion  de pagos
      --   2 Recaudacion SAR 92
      --   3 Recaudación solo INFONAVIT
      --El segundo  parametro pertenece a la tabla "cat_operaciones",
      --y depende del valor recibido en el primer parámetro.
      --El tercer parámtero indica si el proceso debe ejecutarse 
      --   1 Proceso en línea
      --   2 Batch
      
      CALL fn_display_proceso(0,"CARGA")

      CALL fn_carga_archivo(g_pid, g_proceso_reg_pag_svt, g_opera_cod_pag_carga, p_tpo_ejecucion,"PAGL90",v_comando, p_usuario_cod, TRUE)
         RETURNING r_bnd_carga;

      CALL fn_display_proceso(1,"CARGA")
   ELSE
      CALL fn_muestra_inc_operacion(v_estatus)
   END IF

END FUNCTION

{ ==========================================================================
Nombre: fn_llena_combo_proceso
Fecha creacion: 05 de enero de 2012
Autor: GERARDO ALFONSO VEGA PAREDES
Narrativa del proceso que realiza:
Esta función carga los datos del combo que muestra los procesos existentes
para el "Resgistro de Pagos"
============================================================================}
FUNCTION fn_llena_combo_proceso(cb)
   DEFINE 
      cb            ui.ComboBox,
      li_contador   INTEGER,
      li_procesoCod INTEGER,     --Número de proceso
      lv_descrip    VARCHAR(40), --Descripcion del proceso
      ls_qryProceso STRING       --Esta variable permite extraer la información de la tabla cat_proceso

   --Limpiar el combo
   CALL cb.clear()
   LET li_contador = 0

   --Verifica la existencia de procesos para el "Registro de pagos"
   LET ls_qryProceso = "SELECT COUNT(*) ",
                       "FROM cat_proceso ",
                       "WHERE modulo_cod = 'pag'"

   PREPARE prp_conteoProc FROM ls_qryProceso
   EXECUTE prp_conteoProc INTO li_contador

   --Si existen procesos para el "Registro de pagos"
   IF li_contador > 0 THEN
      LET ls_qryProceso = "SELECT proceso_cod, proceso_desc ",
                          "FROM cat_proceso ",
                          "WHERE modulo_cod = 'pag'"

      PREPARE prp_procesos FROM ls_qryProceso
      DECLARE lcur_procesos CURSOR FOR prp_procesos 
      FOREACH lcur_procesos INTO li_procesoCod,lv_descrip 
         CALL cb.addItem(li_procesoCod,lv_descrip)
      END FOREACH
   END IF

END FUNCTION

{ ==========================================================================
Nombre: fn_llena_combo_operacion
Fecha creacion: 05 de Enero de 2011
Autor: GERARDO ALFONSO VEGA PAREDES
Narrativa del proceso que realiza:
Esta función carga los datos del combo que muestra las operaciones existentes
para el "Resgistro de Pagos"
============================================================================}
FUNCTION fn_llena_combo_operacion(lsi_tpoProceso)
   DEFINE 
      lsi_tpoProceso  SMALLINT,
      cb              ui.ComboBox,
      li_contador     INTEGER,
      li_operacionCod INTEGER,     --Número de proceso
      lv_descrip      VARCHAR(40), --Descripcion del proceso
      ls_qryProceso   STRING       --Esta variable permite extraer la información de la tabla cat_proceso

   --Se indica el nombre del objeto al cual hace refeencia
   --el combo en la forma(.per)
   LET cb = ui.ComboBox.forName("formonly.cmb_operacion")

   --Limpiar el combo
   CALL cb.clear()
   LET li_contador = 0

   --Verifica la existencia de procesos para el "Registro de pagos"
   LET ls_qryProceso = "SELECT COUNT(*) ",
                       "FROM cat_operacion ",
                       "WHERE proceso_cod = ?"

   PREPARE prp_conteoOperacion FROM ls_qryProceso
   EXECUTE prp_conteoOperacion USING lsi_tpoProceso
                                INTO li_contador

   --Si existen procesos para el "Registro de pagos"
   IF li_contador > 0 THEN
      LET ls_qryProceso = "SELECT opera_cod, opera_desc ",
                          "FROM   cat_operacion ",
                          "WHERE  proceso_cod = ?"

      PREPARE prp_operaciones FROM ls_qryProceso
      DECLARE lcur_operaciones CURSOR FOR prp_operaciones 
      FOREACH lcur_operaciones USING lsi_tpoProceso
                             INTO li_operacionCod,lv_descrip 
         CALL cb.addItem(li_operacionCod,lv_descrip)
      END FOREACH
   END IF

END FUNCTION
