--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 18/04/2012
--===============================================================

#########################################################################################
#Modulo       => PAG                                                                    #
#Programa     => PAGL13                                                                 #
#Objetivo     => Programa lanzador de las rutinas de carga, integración y               #
#                preliquidacion de recibos de pagos solo infonavit                      #
#Fecha inicio => Diciembre 28, 2011                                                     #
#########################################################################################
DATABASE safre_viv
GLOBALS "PAGG01.4gl"  ---archivo de variables globales proceso_cod, opera_cod
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid --  ID del proceso
       --g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       --g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
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
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, --Clave de usuario
       p_tpo_ejecucion  INTEGER,   --Tipo de ejecucion
                               -- 1 En linea
                               -- 2 Batch
       p_cad_ventana    STRING,    --Cadena de la ventana
       lsi_tpoProceso   SMALLINT,
       lsi_tpoOperacion SMALLINT
      ,v_comando          STRING
      ,v_ruta_ejecutable  STRING
      ,v_ruta_listados    STRING
      ,v_num_folio        INTEGER
      ,v_nom_archivo      STRING
      ,v_cadena_pid       STRING
      --,v_cadena_proc      STRING
      --,v_cadena_opera     STRING
      ,v_estatus          SMALLINT  
      --,g_opera_cod_integracion  SMALLINT
      ,v_ruta_vacia             STRING
      --,v_tipo_archivo     SMALLINT  

   --Inicializacion de variables
   INITIALIZE lsi_tpoProceso, lsi_tpoOperacion TO NULL

   -- se dan las variables de proceso y operacion
   --LET g_proceso_cod = 1403 -- Recaudación solo INFONAVIT
   --LET g_opera_cod   = 1 -- Recaudación solo INFONAVIT
   --LET g_opera_cod_integracion = 2  --Operacion de integracion
   LET v_num_folio   = 0
   LET v_nom_archivo = "N/A"

   --CALL fn_tipo_archivo_sinf(p_cad_ventana) RETURNING v_tipo_archivo

   --si selecciono un tipo de archivo
   {IF v_tipo_archivo IS NOT NULL THEN
      IF v_tipo_archivo = 1 THEN
         LET g_proceso_cod = 3 -- Recaudación solo INFONAVIT Trabajadores
      END IF
      IF v_tipo_archivo = 1 THEN
         LET g_proceso_cod = 3 -- Recaudación solo INFONAVIT Patronal
      END IF
  }  
      --Se asigna el tçitulo de la ventana
      IF ( p_cad_ventana IS NOT NULL ) THEN
         CALL ui.Interface.setText(p_cad_ventana)
      END IF

      -- se verifica si ya se termino el proceso anterior, si no,
      -- no se permite capturar nuevos saldos
      LET v_estatus = fn_valida_operacion(0,g_proceso_cod_pag_registro_pagos_SINF,g_opera_cod_pag_carga)
      IF ( v_estatus = 0 ) THEN
         --Se genera el pid
         {CALL fn_genera_pid(g_proceso_cod
                           ,g_opera_cod
                           ,p_usuario_cod)
                           RETURNING g_pid
         --Se da de alta el proceso de carga y colo todos los demas procesos como listo
         CALL fn_inicializa_proceso(g_pid
                                   ,g_proceso_cod
                                   ,g_opera_cod
                                   ,0
                                   ,"PAGL13"
                                   ,""
                                   ,p_usuario_cod)
                                   RETURNING v_estatus}
         --sE OBTIENEN las rutas de los ejecutables
         CALL fn_rutas("pag") RETURNING v_ruta_ejecutable, v_ruta_vacia
         CALL fn_rutas("bat") RETURNING v_ruta_vacia, v_ruta_listados

         -- se crean las cadenas para el nombre del archivo log
         LET v_cadena_pid   = g_pid         USING "&&&&&"
         --LET v_cadena_proc  = g_proceso_cod_pag_registro_pagos_SINF USING "&&&&&"
         --LET v_cadena_opera = g_opera_cod_pag_integracion   USING "&&&&&"
         {
         --Se arma el comando para que el proceso de carga genere automaticamente la inegracion
         LET v_comando = "nohup fglrun "
                        ,v_ruta_ejecutable CLIPPED
                        ,"/PAGP14.42r "
                        ,p_usuario_cod," "
                        ,g_pid, " "
                        ,g_proceso_cod," "
                        ,g_opera_cod_integracion," "
                        ,v_num_folio, " "
                        ,v_nom_archivo
                        ," 1>", v_ruta_listados CLIPPED
                        ,"/nohup:",v_cadena_pid,":", v_cadena_proc,":",
                          v_cadena_opera ," 2>&1 &"
         --Se ejecutan los displays
         CALL fn_display_proceso(0,"CARGA")
         }
         LET v_comando = "N/A"
         CALL fn_carga_archivo(g_pid, g_proceso_cod_pag_registro_pagos_SINF, g_opera_cod_pag_carga, p_tpo_ejecucion,"PAGL13",v_comando, p_usuario_cod, TRUE)
              RETURNING v_estatus;
         --Se ejecutan los displays
         CALL fn_display_proceso(1,"CARGA")
      --END IF
   --END IF
   ELSE
      DISPLAY "El estatus es:",v_estatus
      DISPLAY fn_recupera_inconsis_opera(v_estatus)
      CALL fn_muestra_inc_operacion(v_estatus)
      {MENU
        COMMAND "Salir"
           EXIT MENU
      END MENU}
   END IF

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

{ ==========================================================================
Nombre: fn_tipo_archivo_sinf
Fecha creacion: 29 de Diciembre de 2011
Autor: David Miguel Garibay Rivera
Narrativa del proceso que realiza:
Indica que tipo de archivo se va a cargar para solo infonavit

Parametros de Entrada:
-

Parámetros de salida;
-

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

============================================================================}
FUNCTION fn_tipo_archivo_sinf(p_cad_ventana)
   DEFINE v_tipo_archivo CHAR(50)
   DEFINE p_cad_ventana  CHAR(50)
   
   OPEN WINDOW w_tipo_archivo_sinf WITH FORM "PAGL131"
      --Se asigna el tçitulo de la ventana
      IF ( p_cad_ventana IS NOT NULL ) THEN
         CALL ui.Interface.setText(p_cad_ventana)
      END IF
   
      INPUT v_tipo_archivo FROM RadioGroup1 ATTRIBUTES (UNBUFFERED)
         ON ACTION ACCEPT
            IF v_tipo_archivo IS NULL THEN
               CONTINUE INPUT
            ELSE
               EXIT INPUT
            END IF
         ON ACTION CANCEL
            LET v_tipo_archivo = NULL
            EXIT INPUT
      END INPUT
   CLOSE WINDOW w_tipo_archivo_sinf

   RETURN v_tipo_archivo
   
END FUNCTION
