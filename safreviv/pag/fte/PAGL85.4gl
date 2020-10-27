-----------------------------------------------------------------------------------------
-- Modulo        => PAG                                                                    
-- Componente    => PAGL85
-- Objetivo      => Lanzador que ejecuta programa PAGC85 que genera cifras globales LQINFO
-- Autor         => GERARDO ALFONSO VEGA PAREDES                                           
-- Fecha inicio  => 06 de Junio de 2018
-- Requerimiento => saci2018-13
-----------------------------------------------------------------------------------------
-- Modificación => 
-- Fehca        => 
-- Autor        => 
-- Clave cambio => 
-----------------------------------------------------------------------------------------

DATABASE safre_viv

GLOBALS "PAGG01.4gl" --archivo de variables globales proceso_cod

GLOBALS
   DEFINE g_pid            LIKE bat_ctr_proceso.pid,      --  ID del proceso
          g_proceso_cod    LIKE cat_proceso.proceso_cod,  -- codigo del proceso
          g_opera_cod      LIKE cat_operacion.opera_cod,  -- codigo de operacion
          g_usuario_cod    LIKE seg_usuario.usuario_cod,  -- clave usuario firmado
          g_tipo_ejecucion SMALLINT,                      -- forma como ejecutara el programa
          g_s_titulo       STRING                         -- titulo de la ventana
          
   DEFINE v_nombre_archivo CHAR(40) 
          
END GLOBALS
     
MAIN 

   DEFINE v_combo_archivo  ui.ComboBox
          
   -- se recuperan parametros de entrada
   LET g_usuario_cod    = ARG_VAL(1) -- Recibe la variable de usuario
   LET g_tipo_ejecucion = ARG_VAL(2) -- Recibe el tipo de proceso
   LET g_s_titulo       = ARG_VAL(3) -- Recibe el nombre del programa

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF g_s_titulo IS NOT NULL THEN
      CALL ui.Interface.setText(g_s_titulo)
   END IF

   -- se asignan los valores de proceso y operacion
   LET g_proceso_cod = g_proceso_reg_pag_cifras_lqinfo
   LET g_opera_cod   = 1        -- generar reporte

   CLOSE WINDOW SCREEN

   OPEN WINDOW pantalla_consulta WITH FORM "PAGL851"

   INPUT v_nombre_archivo WITHOUT DEFAULTS FROM archivo ATTRIBUTE (UNBUFFERED, ACCEPT=FALSE, CANCEL=FALSE)
      BEFORE INPUT

      LET  v_combo_archivo = ui.ComboBox.forName("archivo")
      CALL fn_llena_combo_archivo(v_combo_archivo)

      ON ACTION ACEPTAR
         IF v_nombre_archivo IS NOT NULL THEN
            CALL fn_crea_reporte()
            DISPLAY "SE CREO"
         ELSE
            CALL fn_mensaje("Atención","Debe capturar un archivo","stop")
            CONTINUE INPUT
         END IF

      ON ACTION CANCELAR
         EXIT INPUT

      END INPUT

      CLOSE WINDOW pantalla_consulta

END MAIN

FUNCTION fn_llena_combo_archivo(p_combo_archivo)

   DEFINE p_combo_archivo  ui.ComboBox,
          v_consulta       STRING,
          v_cadena_archivo STRING

   CALL p_combo_archivo.clear()

   LET v_consulta = " SELECT nombre_archivo ",
                    " FROM   glo_ctr_archivo",
                    " WHERE  proceso_cod = 1401 ",
                    " AND    estado     IN (1,2) ",     -- cargado o integrado
                    " ORDER BY nombre_archivo DESC"
   PREPARE prp_recupera_archivos FROM v_consulta
   DECLARE cur_recupera_archivos CURSOR FOR prp_recupera_archivos

   FOREACH cur_recupera_archivos INTO v_nombre_archivo
      LET v_cadena_archivo = v_nombre_archivo CLIPPED
      CALL p_combo_archivo.addItem(v_nombre_archivo,v_cadena_archivo)

   END FOREACH
   FREE cur_recupera_archivos

END FUNCTION

FUNCTION fn_crea_reporte()
   DEFINE v_bandera          SMALLINT -- para verificar resultado de iniciar la operacion
   DEFINE v_comando          STRING
   DEFINE l_bat_ruta_listado CHAR(40)
   DEFINE v_ruta_origen      CHAR(40)
   DEFINE v_desc_salida      VARCHAR(100)
   DEFINE v_mensaje          STRING
   DEFINE v_folio            LIKE glo_folio.folio

   -- el proceso no tiene folio
   LET v_folio = 0

   SELECT ruta_listados
   INTO   l_bat_ruta_listado
   FROM   seg_modulo
   WHERE  modulo_cod = 'bat'
            
   SELECT ruta_bin
   INTO   v_ruta_origen
   FROM   seg_modulo
   WHERE  modulo_cod = 'pag'

    -- se valida si se puede continuar con la operacion
   LET v_bandera = fn_valida_operacion(0,g_proceso_cod,g_opera_cod)
   
   IF v_bandera <> 0 THEN
      -- no se puede ejecutar la operacion
      CALL fn_recupera_inconsis_opera(v_bandera) RETURNING v_mensaje
      CALL fn_mensaje("Atención", v_mensaje, "stop")
      RETURN
   END IF 

   -- se genera el pid
   CALL fn_genera_pid(g_proceso_cod, g_opera_cod, g_usuario_cod) RETURNING g_pid
   
   --validación para iniciar el proceso
   CALL fn_inicializa_proceso(g_pid             ,
                              g_proceso_cod     ,
                              g_opera_cod       ,
                              v_folio           ,
                              "PAGL85"          ,
                              v_nombre_archivo  ,
                              g_usuario_cod)  RETURNING v_bandera
   IF v_bandera <> 0 THEN
      -- se obtiene la descripcion del parametro de salida
      SELECT descripcion
      INTO   v_desc_salida
      FROM   cat_bat_parametro_salida
      WHERE  cod_salida = v_bandera
      
      -- se construye el mensaje de error
      LET v_comando = "No se puede iniciar la operación.\nError: ", v_desc_salida CLIPPED
      CALL fn_mensaje("Atención",v_comando,"stop")
      RETURN
   END IF

   -- se inicia la operacion
   CALL fn_actualiza_opera_ini(g_pid,
                            g_proceso_cod,
                            g_opera_cod,
                            v_folio,
                            "PAGC85",
                            v_nombre_archivo,
                            g_usuario_cod)
        RETURNING v_bandera
               
   -- se ejcuta el comando que genera el archivo de salida			   
   LET v_comando = "nohup fglrun ",v_ruta_origen CLIPPED,"/PAGC85.42r ",
                    g_usuario_cod CLIPPED, " ",
                    g_pid                , " ",
                    g_proceso_cod        , " ",
                    g_opera_cod          , " ",
                    v_nombre_archivo     , " ",
                    " 1>", l_bat_ruta_listado CLIPPED ,
                    "/nohup:",g_pid  USING "&&&&&",":",
                    g_proceso_cod    USING "&&&&&",":",
                    g_opera_cod      USING "&&&&&",
                    " 2>&1 &"
   --DISPLAY v_comando
   RUN v_comando
            
   CALL fn_mensaje("Atención","Se ha enviado la generacion del archivo.\nPuede revisar el avance del proceso en el monitor de ejecución de procesos","")

END FUNCTION