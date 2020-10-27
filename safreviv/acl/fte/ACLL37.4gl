--==============================================================================
-- Version                    => 1.0.0
-- Fecha ultima modificacion  => 17/05/2016
-- Proyecto          => SAFRE VIVIENDA
-- Autor             => GERARDO ALFONSO VEGA PAREDES
---------------------------------------------------------------------------------
-- Modulo            => ACL
-- Componente        => ACLL37
-- Funcionalidad     => Lanzador de extractor pendientes aclaratorio
--                      y pendientes adelantados (no confirmados)
-- Fecha inicio      => 17 de Mayo de 2016
--==============================================================================
--==============================================================================
-- Modificación      => Continuar con el desarrollo
-- Autor             => GERARDO ALFONSO VEGA PAREDES
-- Fecha             => 9 de Junio de 2016
--==============================================================================
 
DATABASE safre_viv

GLOBALS "ACLG02.4gl"     

GLOBALS
   DEFINE g_pid            LIKE bat_ctr_proceso.pid, 
          g_proceso_cod    LIKE cat_proceso.proceso_cod,
          g_opera_cod      LIKE cat_operacion.opera_cod,
          g_usuario_cod    LIKE seg_usuario.usuario_cod,
          g_tipo_ejecucion SMALLINT,
          g_s_titulo       STRING
END GLOBALS
     
MAIN 

   DEFINE v_tipo     CHAR (1)
       
   -- se recuperan parametros de entrada
   LET g_usuario_cod    = ARG_VAL(1) -- Recibe la variable de usuario
   LET g_tipo_ejecucion = ARG_VAL(2) -- Recibe el tipo de proceso
   LET g_s_titulo       = ARG_VAL(3) -- Recibe el nombre del programa

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF g_s_titulo IS NOT NULL THEN
      CALL ui.Interface.setText(g_s_titulo)
   END IF

   -- se asignan los valores de proceso y operacion
   LET g_proceso_cod = 111 --g_proceso_cod_acl_ext_pend
   LET g_opera_cod   = 1 -- generar archivo

   CLOSE WINDOW SCREEN

   --abre ventana para generar archivo
   OPEN WINDOW genera_archivo WITH FORM "ACLL371"
   INPUT BY NAME v_tipo
   ATTRIBUTES(WITHOUT DEFAULTS, UNBUFFERED)

      ON ACTION CANCEL
         EXIT INPUT

		 ON ACTION ACCEPT
         CALL fn_envia_operacion(v_tipo)
         EXIT INPUT
   END INPUT
   CLOSE WINDOW genera_archivo

END MAIN

FUNCTION fn_envia_operacion(p_tipo)
   DEFINE p_tipo             CHAR (1)
   DEFINE v_bandera          SMALLINT -- para verificar resultado de iniciar la operacion
   DEFINE v_comando          STRING
   DEFINE l_bat_ruta_listado CHAR(40)
   DEFINE v_ruta_origen      CHAR(40)
   DEFINE v_desc_salida      VARCHAR(100)
   DEFINE v_mensaje          STRING
   DEFINE v_folio            LIKE glo_folio.folio
   DEFINE v_nombre_archivo   LIKE glo_ctr_archivo.nombre_archivo

   -- el proceso no tiene folio
   LET v_folio = 0

   -- este proceso inicia por webservices, no tiene archivo
   LET v_nombre_archivo = "NA"
            
   SELECT ruta_listados
   INTO   l_bat_ruta_listado
   FROM   seg_modulo
   WHERE  modulo_cod = 'bat'
            
   SELECT ruta_bin
   INTO   v_ruta_origen
   FROM   seg_modulo
   WHERE  modulo_cod = 'acl'

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
   CALL fn_inicializa_proceso(g_pid,
                              g_proceso_cod,
                              g_opera_cod,
                              v_folio,
                              "ACLL37",
                              v_nombre_archivo,
                              g_usuario_cod) RETURNING v_bandera
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
                               "ACLS37",
                               "",
                               g_usuario_cod)
        RETURNING v_bandera
               
   -- se ejcuta el comando que genera el archivo de salida			   
   LET v_comando = "nohup fglrun ",v_ruta_origen CLIPPED,"/ACLS37.42r ",
                    g_usuario_cod CLIPPED, " ",
                    g_pid                , " ",
                    g_proceso_cod        , " ",
                    g_opera_cod          , " ",
                    p_tipo               , " ",
                    " 1>", l_bat_ruta_listado CLIPPED ,
                    "/nohup:",g_pid  USING "&&&&&",":",
                    g_proceso_cod    USING "&&&&&",":",
                    g_opera_cod      USING "&&&&&",
                    " 2>&1 &"
   --DISPLAY v_comando
   RUN v_comando
            
   CALL fn_mensaje("Atención","Se ha enviado la generacion del archivo.\nPuede revisar el avance del proceso en el monitor de ejecución de procesos","")

END FUNCTION