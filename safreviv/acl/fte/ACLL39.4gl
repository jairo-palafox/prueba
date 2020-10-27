-----------------------------------------------------------------------------------------
-- Modulo        => ACL                                                                    
-- Componente    => ACLL38
-- Objetivo      => Lanzador que ejecuta programa ACLC33 que genera reporte 
--               => y archivo de rechazos basura.
-- Autor         => GERARDO ALFONSO VEGA PAREDES                                           
-- Fecha inicio  => 4 de Octubre de 2018
-- Requerimiento => saci2018-68
-----------------------------------------------------------------------------------------
-- Modificación => 
-- Fehca        => 
-- Autor        => 
-- Clave cambio => 
-----------------------------------------------------------------------------------------

DATABASE safre_viv

GLOBALS "ACLG02.4gl" --archivo de variables globales proceso_cod

GLOBALS
   DEFINE g_pid            LIKE bat_ctr_proceso.pid,      --  ID del proceso
          g_proceso_cod    LIKE cat_proceso.proceso_cod,  -- codigo del proceso
          g_opera_cod      LIKE cat_operacion.opera_cod,  -- codigo de operacion
          g_usuario_cod    LIKE seg_usuario.usuario_cod,  -- clave usuario firmado
          g_tipo_ejecucion SMALLINT,                      -- forma como ejecutara el programa
          g_s_titulo       STRING                         -- titulo de la ventana
          
   DEFINE v_nombre_archivo CHAR(40)

   DEFINE  
      v_ventana ui.Window,
      v_forma   ui.form,
      p_ventana STRING, 
      g_usuario_cod LIKE seg_usuario.usuario_cod,
      v_i_cont_registros INTEGER
          
END GLOBALS
     
MAIN 

   DEFINE v_radio       CHAR(15)
   DEFINE v_folio       DECIMAL(9,0)

   -- se recuperan parametros de entrada
   LET g_usuario_cod    = ARG_VAL(1) -- Recibe la variable de usuario
   LET g_tipo_ejecucion = ARG_VAL(2) -- Recibe el tipo de proceso
   LET g_s_titulo       = ARG_VAL(3) -- Recibe el nombre del programa

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF g_s_titulo IS NOT NULL THEN
      CALL ui.Interface.setText(g_s_titulo)
   END IF

   -- se asignan los valores de proceso y operacion
   LET g_proceso_cod = g_proceso_cod_acl_rechazos_dups
   LET g_opera_cod   = 1        -- generar reporte

--====== SECCIÓN NUEVA ========
   CLOSE WINDOW SCREEN

   OPEN WINDOW ventana01 WITH FORM "ACLL391"

   -- Se asigna el titulo de la ventana
   LET v_ventana = ui.Window.getCurrent()
   LET v_forma = v_ventana.getForm() 
   
   IF p_ventana IS NOT NULL THEN
      CALL ui.Interface.setText(p_ventana)         
      CALL v_ventana.setText(p_ventana)
   END IF

   INPUT v_radio WITHOUT DEFAULTS   
      FROM radiogroup1 ATTRIBUTES (UNBUFFERED ,ACCEPT=FALSE)
      
      -- Se evita seleccion en nulo de los criterio con la forma
      BEFORE INPUT
        LET v_radio = NULL
        LET v_folio = -1

      ON CHANGE radiogroup1
         IF v_radio = 1 THEN
         	LET g_proceso_cod = 102
         END IF

      ON ACTION ACCEPT
         IF v_radio IS NULL THEN
            CALL fn_mensaje("Consulta","Debes elegir Rechazos duplicados","about")
            LET v_radio = NULL
            NEXT FIELD radiogroup1
        END IF

        LET v_folio = 0

        CALL Ejecuta_reporte(v_radio,v_folio)
    
      ON ACTION CANCEL
        EXIT INPUT

   END INPUT

   CLOSE WINDOW ventana01

END MAIN

FUNCTION Ejecuta_reporte(v_radio,v_folio)
   DEFINE v_radio CHAR(15)
   DEFINE v_folio DECIMAL(9,0)

   DEFINE v_bandera          SMALLINT -- para verificar resultado de iniciar la operacion
   DEFINE v_comando          STRING
   DEFINE l_bat_ruta_listado CHAR(40)
   DEFINE v_ruta_origen      CHAR(40)
   DEFINE v_desc_salida      VARCHAR(100)
   DEFINE v_mensaje          STRING

   SELECT ruta_listados
   INTO   l_bat_ruta_listado
   FROM   seg_modulo
   WHERE  modulo_cod = 'bat'
            
   SELECT ruta_bin,
          USER
   INTO   v_ruta_origen,
          g_usuario_cod
   FROM   seg_modulo
   WHERE  modulo_cod = 'acl'

   LET g_proceso_cod = 114
   
    -- se valida si se puede continuar con la operacion
   LET v_bandera = fn_valida_operacion(0,g_proceso_cod,g_opera_cod)
   
   IF v_bandera <> 0 THEN
      -- no se puede ejecutar la operacion
      CALL fn_recupera_inconsis_opera(v_bandera) RETURNING v_mensaje
      CALL fn_mensaje("Atención", v_mensaje, "stop")
      RETURN
   END IF 

   LET g_opera_cod = 1
   -- se genera el pid
   CALL fn_genera_pid(g_proceso_cod, g_opera_cod, g_usuario_cod) RETURNING g_pid

   LET v_nombre_archivo = "NA"
   
   --validación para iniciar el proceso
   CALL fn_inicializa_proceso(g_pid             ,
                              g_proceso_cod     ,
                              g_opera_cod       ,
                              v_folio           ,
                              "ACLL39"          ,
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
                            "ACLC34",
                            v_nombre_archivo,
                            g_usuario_cod)
        RETURNING v_bandera
               
   -- se ejcuta el comando que genera el archivo de salida			   
   LET v_comando = "nohup fglrun ",v_ruta_origen CLIPPED,"/ACLC34.42r ",
                    g_usuario_cod CLIPPED, " ",
                    g_pid                , " ",
                    g_proceso_cod        , " ",
                    g_opera_cod          , " ",
                    v_nombre_archivo     , " ",
                    v_radio              , " ",
                    v_folio              , " ",
                    " 1>", l_bat_ruta_listado CLIPPED ,
                    "/nohup:",g_pid  USING "&&&&&",":",
                    g_proceso_cod    USING "&&&&&",":",
                    g_opera_cod      USING "&&&&&",
                    " 2>&1 &"
   DISPLAY v_comando
   RUN v_comando
            
   CALL fn_mensaje("Atención","Se ha enviado la generacion del archivo.\nPuede revisar el avance del proceso en el monitor de ejecución de procesos","")

END FUNCTION