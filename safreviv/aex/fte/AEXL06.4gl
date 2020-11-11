####################################################################################################
# Modulo       => AEX                                                                              #
# Programa     => AEXL06                                                                           #
# Objetivo     => Lanzador para cliente Individualiza Pagos    Aportaciones Extraordinarias        #
# Empresa      => Omnisys                                                                          #
# Autor        = > Jairo Giovanny Palafox Sanchez                                                  #
# Fecha        => 05/11/2020                                                                       #
####################################################################################################


DATABASE safre_viv

   DEFINE g_usuario              CHAR (20)
   DEFINE g_tipo_ejecucion       SMALLINT                      -- forma como se ejecutara el programa
   DEFINE g_nom_ventana          STRING                        -- título de la ventana
   DEFINE g_pid                  DECIMAL (9,0)                 -- ID del proceso
   DEFINE g_proceso_cod          LIKE cat_proceso.proceso_cod  -- código del proceso
   DEFINE g_opera_cod            LIKE cat_operacion.opera_cod  -- código de operacion
   DEFINE v_extension            STRING
   DEFINE v_nom_archivo          STRING
   DEFINE v_resultado            SMALLINT
   DEFINE g_folio                DECIMAL(10,0)


   
MAIN 

   DEFINE v_s_comando        STRING
   DEFINE v_ruta_ejecutable  LIKE seg_modulo.ruta_bin
   DEFINE v_ruta_listados    LIKE seg_modulo.ruta_listados

   LET g_usuario           = ARG_VAL (1)
   LET g_tipo_ejecucion    = ARG_VAL (2)     --forma como se ejecutara el programa
   LET g_nom_ventana       = ARG_VAL (3)

   LET g_proceso_cod = 502    --se asigna valor al proceso
   LET g_opera_cod   = 1      --se asigna la operacion
   LET v_extension   = "aex"     --se asigna el tipo de extenxion que se tomara de la ruta rescate

   --Obtiene ruta de los binarios
   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = 'aex'

   --Obtiene ruta listados batch
   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'bat'


   -- se inicia el log del programa
   CALL STARTLOG (g_usuario CLIPPED||".AEXL06.log")

   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion( 0,g_proceso_cod,g_opera_cod)   RETURNING v_resultado
   -- se verifica si la operacion en proceso es valida
   IF v_resultado <> 0 THEN  
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(v_resultado)
      DISPLAY "ERROR en fn_valida_operacion"
   ELSE
      -- se obtiene el folio
      CALL fn_genera_folio(g_proceso_cod, g_opera_cod, g_usuario)
      RETURNING g_folio  
      --Se genera PID del proceso
      CALL fn_genera_pid (g_proceso_cod, g_opera_cod, g_usuario) RETURNING g_pid
      
      CALL fn_inicializa_proceso(g_pid,
                                 g_proceso_cod,
                                 g_opera_cod,
                                 g_folio,
                                 'AEXL06',
                                 '',
                                 g_usuario)
            RETURNING v_resultado

      CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,
                                  g_opera_cod,
                                  g_folio,
                                  "AEXL06",
                                  v_nom_archivo,
                                  g_usuario)  RETURNING v_resultado

      LET v_s_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,
                        "/AEXWS03 ",g_usuario," ",g_pid," ",
                        g_proceso_cod," ",g_opera_cod," ",g_folio," '",v_nom_archivo,"' ",
                        " ' '  1>", v_ruta_listados CLIPPED ,
                        "/nohup:",g_pid USING "&&&&&",":",
                        g_proceso_cod USING "&&&&&",":",
                        g_opera_cod USING "&&&&&" ," 2>&1 &"

      RUN v_s_comando

      LET v_s_comando = "Se ejecutó carga de archivo"," ",
                        "Verificar en el monitor de proceso la ejecución el PID ", g_pid USING "<<<<<<<<<"
               
      DISPLAY "Ejecucion", v_s_comando
   END IF
END MAIN 
   