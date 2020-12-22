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
   DEFINE v_nom_archivo          STRING
   DEFINE v_resultado            SMALLINT
   DEFINE g_folio                DECIMAL(10,0)


PRIVATE DEFINE v_proceso_desc             CHAR(40)
PRIVATE DEFINE v_extension                CHAR(10)
PRIVATE DEFINE v_opera_desc               CHAR(40)
PRIVATE DEFINE v_layout                   SMALLINT
PRIVATE DEFINE v_usuario_proceso          CHAR(20)
PRIVATE DEFINE v_ruta_rescate             STRING
PRIVATE DEFINE v_ruta_listados            CHAR(40)

MAIN 

   DEFINE v_s_comando        STRING
   DEFINE v_ruta_ejecutable  LIKE seg_modulo.ruta_bin
   DEFINE v_ruta_listados    LIKE seg_modulo.ruta_listados


   LET g_usuario    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET g_folio          = ARG_VAL(5)
   LET v_nom_archivo    = ARG_VAL(6)

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

   CALL fn_recupera_inf_proceso(g_proceso_cod, g_opera_cod) 
                               RETURNING v_proceso_desc,v_extension, 
                                         v_opera_desc,v_layout, 
                                         v_ruta_rescate,v_ruta_listados,
                                         v_usuario_proceso 
                                         
   DISPLAY "*******************************************************************"
   DISPLAY " PROCESO            : ",v_proceso_desc
   DISPLAY " OPERACIÓN          : ",v_opera_desc
   DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY "*******************************************************************"

   -- se solicita el numero de folio asociado a la operacion
   -- parametros: proceso, operacion, usuario
   CALL fn_genera_folio(g_proceso_cod,g_opera_cod,g_usuario)
        RETURNING g_folio
    
   CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,
                               g_opera_cod,
                               g_folio,
                               "AEXL06",
                               v_nom_archivo,
                               g_usuario)  RETURNING v_resultado

    LET v_s_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,
                      "/AEXWS03 ",g_usuario," ",g_pid," ",
                      g_proceso_cod," ",g_opera_cod," ",g_folio," '",v_nom_archivo,"' ",
                      v_proceso_desc ," ", v_opera_desc   ,
                      " ' '  1>", v_ruta_listados CLIPPED ,
                      "/nohup:",g_pid USING "&&&&&",":",
                      g_proceso_cod USING "&&&&&",":",
                      g_opera_cod USING "&&&&&" ," 2>&1 &"

    RUN v_s_comando

    LET v_s_comando = "Se ejecuta individualiza pagos"," ",
                      "Verificar en el monitor de proceso la ejecución el PID ", g_pid USING "<<<<<<<<<"
               
    DISPLAY "Ejecucion: ", v_s_comando
   
END MAIN 
   