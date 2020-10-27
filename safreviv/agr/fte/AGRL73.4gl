######################################################################
#Modulo            => AGR                                            #
#Programa          => AGRL73                                         #
#Objetivo          => Programa lanzador que ejecuta la  validación   #
#                     del archivo actualización solicitud saldo.     #
#Autor             => Emilio Abarca, EFP                             #
#Fecha inicio      => 24/Agosto/2018                                 #
######################################################################

DATABASE safre_viv

GLOBALS 
   DEFINE g_usuario           CHAR(20)
   DEFINE g_titulo            STRING
   DEFINE g_tipo_ejecucion    SMALLINT
   DEFINE v_ruta_binaria      CHAR(40)
   DEFINE g_proceso_cod       SMALLINT  
   DEFINE g_opera_cod         SMALLINT
   DEFINE g_pid               DECIMAL(9,0)
   DEFINE r_b_valida          SMALLINT
   DEFINE v_nom_archivo       CHAR(40)
   DEFINE v_r_comando         STRING
   DEFINE v_programa_cod      CHAR(10)
   DEFINE v_b_continua        SMALLINT
   DEFINE v_folio             LIKE bat_ctr_proceso.folio
  
END GLOBALS

MAIN 

   LET g_usuario          = ARG_VAL  (1)
   LET g_tipo_ejecucion   = ARG_VAL  (2)
   LET g_titulo           = ARG_VAL  (3)
   LET g_proceso_cod      = 350  -- ACTUALIZACIÓN MARCA SOLICITUD SALDO
   LET g_opera_cod        = 1    -- VALIDA ARCHIVO SOLICITUD SALDO

   IF (g_titulo IS NOT NULL) THEN
      CALL ui.Interface.setText(g_titulo)
   END IF

   CALL STARTLOG(g_usuario CLIPPED|| ".AGRL73.log")

   SELECT ruta_bin
     INTO v_ruta_binaria
     FROM seg_modulo
    WHERE modulo_cod = 'agr'

   -- Se invoca la funcion que valida la siguiente operacion

   LET g_pid = 0  -- > para la validación del primer proceso no es necesario el pid

   CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod) RETURNING r_b_valida

   IF(r_b_valida = 0) THEN

      -- se desconoce el nombre del archivo por lo tanto se envia No Aplica
      LET v_nom_archivo    = "N/A"
      LET v_programa_cod   = "AGRL73"
      LET v_folio          = 0
      LET g_tipo_ejecucion = 2

      -- se crea el comando que ejecuta el modulo que reliza la insercion del registro de control
      LET v_r_comando = " fglrun ",v_ruta_binaria CLIPPED,"/AGRP52 ",
                                  g_usuario," ",
                                  g_pid," ",
                                  g_proceso_cod," ",
                                  g_opera_cod," ",
                                  v_folio," ",
                                  v_nom_archivo
                                  --, " 1>> ",
                                  --v_c_ruta_list_bat CLIPPED,
                                  --"/nohup:",v_d_pid USING "&&&&&",":",
                                  --v_i_proceso_cod USING "&&&&&",":",
                                  --v_i_opera_cod USING "&&&&&",
                                  --" 2>&1 &"

       -- Se invoca la funcion que valida y carga la informacion en la tabla temporal
      CALL fn_carga_archivo(g_pid, g_proceso_cod, g_opera_cod,g_tipo_ejecucion, v_programa_cod,v_r_comando, g_usuario, TRUE) RETURNING v_b_continua
   ELSE
      -- Muestra mensaje en caso de no poder continuar con la operación
      CALL fn_muestra_inc_operacion(r_b_valida)
   END IF
   
END MAIN 





 