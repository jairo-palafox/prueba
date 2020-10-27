################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 27/03/2018                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISL27                                                   #
#Objetivo          => Generación de la interface de avances de pagos vs pagos  #
#                     qur compensan el avance (Por folio y masivo).            #
#Fecha inicio      => 28/07/2014                                               #
################################################################################
DATABASE safre_viv
GLOBALS
  DEFINE 
    g_pid                    LIKE bat_ctr_proceso.pid,     --ID del proceso
    g_proceso_cod            LIKE cat_proceso.proceso_cod, --Código del proceso
    g_opera_cod              LIKE cat_operacion.opera_cod, --Código de operación
    p_operacion              SMALLINT,
    g_sql_txt                STRING,
    v_proc_entra             SMALLINT,
    v_proc_val               SMALLINT,
    v_cod_conv               SMALLINT,
    v_desc_proc_val          CHAR(40),
    v_mensaje_val            STRING

END GLOBALS
     
MAIN 
  DEFINE 
    p_usuario_cod            LIKE seg_usuario.usuario_cod, --Clave usuario firmado
    p_tipo_ejecucion         SMALLINT,        --Forma como ejecutara el programa
    p_s_titulo               STRING,          --Titulo de la ventana
    v_folio                  DECIMAL(9,0),
    v_confirma               SMALLINT,
    v_tot_reg                DECIMAL(9,0)
       
  --Se recuperan parametros de entrada
  LET p_usuario_cod    = ARG_VAL(1) --Recibe la variable de usuario
  LET p_tipo_ejecucion = ARG_VAL(2) --Recibe el tipo de proceso
  LET p_s_titulo       = ARG_VAL(3) --Recibe el nombre del programa

  --Si se obtuvo el titulo, se pone como titulo de programa
  IF ( p_s_titulo IS NOT NULL ) THEN
     CALL ui.Interface.setText(p_s_titulo)
  END IF

  LET g_proceso_cod = 918 --Avances vs Compensación
  LET g_opera_cod   = 1   --Liquidacion
  LET p_operacion   = 2   --Ejecutar liquidacion

  --Validación que NO se tenga alguna operación de Dispersión de Pagos ejecutándose
  LET g_sql_txt = "SELECT a.cod_proceso_entra,   ",
                  "       a.cod_proceso_valida,  ",
                  "       b.proceso_desc         ",
                  "FROM   cat_convivencia_dis a, ",
                  "       cat_proceso b          ",
                  "WHERE  a.cod_proceso_entra  = ", g_proceso_cod,
                  "AND    a.cod_proceso_valida = b.proceso_cod ",
                  "AND    a.cod_convivencia    = 0             ",
                  "ORDER BY cod_proceso_valida   "
  PREPARE ps_val_proc FROM g_sql_txt
  DECLARE cur_val_proc CURSOR FOR ps_val_proc
  FOREACH cur_val_proc INTO v_proc_entra,
                            v_proc_val,
                            v_desc_proc_val
    IF f_existe_proceso_operacion_ejecutando(v_proc_val, "") THEN
       LET v_mensaje_val = "Proceso ", v_desc_proc_val CLIPPED, " ejecutándose,\ningrese a esta opción cuando finalice."
       MENU "No se puede ejecutar" 
         ATTRIBUTES ( STYLE="dialog",
         COMMENT= v_mensaje_val,
         IMAGE="information" )

         ON ACTION salir
            RETURN
       END MENU
    END IF
  END FOREACH

  LET v_tot_reg     = 0

  CLOSE WINDOW SCREEN

  OPEN WINDOW DISL271 WITH FORM "DISL271"
    INPUT BY NAME v_folio ATTRIBUTES(WITHOUT DEFAULTS, UNBUFFERED)
      ON ACTION CANCEL
         EXIT INPUT

      ON ACTION ACCEPT
         IF v_folio IS NULL THEN
            CALL fn_ventana_confirma("Error", "Seguro que desea ejecutar todo el histórico", "information")
            RETURNING v_confirma
            IF v_confirma <> 0 THEN
               LET v_folio = 0
            END IF 
         ELSE
            SELECT COUNT(*)
            INTO   v_tot_reg
            FROM   dis_det_avance_pago av   
            WHERE  av.folio  = v_folio
            -- AND    av.estado = 30
            IF v_tot_reg = 0     OR 
               v_tot_reg IS NULL THEN
               LET v_confirma = 0
               
               CALL fn_mensaje("Atención","No existen registros para el folio capturado",
                               "about")
               NEXT FIELD v_folio
            ELSE
               LET v_confirma = 1
            END IF
         END IF

         IF v_confirma <> 0 THEN
            CALL fn_envia_operacion(v_folio,p_usuario_cod)
            EXIT INPUT
         END IF 
    END INPUT
  CLOSE WINDOW DISL271

END MAIN

FUNCTION fn_envia_operacion(p_folio,p_usuario)
  DEFINE p_folio             DECIMAL(9,0)
  DEFINE p_usuario           CHAR(20)
  DEFINE v_bandera           SMALLINT --Para verificar resultado de iniciar la operacion
  DEFINE v_comando           STRING
  DEFINE l_bat_ruta_listado  CHAR(40)
  DEFINE v_ruta_origen       CHAR(40)
  DEFINE v_desc_salida       VARCHAR(100)
  DEFINE v_mensaje           STRING
  DEFINE v_folio             LIKE glo_folio.folio
  DEFINE v_nombre_archivo    LIKE glo_ctr_archivo.nombre_archivo

  --El proceso no tiene folio
  LET v_folio = 0

  --Este proceso inicia por webservices, no tiene archivo
  LET v_nombre_archivo = "NA"
            
  SELECT ruta_listados
  INTO   l_bat_ruta_listado
  FROM   seg_modulo
  WHERE  modulo_cod = 'bat'
            
  SELECT ruta_bin
  INTO   v_ruta_origen
  FROM   seg_modulo
  WHERE  modulo_cod = 'dis'

  --Se verifica si se puede continuar con la operacion
  LET v_bandera = fn_valida_operacion(0, g_proceso_cod, g_opera_cod)
   
  IF ( v_bandera = 0 ) THEN
     CALL fn_genera_pid(g_proceso_cod, g_opera_cod, p_usuario) 
     RETURNING g_pid

     CALL fn_inicializa_proceso(g_pid,
                                g_proceso_cod,
                                g_opera_cod,
                                v_folio,
                                "DISL27",
                                v_nombre_archivo,
                                p_usuario)  
     RETURNING v_bandera

     IF ( v_bandera = 0 ) THEN
        CALL fn_actualiza_opera_ini(g_pid,
                                    g_proceso_cod,
                                    g_opera_cod,
                                    v_folio,
                                    "DISS30",
                                    "",
                                    p_usuario)
        RETURNING v_bandera
                                 
        LET v_comando = "nohup fglrun ",v_ruta_origen CLIPPED,"/DISS30.42r ",
                        p_usuario CLIPPED, " ",
                        g_pid            , " ",
                        g_proceso_cod    , " ",
                        g_opera_cod      , " ",
                        p_folio          , " ",
                        " 1>", l_bat_ruta_listado CLIPPED ,
                        "/nohup:",g_pid  USING "&&&&&",":",
                        g_proceso_cod    USING "&&&&&",":",
                        g_opera_cod      USING "&&&&&",
                        " 2>&1 &"
        RUN v_comando
                  
        CALL fn_mensaje("Atención","Se ha enviado la interface.\nPuede revisar el avance del proceso en el monitor de ejecución de procesos","")
     ELSE
        --Se obtiene la descripcion del parametro de salida
        SELECT descripcion
        INTO   v_desc_salida
        FROM   cat_bat_parametro_salida
        WHERE  cod_salida = v_bandera
         
        --Se construye el mensaje de error
        LET v_comando = "No se puede iniciar la operación. No se ha enviado el proceso de liquidación.\nError: ", v_desc_salida CLIPPED
        CALL fn_mensaje("Atención",v_comando,"stop")
     END IF
  ELSE
     --No se puede ejecutar la operacion
     CALL fn_recupera_inconsis_opera(v_bandera) 
     RETURNING v_mensaje

     CALL fn_mensaje("Atención", v_mensaje, "stop")
  END IF
END FUNCTION