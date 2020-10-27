################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 25/01/2016                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISL51                                                   #
#Objetivo          => Generación de la interface de Aportaciones Subsecuentes  #
#                     Sin Conciliar                                            #
#Fecha inicio      => 22/01/2016                                               #
################################################################################
DATABASE safre_viv

GLOBALS
  DEFINE 
    g_pid                    LIKE bat_ctr_proceso.pid,     --ID del proceso
    g_proceso_cod            LIKE cat_proceso.proceso_cod, --codigo del proceso
    g_opera_cod              LIKE cat_operacion.opera_cod, --codigo de operacion
    p_operacion              SMALLINT
END GLOBALS
     
MAIN 
  DEFINE 
    p_usuario_cod            LIKE seg_usuario.usuario_cod, --clave usuario firmado
    p_tipo_ejecucion         SMALLINT,        --forma como ejecutara el programa
    p_s_titulo               STRING,          --titulo de la ventana
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

  --Validación que NO se tenga Preliquidación de Dispersión de Pagos ejecutándose
  IF f_existe_proceso_operacion_ejecutando(901, 1) THEN
     MENU "No se puede ejecutar" ATTRIBUTES ( STYLE="dialog", 
       COMMENT="Preliquidación de Dispersión de Pagos ejecutándose,\ningrese a esta opción cuando finalice",
       IMAGE="information" )
       ON ACTION salir
          RETURN
     END MENU
  END IF

    --Validación que NO se tenga Preliquidación de Dispersión de Pagos ejecutándose
  IF f_existe_proceso_operacion_ejecutando(902, 1) THEN
     MENU "No se puede ejecutar" ATTRIBUTES ( STYLE="dialog", 
       COMMENT="Avance de Pagos ejecutándose,\ningrese a esta opción cuando finalice",
       IMAGE="information" )
       ON ACTION salir
          RETURN
     END MENU
  END IF

    --Validación que NO se tenga Preliquidación de Dispersión de Pagos ejecutándose
  IF f_existe_proceso_operacion_ejecutando(920, 1) THEN
     MENU "No se puede ejecutar" ATTRIBUTES ( STYLE="dialog", 
       COMMENT="Dispersión Créditos en Cero ejecutándose,\ningrese a esta opción cuando finalice",
       IMAGE="information" )
       ON ACTION salir
          RETURN
     END MENU
  END IF

    --Validación que NO se tenga Preliquidación de Dispersión de Pagos ejecutándose
  IF f_existe_proceso_operacion_ejecutando(932, 1) THEN
     MENU "No se puede ejecutar" ATTRIBUTES ( STYLE="dialog", 
       COMMENT="Aportaciones Subsecuentes ejecutándose,\ningrese a esta opción cuando finalice",
       IMAGE="information" )
       ON ACTION salir
          RETURN
     END MENU
  END IF

  --Validación que NO se tenga Preliquidación de Dispersión de Pagos ejecutándose
  IF f_existe_proceso_operacion_ejecutando(933, 1) THEN
     MENU "No se puede ejecutar" ATTRIBUTES ( STYLE="dialog", 
       COMMENT="Aportaciones Subsecuentes Sin Conciliar ejecutándose,\ningrese a esta opción cuando finalice",
       IMAGE="information" )
       ON ACTION salir
          RETURN
     END MENU
  END IF

  LET g_proceso_cod = 933 --Avance con pago (Masivo)
  LET g_opera_cod   = 1   --Liquidacion
  LET p_operacion   = 1   --Ejecutar Interface de Aportaciones Subsecuentes Sin Conciliar

  LET v_tot_reg     = 0

            SELECT COUNT(*)
              INTO v_tot_reg
              FROM dis_ap_subsecuente da
             WHERE NOT EXISTS(
            SELECT de.folio_liquida
              FROM dis_interface_ef de
             WHERE de.folio_liquida = da.folio_liquida)
              
            IF v_tot_reg = 0     OR 
               v_tot_reg IS NULL THEN
               LET v_confirma = 0
               
               CALL fn_mensaje("Atención","No existen registros para generar la interface.",
                               "about")               
            ELSE
               CALL fn_ventana_confirma("Error", "¿Seguro que desea generar la interface de aportaciones subsecuentes sin conciliar?", "information")
               RETURNING v_confirma               
            END IF
   

         IF v_confirma <> 0 THEN
            CALL fn_envia_operacion(p_usuario_cod)            
         END IF    

END MAIN

FUNCTION fn_envia_operacion(p_usuario)  
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

  --DISPLAY "v_bandera: -",v_bandera,"-"
   
  IF ( v_bandera = 0 ) THEN
     CALL fn_genera_pid(g_proceso_cod, g_opera_cod, p_usuario) 
     RETURNING g_pid

     --DISPLAY "g_pid: -",g_pid,"-"

     CALL fn_inicializa_proceso(g_pid,
                                g_proceso_cod,
                                g_opera_cod,
                                v_folio,
                                "DISL51",
                                v_nombre_archivo,
                                p_usuario)  
     RETURNING v_bandera

     --DISPLAY "--v_bandera: -",v_bandera,"-"

     IF ( v_bandera = 0 ) THEN
        CALL fn_actualiza_opera_ini(g_pid,
                                    g_proceso_cod,
                                    g_opera_cod,
                                    v_folio,
                                    "DISS45",
                                    "",
                                    p_usuario)
        RETURNING v_bandera
        --DISPLAY "---v_bandera: -",v_bandera,"-"
                                 
        LET v_comando = "nohup fglrun ",v_ruta_origen CLIPPED,"/DISS45.42r ",
                        p_usuario CLIPPED, " ",
                        g_pid            , " ",
                        g_proceso_cod    , " ",
                        g_opera_cod      , " ",
                        v_folio          , " ",
                        " 1>", l_bat_ruta_listado CLIPPED ,
                        "/nohup:",g_pid  USING "&&&&&",":",
                        g_proceso_cod    USING "&&&&&",":",
                        g_opera_cod      USING "&&&&&",
                        " 2>&1 &"

        DISPLAY v_comando
        RUN v_comando
                  
        CALL fn_mensaje("Atención","Se ha enviado la interface.\nPuede revisar el avance del proceso en el monitor de ejecución de procesos con pid: "||g_pid,"")
     ELSE
        --Se obtiene la descripcion del parametro de salida
        SELECT descripcion
        INTO   v_desc_salida
        FROM   cat_bat_parametro_salida
        WHERE  cod_salida = v_bandera
         
        --Se construye el mensaje de error
        LET v_comando = "No se puede iniciar la operación. No se ha generado la interface.\nError: ", v_desc_salida CLIPPED
        CALL fn_mensaje("Atención",v_comando,"stop")
     END IF
  ELSE
     --No se puede ejecutar la operacion
     CALL fn_recupera_inconsis_opera(v_bandera) 
     RETURNING v_mensaje

     CALL fn_mensaje("Atención", v_mensaje, "stop")
  END IF
END FUNCTION