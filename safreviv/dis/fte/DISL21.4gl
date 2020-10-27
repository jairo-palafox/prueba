################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 05/05/2014                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISL21                                                   #
#Objetivo          => Generaci�n del cierre de pagos abiertos                  #
#Fecha inicio      => 05/05/2014                                               #
################################################################################
DATABASE safre_viv
GLOBALS
  DEFINE 
    g_pid                    LIKE bat_ctr_proceso.pid,     --ID del proceso
    g_proceso_cod            LIKE cat_proceso.proceso_cod, --Codigo del proceso
    g_opera_cod              LIKE cat_operacion.opera_cod, --Codigo de operacion
    p_operacion              SMALLINT
END GLOBALS
     
MAIN 
  DEFINE 
    p_usuario_cod            LIKE seg_usuario.usuario_cod, --Clave usuario firmado
    p_tipo_ejecucion         SMALLINT, --Forma como ejecutara el programa
    p_s_titulo               STRING,   --Titulo de la ventana
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

  --Validaci�n que NO se tenga Preliquidaci�n de Dispersi�n de Pagos ejecut�ndose
  IF f_existe_proceso_operacion_ejecutando(901, 1) THEN
     MENU "No se puede ejecutar" ATTRIBUTES ( STYLE="dialog", 
       COMMENT="Preliquidaci�n de Dispersi�n de Pagos ejecut�ndose,\ningrese a esta opci�n cuando finalice",
       IMAGE="information" )
       ON ACTION salir
          RETURN
     END MENU
  END IF

  LET g_proceso_cod = 914 --Cierre avance con pago
  LET g_opera_cod   = 1   --Liquidacion
  LET p_operacion   = 2   --Ejecutar liquidacion

  LET v_tot_reg     = 0

  CLOSE WINDOW SCREEN

  OPEN WINDOW DISL211 WITH FORM "DISL211"
    INPUT BY NAME v_folio ATTRIBUTES(WITHOUT DEFAULTS, UNBUFFERED)
      ON ACTION CANCEL
         EXIT INPUT

      ON ACTION ACCEPT
         IF v_folio IS NULL THEN
            CALL fn_ventana_confirma("Error", "Seguro que desea ejecutar el cierre de los avances abiertos", "information")
            RETURNING v_confirma
            IF v_confirma <> 0 THEN
               LET v_folio = 0
            END IF 
         ELSE
            SELECT COUNT(*)
            INTO   v_tot_reg
            FROM   dis_det_avance_pago av   
            WHERE  av.folio  = v_folio
            AND    av.estado = 30
            IF v_tot_reg = 0     OR 
               v_tot_reg IS NULL THEN
               LET v_confirma = 0
               
               CALL fn_mensaje("Atenci�n","No existen registros para el folio capturado",
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
  CLOSE WINDOW DISL211

END MAIN

FUNCTION fn_envia_operacion(p_folio,p_usuario)
  DEFINE p_folio             DECIMAL(9,0)
  DEFINE v_folio_cie         DECIMAL(9,0)
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

     --Enlaza el folio referencia 
     CALL fn_genera_folio_dis(g_proceso_cod, g_opera_cod, p_folio, p_usuario)
     RETURNING v_folio_cie

     DISPLAY "Folio Cierre Avances: ", v_folio_cie

     CALL fn_inicializa_proceso(g_pid,
                                g_proceso_cod,
                                g_opera_cod,
                                v_folio_cie,
                                "DISL21",
                                v_nombre_archivo,
                                p_usuario)  
     RETURNING v_bandera

     IF ( v_bandera = 0 ) THEN
        CALL fn_actualiza_opera_ini(g_pid,
                                    g_proceso_cod,
                                    g_opera_cod,
                                    v_folio_cie,
                                    "DISP02",
                                    "",
                                    p_usuario)
        RETURNING v_bandera
                                 
        LET v_comando = "nohup fglrun ",v_ruta_origen CLIPPED,"/DISP02.42r ",
                        p_usuario CLIPPED, " ",
                        g_pid            , " ",
                        g_proceso_cod    , " ",
                        g_opera_cod      , " ",
                        p_folio          , " ",
                        v_folio_cie      , " ",
                        " 1>", l_bat_ruta_listado CLIPPED ,
                        "/nohup:",g_pid  USING "&&&&&",":",
                        g_proceso_cod    USING "&&&&&",":",
                        g_opera_cod      USING "&&&&&",
                        " 2>&1 &"
        RUN v_comando
                  
        --Se construye el mensaje
        LET v_comando = "Se ha enviado el cierre de los avances abiertos con el PID: ", g_pid CLIPPED,
                        "\n Puede revisar el avance del proceso en el monitor de ejecuci�n de procesos"
        CALL fn_mensaje("Atenci�n",v_comando,"information")

     ELSE
        --Se obtiene la descripcion del parametro de salida
        SELECT descripcion
        INTO   v_desc_salida
        FROM   cat_bat_parametro_salida
        WHERE  cod_salida = v_bandera
         
        --Se construye el mensaje de error
        LET v_comando = "No se puede iniciar la operaci�n. No se ha enviado el proceso de liquidaci�n.\nError: ", v_desc_salida CLIPPED
        CALL fn_mensaje("Atenci�n",v_comando,"stop")
     END IF
  ELSE
     --No se puede ejecutar la operacion
     CALL fn_recupera_inconsis_opera(v_bandera) 
     RETURNING v_mensaje

     CALL fn_mensaje("Atenci�n", v_mensaje, "stop")
  END IF
  
END FUNCTION