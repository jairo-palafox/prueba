################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 27/05/2014                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo       => DIS                                                           #
#Programa     => DISL22                                                        #
#Objetivo     => Programa que lanza la generación de las interfaces del proceso#
#                de dispersion (Autorización de liberación de Recursos)        #
#Fecha inicio => 27/05/2014                                                    #
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
  p_usuario_cod              LIKE seg_usuario.usuario_cod, --clave usuario firmado
  p_tipo_ejecucion           SMALLINT, --forma como ejecutara el programa
  p_s_titulo                 STRING,   --titulo de la ventana
  v_folio                    DECIMAL(9,0),
  v_confirma                 SMALLINT,
  v_tot_registros            SMALLINT

  -- se recuperan parametros de entrada
  LET p_usuario_cod    = ARG_VAL(1)
  LET p_tipo_ejecucion = ARG_VAL(2)
  LET p_s_titulo       = ARG_VAL(3)

  LET v_tot_registros  = 0
  
  -- si se obtuvo el titulo, se pone como titulo de programa
  IF ( p_s_titulo IS NOT NULL ) THEN
     CALL ui.Interface.setText(p_s_titulo)
  END IF
  --CALL STARTLOG (p_usuario_cod CLIPPED|| ".DISL22.log")

  --validación que NO se tenga Preliquidación de Dispersión de Pagos ejecutándose
  IF f_existe_proceso_operacion_ejecutando(901, 1) THEN
     MENU "No se puede ejecutar" ATTRIBUTES ( STYLE="dialog", 
       COMMENT="Preliquidación de Dispersión de Pagos ejecutándose,\ningrese a esta opción cuando finalice",
       IMAGE="information" )

       ON ACTION salir
           RETURN
     END MENU
  END IF

  CLOSE WINDOW SCREEN

  LET g_proceso_cod = 915 --Autorización Liberación de Recursos Dispersión
  LET g_opera_cod   = 1 -- 
  LET p_operacion   = 2 -- 

  --abre la ventana de parametros
  OPEN WINDOW DISL221 WITH FORM "DISL221"
    INPUT BY NAME v_folio ATTRIBUTES(WITHOUT DEFAULTS, UNBUFFERED)
      ON ACTION CANCEL
         EXIT INPUT
         
      ON ACTION ACCEPT
         IF v_folio IS NULL THEN
            CALL fn_mensaje("Error", "Capture el Folio de dispersión para realizar la interface", "information")
            LET v_confirma = 0
            NEXT FIELD v_folio
         ELSE
            SELECT COUNT(*)
            INTO   v_tot_registros
            FROM   glo_folio a
            WHERE  a.folio       = v_folio
            AND    a.proceso_cod = 901
            --AND    a.STATUS      = 2
            AND    a.STATUS      = 1
            IF v_tot_registros > 0 THEN           
               LET v_confirma      = 1
               LET v_tot_registros = 0
            ELSE
               --CALL fn_mensaje("Error", "El Folio de dispersión no esta liquidado", "information")
               CALL fn_mensaje("Error", "El Folio de dispersión no esta preliquidado", "information")
               LET v_confirma      = 0
               LET v_tot_registros = 0
               NEXT FIELD v_folio            
            END IF
         END IF
         
         IF v_confirma <> 0 THEN
            CALL fn_envia_operacion(v_folio, p_usuario_cod)
            EXIT INPUT
         END IF 
   END INPUT
   
   CLOSE WINDOW DISL221
END MAIN

FUNCTION fn_envia_operacion(p_folio, p_usuario)
  DEFINE p_folio             DECIMAL(9,0)
  DEFINE p_usuario           CHAR(20)

  DEFINE v_bandera           SMALLINT -- para verificar resultado de iniciar la operacion
  DEFINE v_comando           STRING
  DEFINE l_bat_ruta_listado  CHAR(40)
  DEFINE v_ruta_origen       CHAR(40)
  DEFINE v_desc_salida       VARCHAR(100)
  DEFINE v_mensaje           STRING
  DEFINE v_folio             LIKE glo_folio.folio
  DEFINE v_nombre_archivo    LIKE glo_ctr_archivo.nombre_archivo

  -- este proceso inicia por webservices, no tiene archivo
  LET v_nombre_archivo = "NA"

  LET v_folio          = 0
                       
  SELECT ruta_listados
  INTO   l_bat_ruta_listado
  FROM   seg_modulo
  WHERE  modulo_cod = 'bat'
            
  SELECT ruta_bin
  INTO   v_ruta_origen
  FROM   seg_modulo
  WHERE  modulo_cod = 'dis'

  -- se verifica si se puede continuar con la operacion
  LET v_bandera = fn_valida_operacion(0, g_proceso_cod, g_opera_cod)
   
  IF ( v_bandera = 0 ) THEN
     CALL fn_genera_pid(g_proceso_cod, g_opera_cod, p_usuario) 
     RETURNING g_pid
     
     CALL fn_inicializa_proceso(g_pid,
                                g_proceso_cod,
                                g_opera_cod,
                                v_folio,
                                "DISL22",
                                v_nombre_archivo,
                                p_usuario)  
     RETURNING v_bandera

     IF ( v_bandera = 0 ) THEN
        CALL fn_actualiza_opera_ini(g_pid,
                                    g_proceso_cod,
                                    g_opera_cod,
                                    v_folio,
                                    "DISS27",
                                    "",
                                    p_usuario)
        RETURNING v_bandera
                                 
        LET v_comando = "nohup fglrun ",v_ruta_origen CLIPPED,"/DISS27.42r ",
                        p_usuario CLIPPED, " ",
                        g_pid            , " ",
                        g_proceso_cod    , " ",
                        g_opera_cod      , " ",
                        v_folio          , " ",
                        p_folio          , " ",
                        " 1>", l_bat_ruta_listado CLIPPED ,
                        "/nohup:",g_pid  USING "&&&&&",":",
                        g_proceso_cod    USING "&&&&&",":",
                        g_opera_cod      USING "&&&&&",
                        " 2>&1 &"
        --DISPLAY v_comando
        RUN v_comando
                  
        CALL fn_mensaje("Atención","Se han enviado las interfaces.\nPuede revisar el avance del proceso en el monitor de ejecución de procesos","")
     ELSE
        -- se obtiene la descripcion del parametro de salida
        SELECT descripcion
        INTO   v_desc_salida
        FROM   cat_bat_parametro_salida
        WHERE  cod_salida = v_bandera
         
        -- se construye el mensaje de error
        LET v_comando = "No se puede iniciar la operación. No se ha enviado el proceso de liquidación.\nError: ", v_desc_salida CLIPPED
        CALL fn_mensaje("Atención",v_comando,"stop")
     END IF
  ELSE
     -- no se puede ejecutar la operacion
     CALL fn_recupera_inconsis_opera(v_bandera) 
     RETURNING v_mensaje
     
     CALL fn_mensaje("Atención", v_mensaje, "stop")
  END IF
END FUNCTION