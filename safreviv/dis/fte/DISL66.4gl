################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 15/08/2018                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISL66                                                   #
#Objetivo          => Extractor de pagos sin dispersión.                       #
#Fecha inicio      => 15/08/2018                                               #
################################################################################
DATABASE safre_viv

GLOBALS
  DEFINE 
    g_pid                    LIKE bat_ctr_proceso.pid,     --ID del proceso
    g_proceso_cod            LIKE cat_proceso.proceso_cod, --codigo del proceso
    g_opera_cod              LIKE cat_operacion.opera_cod, --codigo de operacion
    p_operacion              SMALLINT

  DEFINE
    g_sql_txt                STRING,
    v_proc_entra             SMALLINT,
    v_proc_val               SMALLINT,
    v_cod_conv               SMALLINT,
    v_desc_proc_val          CHAR(40),
    v_mensaje_val            STRING

  DEFINE 
    v_nombre_com             CHAR(150),
    v_id_derechohabiente     DECIMAL(9,0),
    v_nss                    CHAR(11) --NSS   

END GLOBALS
     
MAIN 
  DEFINE 
    p_usuario_cod            LIKE seg_usuario.usuario_cod, --clave usuario firmado
    p_tipo_ejecucion         SMALLINT,        --forma como ejecutara el programa
    p_s_titulo               STRING,          --titulo de la ventana
    v_folio                  DECIMAL(9,0),
    v_confirma               SMALLINT,
    v_tot_reg                DECIMAL(9,0),
    --v_nss                    CHAR(11), -- NSS
    r_existe_registro        INTEGER   --SMALLINT,

  DEFINE 
    f_ventana                ui.Window, -- Define las propìedades de la Ventana
    f_forma                  ui.Form   -- Define las propiedades de la forma

    
       
  --Se recuperan parametros de entrada
  LET p_usuario_cod    = ARG_VAL(1) --Recibe la variable de usuario
  LET p_tipo_ejecucion = ARG_VAL(2) --Recibe el tipo de proceso
  LET p_s_titulo       = ARG_VAL(3) --Recibe el nombre del programa

  --Si se obtuvo el titulo, se pone como titulo de programa
  IF ( p_s_titulo IS NOT NULL ) THEN
     CALL ui.Interface.setText(p_s_titulo)
  END IF

  LET g_proceso_cod = 938 --Extractor pagos sin dispersión.
  LET g_opera_cod   = 1   --Liquidacion
  LET p_operacion   = 1   --Ejecutar Genera Extractor

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

  CALL fn_ventana_confirma("Error", "¿Seguro que desea generar el extractor de pagos sin dispersión?", "information")
  RETURNING v_confirma               

  IF v_confirma <> 0 THEN
     CALL fn_envia_operacion(p_usuario_cod) 
  ELSE
     RETURN  
  END IF

  {CLOSE WINDOW SCREEN 

  OPEN WINDOW vtn_ext_sd WITH FORM "DISL661"
    DIALOG ATTRIBUTES(UNBUFFERED) 
      INPUT v_nss FROM p_nss 
        BEFORE INPUT
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma = f_ventana.getForm()
          CALL f_forma.setElementHidden("gr_nombre", 1) --Oculta el nombre

          ON ACTION Aceptar
             IF v_nss IS NULL THEN
                CALL fn_mensaje("Atención",
                                "El NSS es obligatorio.",
                                "about") 
                EXIT PROGRAM
             END IF

             IF LENGTH(v_nss) <> 11 THEN
                CALL fn_mensaje("Atención",
                                "El NSS debe tener un longitud de 11 caractéres.",
                                "about") 
                NEXT FIELD v_nss
             END IF

             CALL fn_existe_nss(v_nss) RETURNING r_existe_registro    

            IF r_existe_registro > 0 THEN
              CALL f_forma.setElementHidden("gr_nombre", 0) --Muestra el nombre

              CALL fn_ventana_confirma("Error", "¿Seguro que desea generar el extractor de pagos sin dispersión?", "information")
              RETURNING v_confirma               

              IF v_confirma <> 0 THEN
                 CALL fn_envia_operacion(p_usuario_cod)
              ELSE
                 --RETURN
                 EXIT PROGRAM
              END IF
            END IF

          ON ACTION cancelar
             EXIT DIALOG

      END INPUT
    END DIALOG
  CLOSE WINDOW vtn_ext_sd}

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
  LET v_folio              = 0
  LET v_id_derechohabiente = 0
  LET v_nss                = ""
  LET v_nombre_com         = ""

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
                                "DISL66",
                                v_nombre_archivo,
                                p_usuario)  
     RETURNING v_bandera
     --DISPLAY "--v_bandera: -",v_bandera,"-"
     
     IF ( v_bandera = 0 ) THEN
        CALL fn_actualiza_opera_ini(g_pid,
                                    g_proceso_cod,
                                    g_opera_cod,
                                    v_folio,
                                    "DISS48",
                                    "",
                                    p_usuario)
        RETURNING v_bandera
        --DISPLAY "---v_bandera: -",v_bandera,"-"

        LET v_comando = "nohup fglrun ",v_ruta_origen CLIPPED,"/DISS48.42r ",
                        p_usuario CLIPPED    , " ",
                        g_pid                , " ",
                        g_proceso_cod        , " ",
                        g_opera_cod          , " ",
                        v_folio              , " ",
                        " 1>", l_bat_ruta_listado CLIPPED ,
                        "/nohup:",g_pid  USING "&&&&&",":",
                        g_proceso_cod    USING "&&&&&",":",
                        g_opera_cod      USING "&&&&&",
                        " 2>&1 &"

        DISPLAY v_comando
        RUN v_comando
                  
        CALL fn_mensaje("Atención","Se ha enviado la interface.\nPuede revisar el avance del proceso en el monitor de ejecución de procesos con pid: "||g_pid,"")
        EXIT PROGRAM
     ELSE
        --Se obtiene la descripcion del parametro de salida
        SELECT descripcion
        INTO   v_desc_salida
        FROM   cat_bat_parametro_salida
        WHERE  cod_salida = v_bandera
         
        --Se construye el mensaje de error
        LET v_comando = "No se puede iniciar la operación. No se ha generado la interface.\nError: ", v_desc_salida CLIPPED
        CALL fn_mensaje("Atención",v_comando,"stop")
        EXIT PROGRAM
     END IF
  ELSE
     --No se puede ejecutar la operacion
     CALL fn_recupera_inconsis_opera(v_bandera) 
     RETURNING v_mensaje

     CALL fn_mensaje("Atención", v_mensaje, "stop")
     EXIT PROGRAM
  END IF
END FUNCTION

#Objetivo: Consulta para validar que exista el NSS
FUNCTION fn_existe_nss(v_nss)
  DEFINE
    v_nss                    CHAR(11),
    v_ext_der                INTEGER, -- Contador si existen registros
    v_existe_liquida_movto1  INTEGER, -- Contador si existen registros
    v_QryTxt                 STRING,
    v_sql_txt                STRING,
    v_nombre_af              CHAR(50),
    v_ap_paterno_af          CHAR(50),
    v_ap_materno_af          CHAR(50)

  LET v_ext_der = 0
  
  --Validar id_derechohabiente
  LET v_QryTxt = "\n SELECT id_derechohabiente",
                 "\n FROM   afi_derechohabiente",
                 "\n WHERE  nss = ?"
          
  PREPARE prp_cnt_der FROM v_QryTxt
  EXECUTE prp_cnt_der USING v_nss INTO v_id_derechohabiente

  IF v_id_derechohabiente IS NULL THEN 
     CALL fn_mensaje("Atención",
                     "Trabajador no está registrado como derechohabiente.",
                     "about") 
     EXIT PROGRAM
  ELSE 
     LET v_ext_der = 1
     
      -- Busca el nombre del derechohabiente
      LET v_sql_txt= "\n SELECT id_derechohabiente, ",
                     "\n        nombre_af, ", 
                     "\n        ap_paterno_af, ",
                     "\n        ap_materno_af ",  
                     "\n FROM   afi_derechohabiente ",
                     "\n WHERE  nss = ?"

      PREPARE prp_nom_der FROM v_sql_txt 
      EXECUTE prp_nom_der USING v_nss
                           INTO v_id_derechohabiente,
                                v_nombre_af,
                                v_ap_paterno_af,
                                v_ap_materno_af
  
      LET v_nombre_com = v_nombre_af CLIPPED," ",v_ap_paterno_af CLIPPED," ",v_ap_materno_af CLIPPED

      DISPLAY v_nombre_com TO v_nom_com
  END IF
   
  RETURN v_ext_der

END FUNCTION