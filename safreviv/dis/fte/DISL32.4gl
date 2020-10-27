################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 20/03/2018                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISL32                                                   #
#Objetivo          => Programa para realizar la liquidación de créditos        #
#                     vigentes.                                                #
#                                                                              #
#Fecha inicio      => 20/02/2015                                               #
################################################################################
DATABASE safre_viv

GLOBALS
  DEFINE
    v_folio                  DECIMAL(9,0),--Folio de la liquidación de la dispersión 
    v_cuenta_folio           SMALLINT,    --Verifica que existencia de folio
    v_status_folio           SMALLINT,    --Status del folio
    r_bandera                SMALLINT,
    v_programa               CHAR(10),
    v_nom_archivo            CHAR(40),
    v_folio_pag              DECIMAL(9,0), 
    v_proceso_cod_pagos      SMALLINT,    --Proceso cod de registro de pagos
    v_s_mensaje              STRING,
    g_sql_txt                STRING,      --Consultas
    g_usuario                VARCHAR(30), --Almacena al usuario
    g_tipo_proceso           SMALLINT,    --Forma como ejecutara el programa
    g_nom_prog               VARCHAR(30), --Almacena opción del menú
    p_proceso_cod            SMALLINT,    --codigo del proceso
    v_proceso_cod            SMALLINT,    --codigo del proceso
    p_opera_cod              SMALLINT,    --codigo de operacion
    p_pid                    DECIMAL(9,0),
    g_sql_txt                STRING,
    v_proc_entra             SMALLINT,
    v_proc_val               SMALLINT,
    v_cod_conv               SMALLINT,
    v_desc_proc_val          CHAR(40),
    v_mensaje_val            STRING

END GLOBALS

MAIN
  DEFINE 
    f_ventana                ui.Window, --Define las propìedades de la Ventana
    f_forma                  ui.Form ,  --Define las propiedades de la forma
    v_ruta_listados          CHAR(40),
    bnd_consulta             SMALLINT,
    r_bnd_periodo            SMALLINT,
    v_qwery_ibx              STRING 

  DEFINE l_comando           STRING
  DEFINE v_ruta_ejecutable   CHAR(40)
  DEFINE v_folio             LIKE dis_det_avance_pago.folio
  DEFINE l_v_arch_proceso    VARCHAR(100)

  --Recibe valores de argumentos
  LET g_usuario           = ARG_VAL(1)
  LET g_tipo_proceso      = ARG_VAL(2)
  LET g_nom_prog          = ARG_VAL(3)
  LET p_proceso_cod       = 922
  LET p_opera_cod         = 1 
  LET r_bnd_periodo       = 0

  LET v_cuenta_folio      = 0
  LET v_programa          = 'DISL32'
  LET v_nom_archivo       = ''
  LET v_folio_pag         = 0
  LET v_proceso_cod_pagos = 1401

  INITIALIZE l_v_arch_proceso TO NULL

  DATABASE safre_viv
  ##### Se añade modificación de la variable de informix para optimización de consulta #####
   
  --Actualizamos variable de informix para maximizar prioridad de la BD
  LET v_qwery_ibx = "SET PDQPRIORITY HIGH;"
  PREPARE prp_performance FROM v_qwery_ibx
  EXECUTE prp_performance
   
  --Obtiene ruta listados
  SELECT ruta_listados
  INTO   v_ruta_listados
  FROM   seg_modulo 
  WHERE  modulo_cod = 'bat'

  --Obtiene las rutas ejecutable
  SELECT ruta_bin 
  INTO   v_ruta_ejecutable
  FROM   seg_modulo 
  WHERE  modulo_cod = 'dis'

  --Se asigna el titulo del programa
  IF ( g_nom_prog IS NOT NULL ) THEN
     CALL ui.Interface.setText(g_nom_prog)
  END IF

  LET bnd_consulta = 0

  --Validación que NO se tenga alguna operación de Dispersión de Pagos ejecutándose
  LET g_sql_txt = "SELECT a.cod_proceso_entra,   ",
                  "       a.cod_proceso_valida,  ",
                  "       b.proceso_desc         ",
                  "FROM   cat_convivencia_dis a, ",
                  "       cat_proceso b          ",
                  "WHERE  a.cod_proceso_entra  = ", p_proceso_cod,
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

  CLOSE WINDOW SCREEN
   
  OPEN WINDOW w1 WITH FORM "DISL321"
    DIALOG ATTRIBUTES(UNBUFFERED) 
      INPUT BY NAME v_folio
        BEFORE INPUT
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma   = f_ventana.getForm()
     
          NEXT FIELD v_folio
          CALL ui.interface.refresh()
       
        ON ACTION ACCEPT 
           --Valida que se inserte al menos un parámetro
           IF (v_folio IS NULL) THEN
              CALL fn_mensaje("ATENCIÓN",
                              "Debe capturar un folio para la búsqueda",
                              "about")
              NEXT FIELD v_folio   
           END IF  

           IF v_folio IS NOT NULL THEN
              --Solicita la confirmación para ejecutar el proceso de liquidación
              CALL fn_ventana_confirma("DISPERSION", "¿Desea ejecutar la liquidación?", "quest")
              RETURNING r_bandera

              --Si el usuario Acepta
              IF r_bandera = 1 THEN
                 --Valida que el folio no haya sido liquidado o preliquidado
                 CALL fn_valida_folio_liquidado(v_folio) RETURNING v_cuenta_folio,
                                                                   v_folio,
                                                                   v_status_folio
                 IF v_cuenta_folio >= 1 THEN
                    IF v_status_folio <> 0 THEN
                       --Si el folio ya fue liquidado o preliquidado envia mensaje
                       CALL fn_mensaje("DISPERSION", "Folio ya Liquidado en Dispersion", "information")

                       CALL fn_ventana_confirma("DISPERSION", "¿Desea capturar otro folio?", "quest")
                       RETURNING r_bandera
                       IF r_bandera = 1 THEN
                          NEXT FIELD v_folio
                       ELSE
                          EXIT DIALOG
                       END IF
                    ELSE
                       --Si se acepta la ejecución se genera PID del proceso
                       CALL fn_genera_pid (p_proceso_cod, p_opera_cod, g_usuario) 
                       RETURNING p_pid

                       IF (fn_valida_operacion(p_pid, p_proceso_cod, p_opera_cod) = 0 ) THEN
                          DISPLAY "Folio Disp -- ",v_folio
                          --Ejecuta la funcion de preliquida y asigna estatus de LISTO
                          CALL fn_inicializa_proceso (p_pid, p_proceso_cod, p_opera_cod, v_folio,
                                                      v_programa, v_nom_archivo, g_usuario)
                          RETURNING r_bandera

                          --Inicia la operación asignando el estatus de PROCESANDO
                          CALL fn_actualiza_opera_ini(p_pid, p_proceso_cod, p_opera_cod, v_folio,
                                                      v_programa, v_nom_archivo, g_usuario)
                          RETURNING r_bandera

                          DISPLAY "El folio a enviar de registro de pagos es: ", v_folio_pag

                          --Hace el llamado a la función que realiza la preliquidación.
                          LET l_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/DISE17.42r ",
                                           v_folio," ",
                                           v_folio_pag," ",
                                           v_proceso_cod_pagos," ",
                                           g_usuario, " ",
                                           " 1>",v_ruta_listados CLIPPED,"/nohup:",
                                           p_pid USING "&&&&&",":",
                                           p_proceso_cod USING "&&&&&",":",
                                           p_opera_cod USING "&&&&&" ," 2>&1 &"
                          RUN l_comando

                          LET v_s_mensaje = "Se ha enviado la liquidación con el PID: ",
                                             p_pid CLIPPED,
                                            ".\nPuede revisar el avance del proceso en el monitor de ejecución de procesos"
                          CALL fn_mensaje("ATENCIÓN", v_s_mensaje, "about")
                          EXIT DIALOG
                       ELSE
                          CALL fn_muestra_inc_operacion(fn_valida_operacion(p_pid, p_proceso_cod, p_opera_cod))
                       END IF  --De valida operacion                   
                    END IF
                 ELSE
                    CALL fn_mensaje("ATENCIÓN",
                                    "No existe el folio a dispersar",
                                    "about")
                    NEXT FIELD v_folio   
                 END IF  
              ELSE
                 --Si el usuario Cancela
                 CALL fn_mensaje("DISPERSION", "Se ha cancelado la liquidación", "information")
                 CLEAR FORM
                 EXIT PROGRAM
              END IF
           END IF
      END INPUT
      
      ON ACTION cancelar
         EXIT DIALOG

    END DIALOG 
  CLOSE WINDOW w1 

END MAIN

#OBJETIVO: Valida que el Folio Seleccionado de Dispersión NO haya sido liquidado o preliquidado
FUNCTION fn_valida_folio_liquidado(p_folio)
  DEFINE
    p_folio                  DECIMAL(9,0),
    r_count_folio            INTEGER,
    r_folio_liq              DECIMAL(9,0),
    r_status                 INTEGER,
    v_indice                 INTEGER,
    v_QryTxt                 STRING

  LET v_QryTxt = "\n SELECT COUNT (folio), folio, status",
                 "\n FROM   glo_folio",
                 "\n WHERE  folio       = '",p_folio,"' ",
                 "\n AND    status NOT IN (1,2)",
                 "\n GROUP BY 2,3"

  PREPARE prp_ValidaFolio FROM v_QryTxt
  --Declara el cursor para la consulta
  DECLARE cur_ValidaFolio CURSOR FOR prp_ValidaFolio
  FOREACH cur_ValidaFolio INTO r_count_folio, r_folio_liq, r_status
    LET v_indice = v_indice + 1
  END FOREACH

  RETURN r_count_folio, r_folio_liq, r_status

END FUNCTION