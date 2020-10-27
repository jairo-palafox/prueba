###############################################################################
#Version                    => 1.0                                            #
#Fecha ultima modificacion  => 09/04/2018                                     #
###############################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo           => DIS                                                       #
#Programa         => DISL58                                                    #
#Objetivo         => Solicitar la liquidaci�n especial de la dispersi�n de     #
#                    pagos de salidas de aclaratorios para montos de           #
#                    amortizaciones.                                           #
#Fecha de Inicio  => 21/06/2017                                                #
################################################################################
DATABASE safre_viv

MAIN 
  DEFINE 
    arr_foliosliquidados     DYNAMIC ARRAY OF RECORD #Arreglo de folios liquidados
    f_actualiza              DATE,          --Fecha de actualizacion
    folio                    DECIMAL(9,0),  --N�mero de Folio 
    monto_pesos              DECIMAL(12,2), --Monto de pesos
    v_status                 CHAR(53)       --Status del folio
  END RECORD,
 
  --Secci�n de Variables Locales
  v_cb_folio                 DECIMAL(9,0), --Folio que recibe del combobox
  v_de_Fecha                 DATE,         --Fecha determina folio
  v_fecha_actualiza          DATE,
  v_indice                   INTEGER,
  v_folio_disp               DECIMAL(9,0),
  l_comando                  STRING,
  v_ruta_ejecutable          CHAR(40),
  v_ruta_listados            CHAR(40),

  --Secci�n de par�metros
  p_usuario                  CHAR(20),  --Clave de usuario
  p_tipo_proc                CHAR(1),
  p_nombre_menu              CHAR(40),
  p_proceso_cod              SMALLINT,  --C�digo del proceso
  p_opera_cod                SMALLINT,  --C�digo de operaci�n
  p_pid                      DECIMAL(9,0),
  p_programa                 CHAR(10),  

  --Secci�n de Variables de Retorno
  r_bandera                  SMALLINT,
  r_nom_archivo              CHAR(40), --Variable para almacenar el nombre del archivo
  r_folio_liq                DECIMAL(9,0),
  r_count_folio              INTEGER, 
  r_status                   INTEGER,
  v_proceso_cod_pagos        SMALLINT, --Proceso cod de registro de pagos segun folio de pagos
  v_s_mensaje                STRING,
  v_ind_llena_cb             INTEGER, 
  v_tot_reg                  DECIMAL(9,0),
  g_sql_txt                  STRING,
  v_proc_entra               SMALLINT,
  v_proc_val                 SMALLINT,
  v_cod_conv                 SMALLINT,
  v_desc_proc_val            CHAR(40),
  v_mensaje_val             STRING

  DEFINE 
    f_ventana                ui.Window,  --Define las prop�edades de la Ventana
    f_forma                  ui.Form,    --Define las propiedades de la forma
    cb                       ui.ComboBox --Variabla de Combobox

  LET p_programa    = "DISL58"
  LET p_usuario     = ARG_VAL(1) --Recibe la variable de usuario
  LET p_tipo_proc   = ARG_VAL(2) --Recibe el tipo de proceso
  LET p_nombre_menu = ARG_VAL(3) --Recibe el nombre del programa 
  LET v_de_Fecha    = ""
  LET p_proceso_cod = 901 --Codigo del Proceso de Dispersion (4)(901)
  LET p_opera_cod   = 1   --Codigo de la operacion de Liquidacion (1)
  LET cb            = ui.ComboBox.forName("v_cb_folio") --Asignaci�n del combo a la forma

  --Obtiene las rutas ejecutable
  SELECT ruta_bin
  INTO   v_ruta_ejecutable
  FROM   seg_modulo 
  WHERE  modulo_cod = 'dis'

  --Obtiene ruta listados
  SELECT ruta_listados
  INTO   v_ruta_listados
  FROM   seg_modulo 
  WHERE  modulo_cod = 'bat'

  --Abre ventana para consulta de folios liquidados
  OPEN WINDOW vtn_Consulta WITH FORM "DISL581"   
    --DISPLAY FORM vtn_ConsultaFolios
    -- Funci�n UI que asigna Titulo a la ventana del Programa
    CALL ui.Interface.setText(p_nombre_menu)

    --Validaci�n que NO se tenga alguna operaci�n de Dispersi�n de Pagos ejecut�ndose
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
         LET v_mensaje_val = "Proceso ", v_desc_proc_val CLIPPED, " ejecut�ndose,\ningrese a esta opci�n cuando finalice."
         MENU "No se puede ejecutar" 
           ATTRIBUTES ( STYLE="dialog",
           COMMENT= v_mensaje_val,
           IMAGE="information" )

           ON ACTION salir
               RETURN
         END MENU
      END IF
    END FOREACH

    LET f_ventana = ui.Window.getCurrent()
    LET f_forma   = f_ventana.getForm()

    --Oculta bot�n Preliquida
    CALL f_forma.setElementHidden("preliquidar",1)

    --Llama funci�n para llenar como con fecha nula
    CALL fn_llena_combo(v_de_Fecha) 
    RETURNING v_cb_folio,
              v_fecha_actualiza,
              v_ind_llena_cb

    --Obtiene los parametros de fecha y folio
    INPUT BY NAME v_cb_folio, v_de_Fecha WITHOUT DEFAULTS
    ATTRIBUTE (ACCEPT = FALSE, CANCEL = FALSE, UNBUFFERED)
      BEFORE INPUT
        LET v_cb_folio = v_cb_folio

      AFTER INPUT
        --Almacena los datos de las variables para enviarlos a las funciones
        CALL GET_FLDBUF(v_cb_folio, v_de_Fecha)
        RETURNING v_cb_folio, 
                  v_de_Fecha

        ON ACTION CANCEL
           EXIT INPUT

        --Acciones que realiza en el bot�n de aceptar
        ON ACTION ACCEPT
           --Valida que se capture al menos un campo
           IF (v_de_Fecha IS NOT NULL AND v_cb_folio IS NULL) THEN
              IF v_de_Fecha > TODAY THEN
                 CALL fn_mensaje("Error", "Fecha Incorrecta", "information")
                 NEXT FIELD v_de_Fecha
              ELSE
                 --Si la fecha es valida llama la funci�n para llenar el combo con
                 --los folios liquidados en la fecha capturada
                 CALL fn_llena_combo(v_de_Fecha) 
                 RETURNING v_cb_folio,
                           v_fecha_actualiza,
                           v_ind_llena_cb

                 --Valida que exista informaci�n si no la hay, envia mensaje
                 IF v_ind_llena_cb = 1 THEN
                    LET v_fecha_actualiza = NULL
                    CALL fn_mensaje("Info",
                                    "No existe informaci�n para la Fecha de Liquidaci�n capturada",
                                    "information")
                    NEXT FIELD v_cb_folio
                 END IF
              END IF
           END IF

           IF v_fecha_actualiza IS NULL THEN
              CLEAR FORM
              --Llama funci�n para llenar combo con fecha nula
              CALL fn_llena_combo(v_de_Fecha) 
              RETURNING v_cb_folio,
                        v_fecha_actualiza,
                        v_ind_llena_cb
           END IF

           --Si fecha o folio son nulos, muestra mensaje de error
           IF v_de_Fecha IS NULL AND v_cb_folio IS NULL OR v_cb_folio = -1 THEN
              CALL fn_mensaje("ERROR", "Debe Capturar al menos un par�metro", "stop")
              NEXT FIELD v_cb_folio
           END IF

           --Llama funci�n para llenar combo con fecha que el usuario captura
           IF length(v_cb_folio) = 0 THEN
              CALL fn_mensaje("ERROR", "Selecciona un folio", "stop")
              NEXT FIELD v_cb_folio
           END IF

           CALL fn_Folios_Liquidados(v_cb_folio) 
           RETURNING arr_foliosliquidados,
                     v_indice

           --Valida que exista informaci�n si no la hay, envia mensaje
           IF v_indice <= 1 THEN
              CALL fn_mensaje("Info",
                              "No existe informaci�n con los par�metros capturados",
                              "info")

           ELSE
              --Muestra bot�n Preliquida
              CALL f_forma.setElementHidden("preliquidar", 0)

              --Oculta bot�n Buscar Folios por fecha
              CALL f_forma.setElementHidden("folios", 1)

              --Si existe informaci�n que cumpla con los par�metros capturados
              --la muestra en los resultados
              DISPLAY ARRAY arr_foliosliquidados TO sa_HistRegPag.*
              ATTRIBUTES (ACCEPT = FALSE, CANCEL = FALSE)
                AFTER DISPLAY
                  --Oculta bot�n Aceptar
                  CALL f_forma.setElementHidden("accept",1)

                  --Bot�n de salida del Display
                  ON ACTION CANCEL
                     EXIT DISPLAY

                  --Ejecuta la preliquidaci�n de la Dispersi�n
                  ON ACTION preliquidar
                     --Solicita la confirmaci�n para ejecutar el proceso de preliquidaci�n
                     CALL fn_ventana_confirma("DISPERSI�N", "�Desea ejecutar la preliquidaci�n?", "quest")
                     RETURNING r_bandera

                     --Si el usuario Acepta
                     IF r_bandera = 1 THEN
                        --Ejecuta la funci�n para solicitar el nombre del archivo que se carg�
                        CALL fn_recupera_arch_cargado(p_proceso_cod, p_opera_cod) 
                        RETURNING r_nom_archivo

                        --Valida que el folio no haya sido liquidado o preliquidado
                        CALL fn_valida_folio_liquidado(v_folio_disp) 
                        RETURNING r_count_folio,
                                  r_folio_liq,
                                  r_status

                        IF r_count_folio >= 1 THEN
                           IF r_status = 1 THEN
                              --Si el folio ya fue liquidado o preliquidado envia mensaje
                              CALL fn_mensaje("DISPERSI�N", "Folio ya Preliquidado en Dispersi�n", "information")
                           ELSE
                              IF r_status = 2 THEN
                                 CALL fn_mensaje("DISPERSI�N", "Folio ya Liquidado en Dispersi�n", "information")
                              END IF
                           END IF

                           CALL fn_ventana_confirma("DISPERSI�N", "�Desea seleccionar otro folio?", "quest")
                           RETURNING r_bandera

                           IF r_bandera = 1 THEN
                              CLEAR FORM
                              CALL fn_llena_combo(v_de_Fecha) 
                              RETURNING v_cb_folio,
                                        v_fecha_actualiza,
                                        v_ind_llena_cb
                              LET v_cb_folio = -1
                              CALL f_forma.setElementHidden("preliquidar", 1)
                              CONTINUE INPUT
                           ELSE
                              EXIT INPUT
                           END IF
                        ELSE
                           --Si se acepta la ejecuci�n se genera PID del proceso
                           CALL fn_genera_pid (p_proceso_cod, p_opera_cod, p_usuario) 
                           RETURNING p_pid

                           IF (fn_valida_operacion(p_pid, p_proceso_cod, p_opera_cod) = 0 ) THEN
                              --Enlaza el folio referencia
                              CALL fn_genera_folio_dis(p_proceso_cod, p_opera_cod, v_cb_folio, p_usuario)
                              RETURNING v_folio_disp

                              DISPLAY "Folio Disp -- ",v_folio_disp
                              --Ejecuta la funcion de preliquida y asigna estatus de LISTO
                              CALL fn_inicializa_proceso (p_pid, p_proceso_cod, p_opera_cod, v_folio_disp,
                                                          p_programa, r_nom_archivo, p_usuario)
                              RETURNING r_bandera

                              SELECT UNIQUE proceso_cod
                              INTO   v_proceso_cod_pagos
                              FROM   glo_folio
                              WHERE  folio = v_cb_folio

                              --Inicia la operaci�n asignando el estatus de PROCESANDO
                              CALL fn_actualiza_opera_ini(p_pid, p_proceso_cod, p_opera_cod, v_folio_disp,
                                                          p_programa, r_nom_archivo, p_usuario)
                              RETURNING r_bandera

                              DISPLAY "El folio a enviar de registro de pagos es: ", v_cb_folio

                              --Hace el llamado a la funci�n que realiza la preliquidaci�n.
                              LET l_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/DISP11.42r ",
                                              v_folio_disp," ",
                                              v_cb_folio," ",
                                              v_proceso_cod_pagos," ",
                                              v_fecha_actualiza," ",
                                              p_usuario, " ",
                                              " 1>",v_ruta_listados CLIPPED,"/nohup:",
                                              p_pid USING "&&&&&",":",
                                              p_proceso_cod USING "&&&&&",":",
                                              p_opera_cod USING "&&&&&" ," 2>&1 &"
                              RUN l_comando

                              LET v_s_mensaje = "Se ha enviado la preliquidaci�n con PID: ",
                                                p_pid CLIPPED,
                                                ".\nPuede revisar el avance del proceso en el monitor de ejecuci�n de procesos"
                              EXIT DISPLAY
                              EXIT INPUT
                           ELSE
                              CALL fn_muestra_inc_operacion(fn_valida_operacion(p_pid, p_proceso_cod, p_opera_cod))
                           END IF  --De valida operacion
                        END IF
                     ELSE
                        --Si el usuario Cancela
                        CALL fn_mensaje("DISPERSI�N", "Se ha cancelado la preliquidaci�n", "information")
                        CLEAR FORM
                        EXIT INPUT
                     END IF
              END DISPLAY
              EXIT INPUT
           END IF
    END INPUT

---BORRAR---   
    {MENU ""
    
    BEFORE MENU
    DISPLAY "Antes de fn_Folios_Liquidados()"

    CALL fn_Folios_Liquidados() RETURNING arr_foliosliquidados, 
                                          v_indice
                                          
    
    --LET v_tot_reg = CALL arr_folios_liquidados.
    --Valida que exista informaci�n si no la hay, envia mensaje
    IF v_indice <= 1 THEN
      CALL fn_mensaje("Info",
                      "No existe informaci�n con los par�metros capturados",
                      "info")

    ELSE  
      --Solicita la confirmaci�n para ejecutar el proceso de Liquidaci�n
      CALL fn_ventana_confirma("DISPERSION", "�Desea ejecutar la Liquidacion?", "quest")
      RETURNING r_bandera
               
      --Si el usuario Acepta 
      IF r_bandera = 1 THEN
        --Ejecuta la funci�n para solicitar el nombre del archivo que se carg�
        CALL fn_recupera_arch_cargado(p_proceso_cod, p_opera_cod) RETURNING r_nom_archivo

        --Si se acepta la ejecuci�n se genera PID del proceso
        CALL fn_genera_pid (p_proceso_cod, p_opera_cod, p_usuario) RETURNING p_pid

        IF (fn_valida_operacion(p_pid, p_proceso_cod, p_opera_cod) = 0 ) THEN
          --Enlaza el folio referencia 
          CALL fn_genera_folio_dis(p_proceso_cod, p_opera_cod, v_cb_folio, p_usuario)
               RETURNING v_folio_disp

          DISPLAY "Folio Disp -- ",v_folio_disp
          --Ejecuta la funcion de Liquida y asigna estatus de LISTO
          CALL fn_inicializa_proceso (p_pid, p_proceso_cod, p_opera_cod, v_folio_disp, 
                                        p_programa, r_nom_archivo, p_usuario)
               RETURNING r_bandera

          --Inicia la operaci�n asignando el estatus de PROCESANDO
          CALL fn_actualiza_opera_ini(p_pid, p_proceso_cod, p_opera_cod, v_folio_disp, 
                                      p_programa, r_nom_archivo, p_usuario)
               RETURNING r_bandera
                                             
            --Hace el llamado a la funci�n que realiza la Liquidaci�n.
          LET l_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/DISP11.42r ",
                                          v_folio_disp," ",
                                          v_proceso_cod_pagos," ",
                                          v_fecha_actualiza," ",
                                          p_usuario, " ",
                                          " 1>",v_ruta_listados CLIPPED,"/nohup:",
                                          p_pid USING "&&&&&",":",
                                          p_proceso_cod USING "&&&&&",":",
                                          p_opera_cod USING "&&&&&" ," 2>&1 &"
          RUN l_comando
                           
          LET v_s_mensaje = "Se ha enviado la Liquidac�on con PID: ",
                            p_pid CLIPPED,
                            ".\nPuede revisar el avance del proceso en el monitor de ejecuci�n de procesos"

          CALL fn_mensaje("aviso",v_s_mensaje,"information");

          EXIT PROGRAM
          
        ELSE
          CALL fn_muestra_inc_operacion(fn_valida_operacion(p_pid, p_proceso_cod, p_opera_cod))
        END IF  --De valida operacion
      ELSE 
        --Si el usuario Cancela
        CALL fn_mensaje("DISPERSION", "Se ha cancelado la Liquidaci�n", "information")                     
        EXIT PROGRAM                     
      END IF
    END IF
    END MENU
  CLOSE WINDOW vtn_Consulta}
---BORRAR---

END MAIN
      
#Obejtivo: Funci�n que consulta los folios liquidados 
FUNCTION fn_Folios_Liquidados(p_folio)
  DEFINE 
    arr_folios_liquidados    DYNAMIC ARRAY OF RECORD --Almacena folios liquidados 
    f_actualiza              DATE,          --Fecha de actualizacion                                         
    folio                    DECIMAL(9,0),  --N�mero de Folio                                                
    monto_pesos              DECIMAL(12,2), --Monto de pesos                                                
    v_status                 CHAR(53)       --Status de folio 
  END RECORD,

  p_folio                    DECIMAL(9,0),  --Folio que recibe del combobox   
  v_QryTxt                   STRING,        --Cadena para almacenar Query 
  v_indice_f                 INTEGER        --Variable de indice
       
  LET v_indice_f = 1
   
  LET v_QryTxt = "\n SELECT gf.f_actualiza, gf.folio,COUNT(cm.folio),",
                 "\n        gf.status || '-' || ",
                 "\n        ea.estado_descripcion AS ESTADO",
                 "\n FROM   glo_folio gf",
                 "\n JOIN   cta_his_pagos cm",
                 "\n ON     gf.folio      = cm.folio",
                 "\n JOIN   cat_edo_archivo ea ",
                 "\n ON     ea.estado_cod = gf.status",                                          
                 "\n WHERE  gf.status    IN (1,2)",
                 "\n AND    gf.folio      = ",p_folio,
                 "\n GROUP BY 2,1,4" 

  PREPARE prp_Folios_Liq FROM v_QryTxt
   
  -- Declara el cursor para la consulta 
  DECLARE cur_Folios_Liq CURSOR FOR prp_Folios_Liq    
  FOREACH cur_Folios_Liq INTO arr_folios_liquidados[v_indice_f].*
    LET v_indice_f = v_indice_f + 1
  END FOREACH

  DISPLAY "Folios liquidados: ",arr_folios_liquidados.getLength()

  CALL arr_folios_liquidados.deleteElement(v_indice_f)
   
  --Retorna el arreglo con la informaci�n de los folios liquidados y el 
  --tama�o del arreglo 
  RETURN arr_folios_liquidados, v_indice_f

END FUNCTION

#Objetivo: Genera el ComboBox para la dispersi�n
FUNCTION fn_llena_combo(p_fecha)
  DEFINE
    var_dis_hist             DYNAMIC ARRAY OF RECORD --Arreglo para almacenar el folio
    v_folio                  DECIMAL(9,0),           --Campo de Folio
    v_arch_fol               VARCHAR(60)
  END RECORD,

  p_fecha_actualiza          DATE,
  v_cb_folio                 DECIMAL(9,0), --Almacena folio en el combobox
  p_fecha                    DATE,         --Par�metro de fecha
  v_indice                   INTEGER,      --Variable del indice
  v_QryTxt                   STRING,       --Cadena para almacenar Query
  cb                         ui.ComboBox   --Variable de Combobox

  LET cb = ui.ComboBox.forName("v_cb_folio") --Asignaci�n del combo a la forma

  --Validaci�n si el combo es nulo
  IF cb IS NULL THEN
     ERROR "Form field not found in current form"
     EXIT PROGRAM
  END IF

  LET v_indice = 1

  --Validaci�n del tama�o de la fecha si es 0 no la incluye para el Query
  LET v_QryTxt = "\n SELECT gf.folio,",
                 "\n        gf.folio||'-'||gc.nombre_archivo,",
                 "\n        gf.f_actualiza",
                 "\n FROM   glo_folio gf,glo_ctr_archivo gc",
                 "\n WHERE  gf.folio       IN (52611,52644,52918,52990,53010,53030,54032,56755,56769) ",
                 "\n AND    gf.status       = 2",
                 "\n AND    gf.proceso_cod IN (101,102,103,107,110)",
                 "\n AND    gf.folio   NOT IN (SELECT folio_referencia",
                 "\n                           FROM   glo_folio",
                 "\n                           WHERE  proceso_cod           = 901",
                 "\n                           AND    status               <> 0",
                 "\n                           AND    folio_referencia IS NOT NULL",
                 "\n                           AND    folio            NOT IN (52612,52659,52929,53004,53022,53038,54035,56770,56771))",
                 "\n AND    gf.folio        =  gc.folio"
  IF LENGTH(p_fecha) > 0 THEN
     LET v_QryTxt = v_QryTxt||"\n AND gf.f_actualiza = '",p_fecha,"'",
                              "\n ORDER BY 1 DESC"
  ELSE
     LET v_QryTxt = v_QryTxt||"\n ORDER BY 1 DESC"
  END IF

  DISPLAY "Llena combo ",v_QryTxt

  --Prepara la consulta para obtener folios liquidados
  PREPARE prp_Cons_Folio_Hist FROM v_QryTxt

  --Limpia el combo
  CALL cb.clear()

  --Declara el cursor para la consulta
  DECLARE cur_llena_combo_folio CURSOR FOR prp_Cons_Folio_Hist
  FOREACH cur_llena_combo_folio INTO var_dis_hist[v_indice].v_folio,
                                     var_dis_hist[v_indice].v_arch_fol,
                                     p_fecha_actualiza
    --Agrega elementos al combobox
    CALL cb.addItem(var_dis_hist[v_indice].v_folio,
                    var_dis_hist[v_indice].v_arch_fol)
    DISPLAY var_dis_hist[v_indice].v_folio, " - ",
            var_dis_hist[v_indice].v_arch_fol, ", ",
            p_fecha_actualiza
    LET v_indice = v_indice + 1
  END FOREACH

  CALL var_dis_hist.deleteElement(v_indice)

  RETURN v_cb_folio, p_fecha_actualiza, v_indice
  
END FUNCTION

#OBJETIVO: Valida que el Folio Seleccionado de Registro de Pagos NO haya sido liquidado o preliquidado
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
                 "\n WHERE  folio_referencia = '",p_folio,"' ",
                 "\n AND    status          IN (1,2)",
                 "\n GROUP BY 2,3"

  PREPARE prp_ValidaFolio FROM v_QryTxt
  --Declara el cursor para la consulta
  DECLARE cur_ValidaFolio CURSOR FOR prp_ValidaFolio
  FOREACH cur_ValidaFolio INTO r_count_folio, r_folio_liq, r_status
    LET v_indice = v_indice + 1
  END FOREACH

  RETURN r_count_folio, r_folio_liq, r_status

END FUNCTION