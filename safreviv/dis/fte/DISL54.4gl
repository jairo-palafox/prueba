###############################################################################
#Version                    => 1.0.1                                          #
#Fecha ultima modificacion  => 19/02/2016                                     #
###############################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo           => DIS                                                       #
#Programa         => DISL54                                                    #
#Objetivo         => Solicitar los criterios de captura para recuperar la      #
#                    informaci�n del �ltimo folio liquidado del hist�rico del  #
#                    proceso de Aportaciones Subsecuentes sin conciliar        #
#                    para su Dispersi�n.                                       #
#Fecha de Inicio  => 19/02/2016                                                #
################################################################################
DATABASE safre_viv

MAIN 
  DEFINE 
    arr_foliosliquidados           DYNAMIC ARRAY OF RECORD #Arreglo de folios liquidados
    f_actualiza                    DATE,          --fecha de actualizacion
    folio                          DECIMAL(9,0),  --n�mero de Folio 
    monto_pesos                    DECIMAL(12,2), --monto de pesos
    v_status                       CHAR(53)       --status del folio
  END RECORD,
 
  -- Secci�n de Variables Locales
  v_cb_folio                       DECIMAL(9,0), --folio que recibe del combobox
  v_de_Fecha                       DATE,         --fecha determina folio
  v_fecha_actualiza                DATE,
  v_indice                         INTEGER,
  v_folio_disp                     DECIMAL(9,0),
  l_comando                        STRING,
  v_ruta_ejecutable                CHAR(40),
  v_ruta_listados                  CHAR(40),

  -- Secci�n de par�metros
  p_usuario                        CHAR(20),  --clave de usuario
  p_tipo_proc                      CHAR(1),
  p_nombre_menu                    CHAR(40),
  p_proceso_cod                    SMALLINT,  --codigo del proceso
  p_opera_cod                      SMALLINT,  --codigo de operacion
  p_pid                            DECIMAL(9,0),
  p_programa                       CHAR(10),  

  -- Secci�n de Variables de Retorno
  r_bandera                        SMALLINT,
  r_nom_archivo                    CHAR(40), --variable para almacenar el nombre del archivo
  r_folio_liq                      DECIMAL(9,0),
  r_count_folio                    INTEGER, 
  r_status                         INTEGER,
  v_proceso_cod_pagos              SMALLINT, --proceso cod de registro de pagos segun folio de pagos
  v_s_mensaje                      STRING,
  v_ind_llena_cb                   INTEGER

  DEFINE 
    f_ventana                      ui.Window,   -- define las prop�edades de la Ventana
    f_forma                        ui.Form,     -- define las propiedades de la forma
    cb                             ui.ComboBox  -- variable de Combobox       

  LET p_programa    = "DISL54"
  LET p_usuario     = ARG_VAL(1) --Recibe la variable de usuario
  LET p_tipo_proc   = ARG_VAL(2) --Recibe el tipo de proceso
  LET p_nombre_menu = ARG_VAL(3) --Recibe el nombre del programa 
  LET v_de_Fecha    = ""
  LET p_proceso_cod = 933 --Codigo del Proceso de Aportaciones Subsecuentes Sin Adelanto 932)
  LET p_opera_cod   = 4   --Codigo de la operacion de preliquidacion (3)
  LET cb            = ui.ComboBox.forName("v_cb_folio") --Asignaci�n del combo a la forma

  -- Obtiene las rutas ejecutable
  SELECT ruta_bin
  INTO   v_ruta_ejecutable
  FROM   seg_modulo 
  WHERE  modulo_cod = 'dis'

  -- Obtiene ruta listados
  SELECT ruta_listados
  INTO   v_ruta_listados
  FROM   seg_modulo 
  WHERE  modulo_cod = 'bat'

  -- Abre ventana para consulta de folios liquidados
  OPEN FORM vtn_ConsultaFolios FROM "DISL541"
    DISPLAY FORM vtn_ConsultaFolios

    -- Funci�n UI que asigna Titulo a la ventana del Programa
    CALL ui.Interface.setText(p_nombre_menu)

    LET f_ventana = ui.Window.getCurrent()
    LET f_forma   = f_ventana.getForm()

    -- Oculta bot�n Preliquida 
    CALL f_forma.setElementHidden("preliquidar",1) 
   
    -- Llama funci�n para llenar combo con fecha nula
    CALL fn_llena_combo(v_de_Fecha) RETURNING v_cb_folio,
                                              v_fecha_actualiza,
                                              v_ind_llena_cb

    -- Obtiene los parametros de fecha y folio
    INPUT BY NAME v_cb_folio, v_de_Fecha WITHOUT DEFAULTS
    ATTRIBUTES (ACCEPT = FALSE, CANCEL = FALSE, UNBUFFERED)
   
      BEFORE INPUT
        LET v_cb_folio = v_cb_folio

      AFTER INPUT
        -- Almacena los datos de las variables para enviarlos a las funciones
        CALL GET_FLDBUF(v_cb_folio, v_de_Fecha) RETURNING v_cb_folio,
                                                          v_de_Fecha

        ON ACTION cancel
           EXIT INPUT

        -- Acciones que realiza en el bot�n de aceptar 
        ON ACTION accept

           -- Valida que se capture al menos un campo 
           IF (v_de_Fecha IS NOT NULL AND v_cb_folio IS NULL) THEN 
              IF v_de_Fecha > TODAY THEN 
                 CALL fn_mensaje("Error", "Fecha Incorrecta", "information")
                 NEXT FIELD v_de_Fecha
              ELSE
                -- Si la fecha es valida llama la funci�n para llenar el combo con
                -- los folios liquidados en la fecha capturada
                CALL fn_llena_combo(v_de_Fecha) RETURNING v_cb_folio,     --Aqu� me quede!!
                                                          v_fecha_actualiza, 
                                                          v_ind_llena_cb

                -- Valida que exista informaci�n si no la hay, envia mensaje
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
              -- Llama funci�n para llenar combo con fecha nula
              CALL fn_llena_combo(v_de_Fecha) RETURNING v_cb_folio,
                                                        v_fecha_actualiza,
                                                        v_ind_llena_cb
           END IF 

           -- Si fecha o folio son nulos, muestra mensaje de error
           IF v_de_Fecha IS NULL AND v_cb_folio IS NULL OR v_cb_folio = -1 THEN           
              CALL fn_mensaje("ERROR", "Debe Capturar al menos un par�metro", "stop")
              NEXT FIELD v_cb_folio
           END IF  

           -- Llama funci�n para llenar combo con fecha que el usuario captura
           IF length(v_cb_folio) = 0 THEN
              CALL fn_mensaje("ERROR", "Selecciona un folio", "stop")
              NEXT FIELD v_cb_folio
           END IF 

           CALL fn_Folios_Liquidados(v_cb_folio) RETURNING arr_foliosliquidados, 
                                                           v_indice

           -- Valida que exista informaci�n si no la hay, envia mensaje
           IF v_indice <= 1 THEN
              CALL fn_mensaje("Info",
                              "No existe informaci�n con los par�metros capturados",
                              "info")

           ELSE  
             -- Muestra bot�n Preliquida 
             CALL f_forma.setElementHidden("preliquidar", 0)

             -- Oculta bot�n Buscar Folios por fecha
             CALL f_forma.setElementHidden("folios", 1)

             -- Si existe informaci�n que cumpla con los par�metros capturados
             -- la muestra en los resultados
             DISPLAY ARRAY arr_foliosliquidados TO sa_HistRegPag.*
             ATTRIBUTES (ACCEPT = FALSE, CANCEL = FALSE)

               AFTER DISPLAY 
                 -- Oculta bot�n Aceptar
                 CALL f_forma.setElementHidden("accept",1)

                 -- Bot�n de salida del Display
                 ON ACTION CANCEL
                    EXIT DISPLAY
               
                 -- Ejecuta la preliquidaci�n de la Dispersi�n de Aportaciones 
                 -- Subsecuentes Sin Adelanto
                 ON ACTION preliquidar
                    -- Solicita la confirmaci�n para ejecutar el proceso de preliquidaci�n
                    CALL fn_ventana_confirma("APORTACIONES SUBSECUENTES SIN CONCILIAR", "�Desea ejecutar la preliquidaci�n?", "quest")
                    RETURNING r_bandera
               
                    -- Si el usuario Acepta 
                    IF r_bandera = 1 THEN
                       -- Ejecuta la funci�n para solicitar el nombre del archivo que se carg�
                       CALL fn_recupera_arch_cargado(p_proceso_cod, p_opera_cod) 
                       RETURNING r_nom_archivo

                       -- Valida que el folio no haya sido liquidado o preliquidado
                       CALL fn_valida_folio_liquidado(v_folio_disp) 
                       RETURNING r_count_folio, r_folio_liq, r_status 

                       IF r_count_folio >= 1 THEN 
                          IF r_status = 1 THEN 
                             -- Si el folio ya fue liquidado o preliquidado envia mensaje
                             CALL fn_mensaje("APORTACIONES SUBSECUENTES SIN CONCILIAR", "Folio ya Preliquidado en Aportaciones Subsecuentes Sin Conciliar", "information")
                          ELSE  
                            IF r_status = 2 THEN 
                               CALL fn_mensaje("APORTACIONES SUBSECUENTES SIN CONCILIAR", "Folio ya Liquidado en Aportaciones Subsecuentes Sin Conciliar", "information")
                            END IF
                          END IF

                          CALL fn_ventana_confirma("APORTACIONES SUBSECUENTES SIN CONCILIAR", "�Desea seleccionar otro folio?", "quest")
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
                         -- Si se acepta la ejecuci�n se recuper� el PID del proceso                         
                         CALL fn_max_pid(p_proceso_cod, p_opera_cod) RETURNING p_pid
                         
                         IF (fn_valida_operacion(p_pid, p_proceso_cod, p_opera_cod) = 0 ) THEN

                            LET v_folio_disp = v_cb_folio

                            --DISPLAY "Folio Preliq Apo Subs SC-- ",v_folio_disp
                            -- Ejecuta la funcion de preliquida y asigna estatus de LISTO
                            CALL fn_inicializa_proceso (p_pid, p_proceso_cod, p_opera_cod, v_folio_disp, 
                                                        p_programa, r_nom_archivo, p_usuario)
                            RETURNING r_bandera

                            SELECT UNIQUE proceso_cod
                            INTO   v_proceso_cod_pagos
                            FROM   glo_folio
                            WHERE  folio = v_cb_folio
  
                            -- Inicia la operaci�n asignando el estatus de PROCESANDO
                            CALL fn_actualiza_opera_ini(p_pid, p_proceso_cod, p_opera_cod, v_folio_disp, 
                                                        p_programa, r_nom_archivo, p_usuario)
                            RETURNING r_bandera

                            DISPLAY "El folio a enviar de preliquidaci�n de aportaciones subsecuentes es: ", v_cb_folio
                                             
                            -- Hace el llamado a la funci�n que realiza la preliquidaci�n.
                            LET l_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/DISP08.42r ",
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
                         END IF  -- De valida operacion
                       END IF 
                    ELSE 
                      -- Si el usuario Cancela
                      CALL fn_mensaje("APORTACIONES SUBSECUENTES SIN CONCILIAR", "Se ha cancelado la preliquidaci�n", "information")                     
                      CLEAR FORM
                      EXIT INPUT                     
                    END IF 
             END DISPLAY
             EXIT INPUT
           END IF
    END INPUT
END MAIN
      
#Objetivo: Genera el ComboBox para la preliquidaci�n de aportaciones subsecuentes sin conciliar
FUNCTION fn_llena_combo(p_fecha)               
  DEFINE 
    var_dis_hist                   DYNAMIC ARRAY OF RECORD --arreglo para almacenar el folio
    v_folio                        DECIMAL(9,0),           --campo de Folio
    v_arch_fol                     VARCHAR(60)
  END RECORD,

  p_fecha_actualiza                DATE,     
  v_cb_folio                       DECIMAL(9,0), --almacena folio en el combobox
  p_fecha                          DATE,         --par�metro de fecha
  v_indice                         INTEGER,      --variable del indice  
  v_QryTxt                         STRING,       --cadena para almacenar Query
  cb                               ui.ComboBox   --variable de Combobox

  LET cb = ui.ComboBox.forName("v_cb_folio") --asignaci�n del combo a la forma

  -- Validaci�n si el combo es nulo 
  IF cb IS NULL THEN
     ERROR "Form field not found in current form"
     EXIT PROGRAM
  END IF

  LET v_indice = 1
   
  -- Validaci�n del tama�o de la fecha si es 0 no la incluye para el Query
  LET v_QryTxt = "\n SELECT UNIQUE gf.folio,",
                 "\n        gf.folio||'-'||gc.nombre_archivo,",
                 "\n        gf.f_actualiza",
                 "\n FROM   glo_folio gf, glo_ctr_archivo gc",
                 "\n WHERE  gf.status      = 0",
                 "\n AND    gf.proceso_cod = 933 ",
                 "\n AND gf.folio          = gc.folio"
  IF LENGTH(p_fecha) > 0 THEN
     LET v_QryTxt = v_QryTxt||"\n AND gf.f_actualiza = '",p_fecha,"'",
                              "\n ORDER BY 1 DESC"
  ELSE
     LET v_QryTxt = v_QryTxt||"\n ORDER BY 1 DESC"
  END IF

  --DISPLAY "Llena combo ",v_QryTxt

  -- Prepara la consulta para obtener folios liquidados
  PREPARE prp_Cons_Folio_Hist FROM v_QryTxt
   
  -- Limpia el combo
  CALL cb.clear()

  -- Declara el cursor para la consulta 
  DECLARE cur_llena_combo_folio CURSOR FOR prp_Cons_Folio_Hist
  FOREACH cur_llena_combo_folio INTO var_dis_hist[v_indice].v_folio,
                                     var_dis_hist[v_indice].v_arch_fol,
                                     p_fecha_actualiza
    -- Agrega elementos al combobox
    CALL cb.addItem(var_dis_hist[v_indice].v_folio,
                    var_dis_hist[v_indice].v_arch_fol)
   { DISPLAY var_dis_hist[v_indice].v_folio, " - ", 
            var_dis_hist[v_indice].v_arch_fol, ", ",
            p_fecha_actualiza}
    LET v_indice = v_indice + 1
  END FOREACH

  CALL var_dis_hist.deleteElement(v_indice)
         
  RETURN v_cb_folio, p_fecha_actualiza, v_indice
   
END FUNCTION

#Obejtivo: Funci�n que consulta los folios liquidados 
FUNCTION fn_Folios_Liquidados(p_folio)
  DEFINE arr_folios_liquidados     DYNAMIC ARRAY OF RECORD --almacena folios liquidados 
    f_actualiza                    DATE,          --fecha de actualizacion                                         
    folio                          DECIMAL(9,0),  --n�mero de Folio                                                
    monto_pesos                    DECIMAL(12,2), --monto de pesos                                                
    v_status                       CHAR(53)       --status de folio 
  END RECORD,

  p_folio                          DECIMAL(9,0),  --folio que recibe del combobox   
  v_QryTxt                         STRING,        --cadena para almacenar Query 
  v_indice_f                       INTEGER        --variable de indice
       
  LET v_indice_f = 1

  LET v_QryTxt = "\n SELECT gf.f_actualiza, gf.folio, COUNT(ap.folio_ap_subs),",
                 "\n        gf.status || '-APORTACIONES SUBSECUENTES SIN CONCILIAR RECIBIDOS'",
                 "\n FROM   glo_folio gf",
                 "\n JOIN   dis_as_sin_conciliar ap",
                 "\n ON     gf.folio      = ap.folio_ap_subs",
                 "\n WHERE  gf.status     = 0",
                 "\n AND    gf.folio      = ",p_folio,
                  "\n GROUP BY 2,1,4"

  --DISPLAY "v_QryTxt: ",v_QryTxt
   
  PREPARE prp_Folios_Liq FROM v_QryTxt
   
  -- Declara el cursor para la consulta 
  DECLARE cur_Folios_Liq CURSOR FOR prp_Folios_Liq    
  FOREACH cur_Folios_Liq INTO arr_folios_liquidados[v_indice_f].*
    LET v_indice_f = v_indice_f + 1
  END FOREACH

  CALL arr_folios_liquidados.deleteElement(v_indice_f)
   
  -- Retorna el arreglo con la informaci�n de los folios liquidados y el 
  -- tama�o del arreglo 
  RETURN arr_folios_liquidados, v_indice_f

END FUNCTION

#OBJETIVO: Valida que el Folio Seleccionado de Aportaciones Subsecuentes NO haya sido liquidado o preliquidado
FUNCTION fn_valida_folio_liquidado(p_folio)
  DEFINE 
    p_folio                        DECIMAL(9,0),
    r_count_folio                  INTEGER,
    r_folio_liq                    DECIMAL(9,0),
    r_status                       INTEGER,
    v_indice                       INTEGER,
    v_QryTxt                       STRING
       
  LET v_QryTxt = "\n SELECT COUNT (folio), folio, status",
                 "\n FROM   glo_folio",
                 "\n WHERE  folio   = '",p_folio,"' ",
                 "\n AND    status IN (1,2)",
                 "\n GROUP BY 2,3"

  PREPARE prp_ValidaFolio FROM v_QryTxt   
  --Declara el cursor para la consulta 
  DECLARE cur_ValidaFolio CURSOR FOR prp_ValidaFolio    
  FOREACH cur_ValidaFolio INTO r_count_folio, r_folio_liq, r_status
    LET v_indice = v_indice + 1
  END FOREACH             

  RETURN r_count_folio, r_folio_liq, r_status
   
END FUNCTION