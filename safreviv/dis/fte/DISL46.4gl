###############################################################################
#Version                    => 1.0.0                                          #
#Fecha ultima modificacion  => 28/09/2015                                     #
###############################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo           => DIS                                                       #
#Programa         => DISL46                                                    #
#Objetivo         => Lanzador de la liquidación de la dispersión de las        #
#                    aportaciones subsecuentes PORTABILIDAD receptora.         #                                      #
#Fecha de Inicio  => 28/09/2015                                                #
################################################################################
DATABASE safre_viv

MAIN 
  DEFINE 
    arr_foliosliquidados     DYNAMIC ARRAY OF RECORD #Arreglo de folios liquidados
    f_actualiza              DATE,          --Fecha de actualizacion
    folio                    DECIMAL(9,0),  --Número de Folio 
    monto_pesos              DECIMAL(12,2), --Monto de pesos
    v_status                 CHAR(53)       --Status del folio
  END RECORD,
 
    --Sección de Variables Locales
    v_cb_folio               DECIMAL(9,0), --Folio que recibe del combobox
    v_de_Fecha               DATE,         --Fecha determina folio
    v_fecha_actualiza        DATE,
    v_indice                 INTEGER,
    v_folio_disp             DECIMAL(9,0),
    --v_folio_dae_dis          DECIMAL (9,0),--Folio para DAE
    l_comando                STRING,
    v_ruta_ejecutable        CHAR(40),
    v_ruta_listados          CHAR(40),

    --Sección de parámetros
    p_usuario                CHAR(20),  --Clave de usuario
    p_tipo_proc              CHAR(1),
    p_nombre_menu            CHAR(40),
    p_proceso_cod            SMALLINT,  --Codigo del proceso
    p_opera_cod              SMALLINT,  --codigo de operacion
    p_pid                    DECIMAL(9,0),
    p_programa               CHAR(10),  

    --Sección de Variables de Retorno
    r_bandera                SMALLINT,
    r_nom_archivo            CHAR(40), --Variable para almacenar el nombre del archivo
    r_folio_liq              DECIMAL(9,0),
    r_count_folio            INTEGER, 
    r_status                 INTEGER,
    v_proceso_cod_pagos      SMALLINT, --Proceso cod de registro de pagos segun folio de pagos
    v_s_mensaje              STRING,
    v_ind_llena_cb           INTEGER

  DEFINE 
    f_ventana                ui.Window,   --Define las propìedades de la Ventana
    f_forma                  ui.Form,     --Define las propiedades de la forma
    cb                       ui.ComboBox  --Variable de Combobox       

  LET p_programa    = "DISL46"
  LET p_usuario     = ARG_VAL(1) --Recibe la variable de usuario
  LET p_tipo_proc   = ARG_VAL(2) --Recibe el tipo de proceso
  LET p_nombre_menu = ARG_VAL(3) --Recibe el nombre del programa 
  LET v_de_Fecha    = ""
  LET p_proceso_cod = 931 --Codigo del Proceso de Dispersion de aportaciones subsecuentes portabilidad receptora (4)(931)
  LET p_opera_cod   = 1   --Codigo de la operacion de liquidación (1)
  LET cb            = ui.ComboBox.forName("v_cb_folio") --Asignación del combo a la forma

  CALL STARTLOG(p_usuario CLIPPED||".DISL46.log")
  
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
  OPEN FORM vtn_ConsultaFolios FROM "DISL461"
    DISPLAY FORM vtn_ConsultaFolios

    -- Función UI que asigna Titulo a la ventana del Programa
    CALL ui.Interface.setText(p_nombre_menu)

    LET f_ventana = ui.Window.getCurrent()
    LET f_forma   = f_ventana.getForm()

    --Oculta botón Preliquida 
    CALL f_forma.setElementHidden("consulta_liquidacion",1) 
   
    --Llama función para llenar combo con fecha nula
    CALL fn_llena_combo(v_de_Fecha) RETURNING v_cb_folio,
                                              v_fecha_actualiza,
                                              v_ind_llena_cb

    --Obtiene los parametros de fecha y folio
    INPUT BY NAME v_cb_folio, v_de_Fecha WITHOUT DEFAULTS
    ATTRIBUTES (ACCEPT = FALSE, CANCEL = FALSE, UNBUFFERED)
   
      BEFORE INPUT
        LET v_cb_folio = v_cb_folio

      --CALL fn_mensaje("Info","Selecciona un Folio para Liquidar","about")
      AFTER INPUT
        --Almacena los datos de las variables para enviarlos a las funciones
        CALL GET_FLDBUF(v_cb_folio, v_de_Fecha) RETURNING v_cb_folio, 
                                                          v_de_Fecha

        ON ACTION cancel
           EXIT INPUT

        --Acciones que realiza en el botón de aceptar 
        ON ACTION accept
           --Valida que se capture al menos un campo 
         
           IF (v_de_Fecha IS NOT NULL AND v_cb_folio IS NULL) THEN 
              IF v_de_Fecha > TODAY THEN 
                 CALL fn_mensaje("Error", "Fecha Incorrecta", "information")
                 NEXT FIELD v_de_Fecha
              ELSE
                 --Si la fecha es valida llama la funciòn para llenar el combo con
                 --los folios liquidados en la fecha capturada
                 CALL fn_llena_combo(v_de_Fecha) RETURNING v_cb_folio,
                                                           v_fecha_actualiza, 
                                                           v_ind_llena_cb

                 --Valida que exista información si no la hay, envia mensaje
                 IF v_ind_llena_cb = 1 THEN
                    LET v_fecha_actualiza = NULL 
                    CALL fn_mensaje("Info", 
                                    "No existe información para la Fecha de Liquidación capturada", 
                                    "information")
                    NEXT FIELD v_cb_folio
                 END IF 
              END IF
           END IF

           IF v_fecha_actualiza IS NULL THEN 
              CLEAR FORM
              --Llama función para llenar combo con fecha nula
              CALL fn_llena_combo(v_de_Fecha) RETURNING v_cb_folio,
                                                        v_fecha_actualiza,
                                                        v_ind_llena_cb
           END IF 

           --Si fecha o folio son nulos, muestra mensaje de error
           IF v_de_Fecha IS NULL AND v_cb_folio IS NULL OR v_cb_folio = -1 THEN
           --IF v_de_Fecha IS NULL AND v_cb_folio IS NULL THEN
              CALL fn_mensaje("ERROR", "Debe Capturar al menos un parámetro", "stop")
              NEXT FIELD v_cb_folio
           END IF  

           --Llama función para llenar combo con fecha que el usuario captura
           IF length(v_cb_folio) = 0 THEN
              CALL fn_mensaje("ERROR", "Selecciona un folio", "stop")
              NEXT FIELD v_cb_folio
           END IF 

           CALL fn_Folios_Liquidados(v_cb_folio) RETURNING arr_foliosliquidados, 
                                                           v_indice

           --Valida que exista información si no la hay, envia mensaje
           IF v_indice <= 1 THEN
              CALL fn_mensaje("Info",
                              "No existe información con los parámetros capturados",
                              "info")

           ELSE  
              --Muestra botón Preliquida 
              CALL f_forma.setElementHidden("consulta_liquidacion", 0)

              --Oculta botón Buscar Folios por fecha
              CALL f_forma.setElementHidden("folios", 1)

              --Si existe información que cumpla con los parámetros capturados
              --la muestra en los resultados
              DISPLAY ARRAY arr_foliosliquidados TO sa_HistRegPag.*
              ATTRIBUTES (ACCEPT = FALSE, CANCEL = FALSE)

                AFTER DISPLAY 
                  --Oculta botón Aceptar
                  CALL f_forma.setElementHidden("accept",1)

                  --Botón de salida del Display
                  ON ACTION CANCEL
                     EXIT DISPLAY
               
                  --Ejecuta la liquidación de la Dispersión
                  ON ACTION consulta_liquidacion
                     --Solicita la confirmación para ejecutar el proceso de liquidación
                     CALL fn_ventana_confirma("DISPERSION", "¿Desea ejecutar la liquidación de Aportaciones Subsecuentes Portabilidad Receptora?", "quest")
                     RETURNING r_bandera
               
                     --Si el usuario Acepta 
                     IF r_bandera = 1 THEN
                        --Ejecuta la función para solicitar el nombre del archivo que se cargó
                        CALL fn_recupera_arch_cargado(p_proceso_cod, p_opera_cod) RETURNING r_nom_archivo

                        --Valida que el folio no haya sido liquidado o preliquidado
                        CALL fn_valida_folio_liquidado(v_folio_disp) RETURNING r_count_folio, 
                                                                               r_folio_liq, 
                                                                               r_status 

                        IF r_count_folio >= 1 THEN 
                           IF r_status = 1 THEN 
                              --Si el folio ya fue liquidado o preliquidado envia mensaje
                              CALL fn_mensaje("DISPERSION", "Folio ya Preliquidado en Dispersion", "information")
                           ELSE  
                              IF r_status = 2 THEN 
                                 CALL fn_mensaje("DISPERSION", "Folio ya Liquidado en Dispersion", "information")
                              END IF
                           END IF

                           CALL fn_ventana_confirma("DISPERSION", "¿Desea seleccionar otro folio?", "quest")
                           RETURNING r_bandera
               
                           IF r_bandera = 1 THEN
                              CLEAR FORM
                              CALL fn_llena_combo(v_de_Fecha) RETURNING v_cb_folio,
                                                                        v_fecha_actualiza, 
                                                                        v_ind_llena_cb
                              LET v_cb_folio = -1
                              CALL f_forma.setElementHidden("consulta_liquidacion", 1)
                              CONTINUE INPUT
                           ELSE        
                              EXIT INPUT
                           END IF 
                           
                        ELSE

                           {IF f_existe_proceso_operacion_ejecutando(901, 1) THEN
                              MENU "No se puede ejecutar" ATTRIBUTES ( STYLE="dialog", 
                                 COMMENT="Preliquidación de Dispersión de Pagos ejecutándose,\ningrese a esta opción cuando finalice",
                                 IMAGE="information" )

                                 ON ACTION salir
                                    RETURN
                              END MENU
                           END IF}
                                               
                           --Si se acepta la ejecución se genera PID del proceso
                           CALL fn_genera_pid (p_proceso_cod, p_opera_cod, p_usuario) RETURNING p_pid

                           IF (fn_valida_operacion(p_pid, p_proceso_cod, p_opera_cod) = 0 ) THEN
                              --Enlaza el folio referencia 
                              CALL fn_genera_folio_dis(p_proceso_cod, p_opera_cod, v_cb_folio, p_usuario)
                              RETURNING v_folio_disp

                              --genera folio de referencia para el proceso de DAE - 2400
                              --CALL fn_genera_folio_dis(p_proceso_cod, p_opera_cod, v_folio_disp, p_usuario)
                              --RETURNING v_folio_dae_dis

                              DISPLAY "Folio Disp -- ",v_folio_disp
                              --Ejecuta la funcion de preliquida y asigna estatus de LISTO
                              CALL fn_inicializa_proceso (p_pid, p_proceso_cod, p_opera_cod, v_folio_disp, 
                                                          p_programa, r_nom_archivo, p_usuario)
                              RETURNING r_bandera

                              SELECT UNIQUE proceso_cod
                              INTO   v_proceso_cod_pagos
                              FROM   glo_folio
                              WHERE  folio = v_cb_folio
  
                              --Inicia la operación asignando el estatus de PROCESANDO
                              CALL fn_actualiza_opera_ini(p_pid, p_proceso_cod, p_opera_cod, v_folio_disp, 
                                                          p_programa, r_nom_archivo, p_usuario)
                              RETURNING r_bandera

                              DISPLAY "El folio a enviar de registro de pagos es: ", v_cb_folio
                                             
                              --Hace el llamado a la función que realiza la liquidación.
                              LET l_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/DISP06.42r ",
                                              v_folio_disp," ",
                                              v_cb_folio," ",
                                              v_proceso_cod_pagos," ",
                                              v_fecha_actualiza," ",
                                              p_usuario, " ",
                                              --v_folio_dae_dis,
                                              " 1>",v_ruta_listados CLIPPED,"/nohup:",
                                              p_pid USING "&&&&&",":",
                                              p_proceso_cod USING "&&&&&",":",
                                              p_opera_cod USING "&&&&&" ," 2>&1 &"
                              RUN l_comando
                           
                              LET v_s_mensaje = "Se ha enviado la liquidación de portabilidad con PID: ",
                                                p_pid CLIPPED,
                                                ".\nPuede revisar el avance del proceso en el monitor de ejecución de procesos"

                              CALL fn_mensaje("DISPERSION", v_s_mensaje, "information") 
                              
                              EXIT DISPLAY
                              EXIT INPUT
                           ELSE
                              CALL fn_muestra_inc_operacion(fn_valida_operacion(p_pid, p_proceso_cod, p_opera_cod))
                           END IF  --De valida operacion
                        END IF 
                     ELSE 
                        --Si el usuario Cancela
                        CALL fn_mensaje("DISPERSION", "Se ha cancelado la liquidación de portabilidad", "information")                     
                        CLEAR FORM
                        EXIT INPUT                     
                     END IF 
                     
              END DISPLAY
              EXIT INPUT
           END IF
           -- END IF
    END INPUT
END MAIN
      
#Objetivo: Genera el ComboBox para la dispersión
FUNCTION fn_llena_combo(p_fecha)               
  DEFINE 
    var_dis_hist             DYNAMIC ARRAY OF RECORD --Arreglo para almacenar el folio
    v_folio                  DECIMAL(9,0),           --Campo de Folio
    v_arch_fol               VARCHAR(60)
  END RECORD,

  p_fecha_actualiza          DATE,     
  v_cb_folio                 DECIMAL(9,0), --Almacena folio en el combobox
  p_fecha                    DATE,         --Parámetro de fecha
  v_indice                   INTEGER,      --Variable del indice  
  v_QryTxt                   STRING,       --Cadena para almacenar Query
  cb                         ui.ComboBox   --Variable de Combobox

  LET cb = ui.ComboBox.forName("v_cb_folio") --Asignación del combo a la forma

  --Validación si el combo es nulo 
  IF cb IS NULL THEN
     ERROR "Form field not found in current form"
     EXIT PROGRAM
  END IF

  LET v_indice = 1
   
  --Validación del tamaño de la fecha si es 0 no la incluye para el Query
  LET v_QryTxt = "\n SELECT gf.folio,",
                 "\n        gf.folio||'-'||gc.nombre_archivo,",
                 "\n        gf.f_actualiza",
                 "\n FROM   glo_folio gf,glo_ctr_archivo gc",
                 "\n WHERE  gf.status = 2",
                 --"\n   AND gf.proceso_cod IN (1401,1403)",
                 "\n AND    gf.proceso_cod = 2804 ", --Se agregan los procesos 101-Registro Pag en Aclara y 110-Carga Inicial Aclara 
                 "\n AND    gf.folio   NOT IN (SELECT folio_referencia",
                 "\n                           FROM   glo_folio",
                 "\n                           WHERE  proceso_cod           = 931",
                 "\n                           AND    status               <> 0",
                 "\n                           AND    folio_referencia IS NOT NULL)",
                 "\n   AND gf.folio = gc.folio"
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

#Obejtivo: Función que consulta los folios liquidados 
FUNCTION fn_Folios_Liquidados(p_folio)
  DEFINE 
    arr_folios_liquidados    DYNAMIC ARRAY OF RECORD --Almacena folios liquidados 
    f_actualiza              DATE,          --Fecha de actualizacion                                         
    folio                    DECIMAL(9,0),  --Número de Folio                                                
    monto_pesos              DECIMAL(12,2), --Monto de pesos                                                
    v_status                 CHAR(53)       --Status de folio 
  END RECORD,

  p_folio                    DECIMAL(9,0),  --Folio que recibe del combobox   
  v_QryTxt                   STRING,        --Cadena para almacenar Query 
  v_indice_f                 INTEGER        --Variable de indice
       
  LET v_indice_f = 1
   
  LET v_QryTxt = "\n SELECT gf.f_actualiza,gf.folio,COUNT(prt.folio_liquida),",
                 "\n        gf.status || '-' || ",
                 "\n        ea.estado_descripcion AS ESTADO",
                 "\n FROM   glo_folio gf",
                 "\n JOIN   prt_traspaso_receptora prt",
                 "\n ON     gf.folio      = prt.folio_liquida",
                 "\n JOIN   cat_edo_archivo ea ",
                 "\n ON     ea.estado_cod = gf.status",                                          
                 "\n WHERE  gf.status    IN (1,2)",
                 "\n AND    gf.folio      = ",p_folio,
                 "\n GROUP BY 2,1,4" 

  PREPARE prp_Folios_Liq FROM v_QryTxt

   DISPLAY v_QryTxt
   
  -- Declara el cursor para la consulta 
  DECLARE cur_Folios_Liq CURSOR FOR prp_Folios_Liq    
  FOREACH cur_Folios_Liq INTO arr_folios_liquidados[v_indice_f].*
    LET v_indice_f = v_indice_f + 1
  END FOREACH

  CALL arr_folios_liquidados.deleteElement(v_indice_f)
   
  --Retorna el arreglo con la información de los folios liquidados y el 
  --tamaño del arreglo 
  RETURN arr_folios_liquidados, v_indice_f

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
