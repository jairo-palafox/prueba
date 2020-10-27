################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 03/04/2018                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo       => DIS                                                           #
#Programa     => DISR06                                                        #
#Objetivo     => Realizar el reverso del rechazo del archivo de avances de pago#
#Fecha inicio => 20/06/2012                                                    #
################################################################################
DATABASE safre_viv
MAIN 
DEFINE v_edit_Folio          VARCHAR(20),--Recibe el valor del folio a consultar
       v_de_Fecha            DATE,       --Recibe el valor de la fecha a consultar
       v_valida_fecha        LIKE dis_det_avance_pago.f_presentacion, --Recibe el valor si la fecha capturada es valida
       v_valida_folio        LIKE dis_det_avance_pago.folio, --Recibe el valor si el folio capturado es valido
       v_NoRechazados        SMALLINT, --Recibe el conteo del No. de folios rechazados
       v_respuesta           SMALLINT, --Recibe el valor que confirma o cancela una acción
       v_des_error           CHAR(150),--Recibe la descripción del error 
       v_no_reg_avpag        SMALLINT, --Almacena el número de registros en el sumario de Av. Pagos
       v_indice              INTEGER   --Indice para los ciclos 
       
DEFINE p_nombre_menu         LIKE seg_menu.opcion, --Asigna el titulo de la ventana
       p_pid                 LIKE bat_ctr_proceso.pid,     --ID del proceso
       p_proceso_cod         LIKE cat_proceso.proceso_cod, --Código del proceso
       p_opera_cod           LIKE cat_operacion.opera_cod, --Código de operacion
       p_usuario             LIKE seg_usuario.usuario_cod, --Clave de usuario
       p_tipo_proc           CHAR(1),    --Recibe el tipo de proceso
       r_bandera             SMALLINT    --Bandera que recibe los valores de las funciones

DEFINE f_ventana             ui.Window,  --Define las propiedades de la Ventana
       f_forma               ui.Form     --Define las propiedades de la forma
       
DEFINE arr_AvancePagos       DYNAMIC ARRAY OF RECORD --Arreglo que recibe los valores del avance de pagos
       folio                 LIKE dis_sum_avance_pago.folio,
       f_presentacion        LIKE dis_sum_avance_pago.f_presentacion,
       tot_registros         LIKE dis_sum_avance_pago.tot_registros,
       tot_aportacion        LIKE dis_sum_avance_pago.tot_aportacion,
       tot_amortizacion      LIKE dis_sum_avance_pago.tot_amortizacion,
       tot_rechazados        LIKE dis_det_avance_pago.estado,
       estado                CHAR(53)
       END RECORD
       
DEFINE 
   r_bnd_actualiza           INTEGER,
   v_QryTxt                  STRING,
   r_bnd_cnt                 SMALLINT,
   r_tipo_rev_cnt            SMALLINT,
   r_bandera_rev             SMALLINT 

DEFINE 
  v_ruta_ejecutable          LIKE seg_modulo.ruta_bin,     --Ruta del ejecutable
  v_ruta_listados            LIKE seg_modulo.ruta_listados,--Ruta del log
  v_s_mensaje                STRING,
  l_comando                  STRING,
  g_sql_txt                  STRING,
  v_proc_entra               SMALLINT,
  v_proc_val                 SMALLINT,
  v_cod_conv                 SMALLINT,
  v_desc_proc_val            CHAR(40),
  v_mensaje_val              STRING
 
  LET p_usuario     = ARG_VAL(1) -- Recibe la variable de usuario
  LET p_tipo_proc   = ARG_VAL(2) -- Recibe el tipo de proceso
  LET p_nombre_menu = ARG_VAL(3) -- Recibe el nombre del programa 
  LET p_proceso_cod = 905 -- Código asignado al proceso de Dispersión (5) 
  LET p_opera_cod   = 2 -- Código asignado a la operación de consulta(3)

  INITIALIZE v_de_Fecha   TO NULL  -- Inicializa la variable fecha a nulo
  INITIALIZE v_edit_Folio TO NULL  -- Inicializa la variable folio a nulo      

  --Abre la ventana para ingresar parámetros de consulta    
  CLOSE WINDOW SCREEN 

  OPEN WINDOW vtn_AvancePagos WITH FORM "DISR061"
    LET r_bnd_cnt      = 0
    LET r_tipo_rev_cnt = 0
    LET r_bandera_rev  = 0
   
    LET f_ventana = ui.Window.getCurrent()
    LET f_forma   = f_ventana.getForm()

    --Se invoca la función que asigna el titulo a la ventana
    CALL ui.Interface.setText(p_nombre_menu)

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

    CALL f_forma.setElementHidden("gr_resregavpag", 1) --Oculta la Sección Resúmen Registro Av. Pagos
    CALL f_forma.setElementHidden("reverso", 1)        --Oculta el botón de Reverso
   
    --Se obtienen los parámetros de fecha y folio para consulta
    INPUT BY NAME v_edit_Folio, v_de_Fecha WITHOUT DEFAULTS
    ATTRIBUTES (ACCEPT = FALSE, CANCEL = FALSE, UNBUFFERED)
      -- Botón aceptar que realiza la consulta en base a folio y fecha
      ON ACTION ACCEPT
         -- Valida que la fecha capturada no sea mayor a la fecha actual
         IF ( v_de_Fecha > TODAY ) THEN
            CALL fn_mensaje("ERROR", "Fecha posterior a fecha actual", "information")
            NEXT FIELD v_de_Fecha
         ELSE
            --Valida que el folio capturado exista en la tabla de historico
            CALL fn_valida_parametros(v_edit_Folio, v_de_Fecha) 
            RETURNING v_valida_folio, v_valida_fecha

            -- Si el folio no existe en el histórico envia mensaje 
            IF (v_valida_folio IS NULL OR v_valida_folio = 0) THEN
               CALL fn_mensaje("ERROR", "No existe información con los datos capturados", "information")
               NEXT FIELD v_edit_Folio
            ELSE
               --Si el folio existe, consulta el número de registros rechazados
               CALL fn_CalculaRechazados(v_edit_Folio, v_de_Fecha) RETURNING v_NoRechazados
            END IF
         END IF
            
         --Cuando el existen registros con estado de rechazo 
         IF ( v_valida_folio >= 1 OR v_valida_fecha >= 1 ) THEN --OR v_NoRechazados >= 1  THEN
            -- Llama la función que consulta los datos a mostrar en el resumen 
            CALL fn_consulta_AvPago(v_edit_Folio, v_de_Fecha, p_proceso_cod) 
            RETURNING arr_AvancePagos, v_indice
               
            IF ( v_indice <= 1 ) THEN
               CALL fn_mensaje ("ERROR", "No existen registros a reversar", "error")
               NEXT FIELD v_edit_Folio
            ELSE 
               CALL f_forma.setElementHidden("gr_resregavpag", 0) --Muestra la Sección Resúmen Registro Av. Pagos
               CALL f_forma.setElementHidden("reverso", 0)  --Muestra el botón de Reverso
               CALL f_forma.setElementHidden("accept", 1)   --Oculta el botón de Aceptar  
            END IF 
            
            --Muestra información del Resúmen de Avance de Pagos
            DISPLAY ARRAY arr_AvancePagos TO scr_ResAvPag.*
            ATTRIBUTES (ACCEPT = FALSE, CANCEL = FALSE)
              --Si se solicita la ejecución del reverso 
              ON ACTION reverso
                 LET v_edit_Folio = arr_AvancePagos[arr_curr()].folio
                 CALL fn_ventana_confirma("REVERSO", 
                                          "¿Está seguro que desea realizar el reverso?", 
                                          "quest")
                 RETURNING v_respuesta

                 --Se obtiene pid de la operación de acuerdo al folio
                 SELECT DISTINCT pid 
                 INTO   p_pid
                 FROM   bat_ctr_operacion
                 WHERE  folio = v_edit_Folio
                 --Si no existe el pid de acuerdo al folio, traerá el último
                 IF p_pid IS NULL OR p_pid = 0 THEN
                    CALL fn_max_pid(p_proceso_cod, 1) RETURNING p_pid 
                 END IF 
                        
                 --Llama la función para validar la ejecucución del reverso
                 CALL fn_valida_reverso(p_pid, p_proceso_cod, p_opera_cod)
                 RETURNING r_bandera_rev

                 IF r_bandera_rev <> 0 THEN 
                    CALL fn_muestra_inc_operacion(r_bandera_rev)
                    EXIT PROGRAM 
                 END IF 
                        
                 --Si el usuario confirma la ejecución del reverso
                 IF ( v_respuesta = 1 ) THEN 
                    --Se integra la función para validar que se puede realizar el reverso
                    --de acuerdo al registro de la póliza contable
                    LET v_QryTxt = "EXECUTE PROCEDURE fn_reverso_reg_cnt(?,?)"
                        
                    PREPARE prp_reverso_reg_cnt FROM v_QryTxt
                    EXECUTE prp_reverso_reg_cnt USING v_edit_Folio,p_proceso_cod INTO r_bnd_cnt, r_tipo_rev_cnt

                    IF r_bnd_cnt = 0 THEN
                       CALL fn_rutas("dis") RETURNING v_ruta_ejecutable, v_ruta_listados
                  
                       LET l_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,
                                       "/DISR061.42r ",p_usuario," ",p_pid," ",
                                       p_proceso_cod," ",p_opera_cod," ",v_edit_Folio," ",
                                       " '","NA","' 1>", v_ruta_listados CLIPPED ,
                                       "/nohup:",p_pid USING "&&&&&",":",
                                       p_proceso_cod USING "&&&&&",":",
                                       p_opera_cod USING "&&&&&" ," 2>&1 &"
                       RUN l_comando

                       LET v_s_mensaje = "Se ha enviado el Reverso del Rechazo de Avance de Pagos con el pid: ",
                                         p_pid CLIPPED,
                                         ".\nPuede revisar el estado del proceso en el monitor de ejecución de procesos."
                       CALL fn_mensaje("Reverso",v_s_mensaje,"information")

                       EXIT PROGRAM 
                    ELSE 
                       IF r_bnd_cnt = 1 THEN 
                          CALL fn_mensaje("ERROR","No existe el registro contable del proceso","error")
                       END IF

                       IF r_bnd_cnt = 2 THEN 
                          CALL fn_mensaje("ERROR","La fecha de emisión del registro contable es diferente al día de hoy","error")
                       END IF

                       IF r_bnd_cnt = 3 THEN 
                          CALL fn_mensaje("ERROR","La póliza contable ya fue generada","error")
                       END IF 
                    END IF 
                 ELSE  
                    --Si el usuario cancela el reverso, envía mensaje de cancelación
                    CALL fn_mensaje("REVERSO", "Operación Cancelada", "information")
                    EXIT DISPLAY
                 END IF 

                 --Botón de cancelar y salir de la opción que muestra detalles de Reverso
                 ON ACTION CANCEL
                    EXIT DISPLAY
            END DISPLAY
         ELSE 
            --Si no existen registros rechazados
            CALL fn_mensaje("Error", "Información no existe", "information")                      
         END IF

         EXIT INPUT

         --Botón de cancelar y salir de la opción de Reverso
         ON ACTION CANCEL
            EXIT INPUT

    END INPUT  
END MAIN

#OBJETIVO: Calcular el número de rechazados 
FUNCTION fn_CalculaRechazados(p_folio, p_fecha) 
DEFINE r_No_Rechazos         SMALLINT,
       p_folio               SMALLINT,
       p_fecha               DATE

  --Consulta que cuenta los rechazados
  --Valida que exista fecha si es 0, no se considera en la consulta
  IF length(p_fecha) = 0 THEN    
     SELECT COUNT(folio) 
     INTO   r_No_Rechazos
     FROM   dis_rch_avance_pago
     WHERE  folio = p_folio
  ELSE 
     SELECT COUNT(folio) 
     INTO   r_No_Rechazos
     FROM   dis_rch_avance_pago
    WHERE   f_pago = p_fecha
  END IF 

  RETURN r_No_Rechazos
   
END FUNCTION

#OBJETIVO: Realiza consulta de datos en base a fecha y folio 
--FUNCION SIN SP--
FUNCTION fn_consulta_AvPago(p_folio, p_fecha, p_proceso_cod) 
DEFINE arr_Reg_AvPagos       DYNAMIC ARRAY OF RECORD
       folio                 LIKE dis_sum_avance_pago.folio,
       f_presentacion        LIKE dis_sum_avance_pago.f_presentacion,
       tot_registros         LIKE dis_sum_avance_pago.tot_registros,
       tot_aportacion        LIKE dis_sum_avance_pago.tot_aportacion,
       tot_amortizacion      LIKE dis_sum_avance_pago.tot_amortizacion,
       tot_rechazados        LIKE dis_det_avance_pago.estado,
       estado                CHAR(53)
       END RECORD,
       p_folio               LIKE dis_sum_avance_pago.folio,--Parámetro de Folio
       p_fecha               LIKE dis_sum_avance_pago.f_presentacion, --Parámetro de Fecha
       p_proceso_cod         LIKE cat_proceso.proceso_cod,  --codigo del proceso
       v_QryTxt              STRING,     --Cadena para almacenar Query 
       v_indice              INTEGER,    --Variable de indice
       v_folio               DECIMAL(10,0)

  LET v_QryTxt = "\n SELECT ds.folio, ds.f_presentacion, ds.tot_registros,",
                 "\n        ds.tot_aportacion, ds.tot_amortizacion,",
                 "\n        ga.estado || '-' || ",
                 "\n        ea.estado_descripcion AS ESTADO",
                 "\n FROM   dis_sum_avance_pago ds, ", 
                 "\n        glo_ctr_archivo ga, ",
                 "\n        cat_edo_archivo ea ",
                 "\n WHERE  ea.estado_cod  = ga.estado",
                 "\n AND    ga.proceso_cod = ",p_proceso_cod,"",
                 "\n AND    ga.estado      = 2",
                 "\n AND    ds.estado      = 40"

  --Si se capturan ambos parametros 
  IF length(p_fecha) > 0 AND length(p_folio) > 0 THEN
     LET v_QryTxt = v_QryTxt || "\n    AND ds.folio          = ",p_folio,"",
                                "\n    AND ds.f_presentacion = '",p_fecha,"'"
  END IF 
   
  --Consulta por Folio 
  --Valida que exista fecha si es 0, no se considera en la consulta
  IF length(p_fecha) = 0 THEN                       
     LET v_QryTxt = v_QryTxt || "\n    AND ds.folio = ",p_folio,"" 
  ELSE 
     --Consulta por Fecha
     --Si el parámetro fecha trae algún valor lo incluye en la consulta   
     LET v_QryTxt = v_QryTxt || "\n    AND ds.f_presentacion = '",p_fecha,"'"                                   
  END IF

  LET v_QryTxt = v_QryTxt || "\n GROUP BY 1,2,3,4,5,6"
  DISPLAY "PRIMERA CONSULTA",v_QryTxt      

  --Prepara la consulta para el display
  PREPARE prp_Reg_AvPagos FROM v_QryTxt
  LET v_indice = 1
  --Declara el cursor para la consulta
  DECLARE cur_RegAvancePagos CURSOR FOR prp_Reg_AvPagos
  FOREACH cur_RegAvancePagos INTO arr_Reg_AvPagos[v_indice].folio, 
                                  arr_Reg_AvPagos[v_indice].f_presentacion, 
                                  arr_Reg_AvPagos[v_indice].tot_registros,
                                  arr_Reg_AvPagos[v_indice].tot_aportacion, 
                                  arr_Reg_AvPagos[v_indice].tot_amortizacion
                                  --arr_Reg_AvPagos[v_indice].estado

    LET v_folio = arr_Reg_AvPagos[arr_curr()].tot_rechazados

    -- Cuenta el número de regstros con rechazo  
    LET v_QryTxt = "\n SELECT COUNT(folio)",
                   "\n FROM   dis_rch_avance_pago",                        
                   "\n WHERE  folio          = ?",
                   "\n AND    f_presentacion = ?"
                        
    PREPARE prp_sql_count_rch FROM v_QryTxt
    EXECUTE prp_sql_count_rch 
    USING arr_Reg_AvPagos[v_indice].folio,
          arr_Reg_AvPagos[v_indice].f_presentacion  
    INTO arr_Reg_AvPagos[v_indice].tot_rechazados--, v_folio

    SELECT estado 
    INTO   arr_Reg_AvPagos[v_indice].estado
    FROM   glo_ctr_archivo
    WHERE  folio = arr_Reg_AvPagos[v_indice].folio
         
    SELECT estado_descripcion 
    INTO   arr_Reg_AvPagos[v_indice].estado
    FROM   cat_edo_archivo
    WHERE  estado_cod = arr_Reg_AvPagos[v_indice].estado

    --LET arr_Reg_AvPagos[v_indice].tot_registros = arr_Reg_AvPagos[v_indice].tot_registros + arr_Reg_AvPagos[v_indice].tot_rechazados
    LET v_indice = v_indice + 1
  END FOREACH

  --LET v_indice = v_indice - 1
  CALL arr_Reg_AvPagos.deleteElement(v_indice)

  RETURN arr_Reg_AvPagos, v_indice

END FUNCTION

#OBJETIVO: Validar que el folio y la fecha capturados sean validos
FUNCTION fn_valida_parametros(p_folio,p_fecha)
DEFINE p_folio               DECIMAL(9,0),
       p_fecha               DATE,
       v_folio_valido        LIKE dis_det_avance_pago.folio,
       v_fecha_valida        LIKE dis_det_avance_pago.f_presentacion,--DATE,
       v_qry_txt             STRING

  LET v_qry_txt = "\n SELECT folio,f_presentacion",
                  "\n FROM   dis_det_avance_pago",
                  "\n WHERE  1      = 1"
                 ,"\n AND    estado = 40"

  IF length(p_folio) > 0 THEN
     LET v_qry_txt = v_qry_txt||"\n AND folio = ",p_folio
  END IF

  IF length(p_fecha) > 0 THEN
     LET v_qry_txt = v_qry_txt||"\n AND f_presentacion = '",p_fecha,"'"
  END IF

  LET v_qry_txt = v_qry_txt||"\n GROUP BY folio, f_presentacion"

  PREPARE prp_valida_folio_dis_det FROM v_qry_txt
  EXECUTE prp_valida_folio_dis_det INTO v_folio_valido,v_fecha_valida

  IF v_folio_valido = 0 OR v_fecha_valida IS NULL OR v_folio_valido IS NULL THEN 
     LET v_qry_txt = "\n SELECT folio,f_presentacion",
                     "\n FROM   dis_rch_avance_pago",
                     "\n WHERE  1 = 1"

     IF length(p_folio) > 0 THEN
        LET v_qry_txt = v_qry_txt||"\n AND folio = ",p_folio
     END IF

     IF length(p_fecha) > 0 THEN
        LET v_qry_txt = v_qry_txt||"\n AND f_presentacion = '",p_fecha,"'"
     END IF

     LET v_qry_txt = v_qry_txt||"\n GROUP BY folio, f_presentacion"

     PREPARE prp_valida_folio_dis_rch FROM v_qry_txt
     EXECUTE prp_valida_folio_dis_rch INTO v_folio_valido,v_fecha_valida
  END IF

  RETURN v_folio_valido,v_fecha_valida
   
END FUNCTION