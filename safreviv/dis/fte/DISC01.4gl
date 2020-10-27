################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 27/03/2018                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISC01                                                   #
#Objetivo          => Programa para consultar el registro de Avances de Pago   #
#Fecha inicio      => 23/12/2011 *                                             #
################################################################################
DATABASE safre_viv

GLOBALS 
  DEFINE --Define variables para detalle
    v_total_reg_apo          DECIMAL(10,0),
    v_total_reg_amo          DECIMAL(10,0),
    v_total_reg_apo_amo      DECIMAL(10,0),
    v_total_reg              DECIMAL(10,0),
    v_total_suma_apo         DECIMAL(22,2),
    v_total_suma_amo         DECIMAL(22,2),
    v_total_reg_num_cred     DECIMAL(10,0),
    v_total_reg_rch          DECIMAL(10,0),
    v_total_reg_rch_om       DECIMAL(10,0),
--    v_total_pago_previo      DECIMAL(10,0),
    arr_total_reg_periodo    DYNAMIC ARRAY OF RECORD 
      v_perido               LIKE dis_rch_avance_pago.periodo_pago,
      v_total_reg_p          DECIMAL(10,0)
    END RECORD 

  DEFINE 
    v_estado_registro        SMALLINT,
    v_nombre_archivo         CHAR(40)

  DEFINE 
    v_edit_Folio             LIKE dis_sum_avance_pago.folio --Recibe el valor del folio a consultar

  DEFINE
    g_sql_txt                STRING,
    v_proc_entra             SMALLINT,
    v_proc_val               SMALLINT,
    v_cod_conv               SMALLINT,
    v_desc_proc_val          CHAR(40),
    v_mensaje_val            STRING

END GLOBALS 

MAIN

  CALL fn_principalConsulta()

END MAIN

#OBJETIVO: Controla las principales opciones de consulta de folios
FUNCTION fn_principalConsulta()
  DEFINE 
    v_de_Fecha               DATE,     --Recibe el valor de la fecha a consultar
    p_usuario                LIKE seg_usuario.usuario_cod, --Clave de usuario
    p_tipo_proc              CHAR(1),
    p_nombre_menu            LIKE seg_menu.opcion,
    v_valida_fecha           SMALLINT, 
    v_valida_folio           DECIMAL(9,0),
    v_indice                 DECIMAL(10,0),
    v_indice_rch             DECIMAL(10,0),
    v_NoRechazados           DECIMAL(10,0),
    v_tit_reporte            STRING,
    v_fecha_present          DATE, 
    v_respuesta              SMALLINT, 
    v_des_error              CHAR(150),   
    l_comando                STRING, 
    v_estado_folio           DECIMAL(20,0),
    v_estado_fecha           DECIMAL(20,0),
    v_ruta_ejecutable        LIKE seg_modulo.ruta_bin,
    v_ruta_listados          LIKE seg_modulo.ruta_listados,--Rute del log
    p_pid                    LIKE bat_ctr_proceso.pid,     --ID del proceso
    p_proceso_cod            LIKE cat_proceso.proceso_cod, --Codigo del proceso
    p_opera_cod              LIKE cat_operacion.opera_cod, --Codigo de operacion
    p_programa               CHAR(10),       
    r_nom_archivo            LIKE glo_ctr_archivo.nombre_archivo, --Variable para almacenar el nombre del archivo 
    r_bandera                SMALLINT,
    v_pid                    DECIMAL(9,0),
    v_s_mensaje              STRING
     
  DEFINE 
    f_ventana                ui.Window, --Define las propìedades de la Ventana
    f_forma                  ui.Form,   --Define las propiedades de la forma
    manejador_rpt            om.SaxDocumentHandler   --Contenedor de Documentos para el reporte

  DEFINE 
    v_ind_for                INTEGER,
    v_inicia                 INTEGER

  DEFINE arr_AvancePagos     DYNAMIC ARRAY OF RECORD --Arreglo que recibe los valores del avance de pagos
    folio                    LIKE dis_sum_avance_pago.folio,
    f_presentacion           LIKE dis_sum_avance_pago.f_presentacion,
    tot_registros            DECIMAL(10,0),
    tot_aportacion           LIKE dis_sum_avance_pago.tot_aportacion,
    tot_amortizacion         LIKE dis_sum_avance_pago.tot_amortizacion,
    tot_rechazados           DECIMAL(10,0),
    estado                   CHAR(52)
    END RECORD
       
  DEFINE arr_VerRechazos     DYNAMIC ARRAY OF RECORD --Arreglo para registros rechazados
    nss                      LIKE afi_derechohabiente.nss,
    num_credito              LIKE dis_det_avance_pago.num_credito,
    periodo_pago             LIKE dis_det_avance_pago.periodo_pago,
    f_pago                   LIKE dis_det_avance_pago.f_pago,
    nrp                      LIKE dis_det_avance_pago.nrp,
    monto_aportacion         LIKE dis_det_avance_pago.monto_aportacion,  
    monto_amortizacion       LIKE dis_det_avance_pago.monto_amortizacion,
    estado                   CHAR(52)
    END RECORD

  --Validación que NO se tenga Preliquidación de Dispersión de Pagos ejecutándose
  {IF f_existe_proceso_operacion_ejecutando(901, 1) THEN
     MENU "No se puede ejecutar"
       ATTRIBUTES ( STYLE="dialog", COMMENT="Preliquidación de Dispersión de Pagos ejecutándose,\ningrese a esta opción cuando finalice",
       IMAGE="information" )

       ON ACTION salir
          RETURN
     END MENU
  END IF}

  LET p_programa    = "DISC01"       
  LET p_usuario     = ARG_VAL(1) --Recibe la variable de usuario
  LET p_tipo_proc   = ARG_VAL(2) --Recibe el tipo de proceso
  LET p_nombre_menu = ARG_VAL(3) --Recibe el nombre del programa
  LET p_proceso_cod = 902        --Proceso Cod Para Avance de Pagos(5)
  LET p_opera_cod   = 3

  CALL fn_max_pid(p_proceso_cod, 1) RETURNING v_pid

  LET p_pid = v_pid
  INITIALIZE v_valida_fecha TO NULL   
  INITIALIZE v_de_Fecha     TO NULL --Inicializa la variable fecha a nulo
  INITIALIZE v_edit_Folio   TO NULL --Inicializa la variable folio a nulo

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
    
  --Abre la ventana para ingresar parámetros de consulta   
  CLOSE WINDOW SCREEN  

  OPEN WINDOW vtn_AvancePagos WITH FORM "DISC011"
    DIALOG ATTRIBUTES(UNBUFFERED) 

      --Se obtienen los parámetros de fecha y folio para consulta
      INPUT v_edit_Folio, v_de_Fecha 
      FROM  f_edit_Folio, f_de_Fecha 

        BEFORE INPUT 
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma   = f_ventana.getForm()
         
          CALL f_forma.setElementHidden("accept", 0) --Muestra el botón Aceptar

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

          CALL f_forma.setElementHidden("gr_resavpag", 1) --Oculta la Sección Resúmen Registro Av. Pagos
          CALL f_forma.setElementHidden("gr_detavpag", 1) --Oculta la sección de Detalle de Registro Avance Pagos
      END INPUT

      --Botón cancel que da salida a la consulta y terminar la captura de los parámetros
      ON ACTION cancelar
         EXIT DIALOG

      --Botón aceptar que realiza la consulta en base a folio y fecha
      ON ACTION ACCEPT
         --Valida que la fecha capturada no sea mayor a la fecha actual
         IF v_de_Fecha > TODAY THEN
            CALL fn_mensaje("Error", 
                            "Fecha posterior a fecha actual", 
                            "information")
            NEXT FIELD f_de_Fecha
         END IF 

         --CONSULTA POR FOLIO 
         IF v_edit_Folio IS NOT NULL THEN
            --Valida que el folio capturado exista en la tabla de historico
            CALL fn_valida_folio_hist(v_edit_Folio) RETURNING v_valida_folio

            --Si el folio no existe en el histórico envia mensaje 
            IF (v_valida_folio) = 0   THEN
               CALL fn_mensaje("Error", 
                               "El folio capturado no existe", 
                               "information")
               NEXT FIELD f_edit_Folio
            ELSE               
               --Si el folio capturado existe en el histórico ejecuta la consulta
               CALL fn_consulta_AvPago(v_edit_Folio, v_de_Fecha, p_proceso_cod) 
               RETURNING arr_AvancePagos, v_indice
            END IF

            LET v_estado_folio = 0.00

            --Valida si el folio ya está registrado  
            SELECT estado
            INTO   v_estado_folio
            FROM   dis_det_avance_pago
            WHERE  folio   = v_edit_Folio
            AND    estado  = 10 
            AND    estado <> 50
            GROUP BY estado
         END IF

         DISPLAY "v_edit_Folio ",v_edit_Folio
         
         --CONSULTA POR FECHA
         IF v_de_Fecha IS NOT  NULL THEN
            --Valida que la fecha exista en la tabla de histórico
            CALL fn_valida_fecha(v_de_Fecha) RETURNING v_valida_fecha

            --Si la fecha proporcionada no se encuentra en el histórico envia mensaje
            IF v_valida_fecha = 0 THEN  
               CALL fn_mensaje("Error", 
                               "La fecha capturada no existe", 
                               "information")
               NEXT FIELD f_de_Fecha
            ELSE
               --Si la fecha capturado existe en el histórico ejecuta la consulta                     
               CALL fn_consulta_AvPago(v_edit_Folio, v_de_Fecha, p_proceso_cod) 
               RETURNING arr_AvancePagos, v_indice                                 
            END IF         
         END IF

         LET v_estado_fecha = 0.00

         --Valida si el folio ya está registrado  
         SELECT estado
         INTO   v_estado_fecha
         FROM   dis_det_avance_pago
         WHERE  f_presentacion = v_de_Fecha
         AND    estado         = 10 
         AND    estado        <> 50
         GROUP BY estado  

         --Valida el tamaño del arreglo de Avance de pago si existe ejecuta lo siguiente
         IF v_indice > 1 THEN
            --Obtiene el Numero de Rechazos 
            LET v_NoRechazados = arr_AvancePagos[arr_curr()].tot_rechazados

            --Muestra la Sección Resúmen Registro Av. Pagos
            CALL f_forma.setElementHidden("gr_resavpag", 0) 

            CALL fn_valida_operacion(p_pid, p_proceso_cod, p_opera_cod)
            RETURNING r_bandera
            
            --Si el arreglo contiene información la muestra en el resultado de la consulta
            DISPLAY ARRAY arr_AvancePagos TO scr_ResAvPag.*         
              BEFORE DISPLAY
                --Valida si existen rechazos y habilita la opción de ver rechazos
                IF v_NoRechazados <> 0 THEN
                   CALL DIALOG.setActionHidden("accept",1)  --Oculta el botón Aceptar
                   CALL DIALOG.setActionHidden("registro",1)--Oculta botón Registro
                   CALL DIALOG.setActionHidden("rechazos",0)--Muestra botón Ver Rechazos
                   CALL DIALOG.setActionHidden("reporte",0) --Muestra botón Reporte                   
                ELSE
                   --Si no existen rechazos habilita la opción de registro
                   CALL DIALOG.setActionHidden("registro",0) --Muestra botón Registro
                   CALL DIALOG.setActionHidden("reporte", 0) --Muestra botón Reporte
                   CALL DIALOG.setActionHidden("rechazos",1) --Oculta  botón Ver Rechazos
                   CALL DIALOG.setActionHidden("accept",1)   --Oculta el botón Aceptar 

                   IF v_estado_folio <> 10 OR v_estado_fecha <> 10 THEN 
                      CALL DIALOG.setActionHidden("registro", 1) --Oculta botón Registro
                   END IF
                  
                   IF v_estado_folio = 10 OR v_estado_fecha = 10 THEN 
                      CALL DIALOG.setActionHidden("registro", 0) --Muestra botón Registro"              
                   END IF
                END IF

              --Acciones del Botón de Registro
              ON ACTION registro
                 LET v_edit_Folio = arr_AvancePagos[arr_curr()].folio                     
              
                 CALL fn_valida_operacion(p_pid, p_proceso_cod, p_opera_cod)
                 RETURNING r_bandera

                 IF r_bandera = 0 THEN
                    --Solicita confirmar(1) o cancel(0) la operación de Registro
                    CALL fn_ventana_confirma("Registro de Avance de Pago", 
                                             "¿Desea ejecutar el proceso de Registro de Avance de Pago?", 
                                             "quest")
                    RETURNING v_respuesta

                    --Si el usuario confirma la ejecución del proceso de registro 
                    IF v_respuesta = 1 THEN 
                       --Ejecuta la función para solicitar el nombre del archivo que se cargó
                       CALL fn_recupera_arch_cargado(p_proceso_cod, p_opera_cod)
                       RETURNING r_nom_archivo

                       --Inicia la operación asignando el estatus de PROCESANDO 
                       CALL fn_actualiza_opera_ini(p_pid, 
                                                   p_proceso_cod, 
                                                   p_opera_cod, 
                                                   v_edit_folio, 
                                                   p_programa, 
                                                   r_nom_archivo, 
                                                   p_usuario) 
                       RETURNING r_bandera

                       IF r_bandera = 0 THEN 
                          LET l_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,
                                          "/DISS02.42r ",p_usuario," ",p_pid," ",
                                          p_proceso_cod," ",p_opera_cod," ",v_edit_Folio," ",
                                          " '",r_nom_archivo,"' 1>", v_ruta_listados CLIPPED ,
                                          "/nohup:",p_pid USING "&&&&&",":",
                                          p_proceso_cod USING "&&&&&",":",
                                          p_opera_cod USING "&&&&&" ," 2>&1 &"
                                 
                          DISPLAY "v_comando = \n",l_comando
                          RUN l_comando

                          LET v_s_mensaje = "Se ha enviado el Registro de Avances de Pago con el PID: ",
                                             p_pid CLIPPED,
                                           ".\nPuede revisar el avance del proceso en el monitor de ejecución de procesos"
                          CALL fn_mensaje("Integración",v_s_mensaje,"information")
                          EXIT PROGRAM 
                       ELSE 
                          CALL fn_muestra_inc_operacion(r_bandera)
                          EXIT PROGRAM 
                       END IF 
                    ELSE 
                       --Si el usuario desea cancel la operacion envia mensaje de cancelación 
                       CALL fn_mensaje ("Registro Cancelado","Se ha cancelado el proceso de Registro de Avance de Pago","information")
                       EXIT PROGRAM 
                    END IF   
                 ELSE
                    --Envía mensaje con la descripción del Error
                    SELECT descripcion 
                    INTO   v_des_error
                    FROM   cat_bat_parametro_salida
                    WHERE  cod_salida = r_bandera
                   
                    CALL fn_mensaje("CONSULTA", v_des_error, "information")
                 END IF  

              --Si si existen rechazos, acciones del Botón Ver Rechazos                 
              ON ACTION rechazos
                 --Si existen rechazos muestra en la forma el detalle de los rechazos            
                 CALL f_forma.setElementHidden("gr_detavpag", 0)

                 --Llama la función para ver detalles de los folios rechazados 
                 LET arr_AvancePagos[arr_curr()].folio = arr_AvancePagos[arr_curr()].folio CLIPPED
                 LET v_edit_folio                      = arr_AvancePagos[arr_curr()].folio

                 --Obtiene la fecha de presentación para presentar en el reporte 
                 LET arr_AvancePagos[arr_curr()].f_presentacion = arr_AvancePagos[arr_curr()].f_presentacion CLIPPED
                 LET v_fecha_present                            = arr_AvancePagos[arr_curr()].f_presentacion

                 --Llama la función que consulta los rechazos que existen 
                 CALL fn_MuestraRechazos(v_edit_Folio) 
                 RETURNING arr_VerRechazos, v_indice_rch    

                 --Muestra en el detalle los registros rechazados
                 DISPLAY ARRAY arr_VerRechazos TO scr_VerRechazos.*
                   ATTRIBUTES (ACCEPT = FALSE, CANCEL = FALSE)                  

                   --Botón que soliicita la ejecucion del reporete de Rechazos 
                   ON ACTION reporte
                      CALL fn_obtiene_nombre_archivo(v_edit_Folio)

                      IF v_indice_rch > 40000 THEN
                         --Se asigna la plantilla para generar el reporte
                         IF fgl_report_loadCurrentSettings("DISC013.4rp") THEN
                            CALL fgl_report_selectDevice ("SVG")
                            LET manejador_rpt = fgl_report_commitCurrentSettings()
                         END IF
                      ELSE
                         --Se asigna la plantilla para generar el reporte
                         IF fgl_report_loadCurrentSettings("DISC013.4rp") THEN
                            CALL fgl_report_selectDevice ("PDF")
                            LET manejador_rpt = fgl_report_commitCurrentSettings()
                         END IF
                      END IF

                      --Inicia el reporte de registros con rechazo
                      START REPORT reporte_Rechazados TO XML HANDLER manejador_rpt
                        --Asigna el titulo del reporte
                        LET v_tit_reporte = "CONSULTA RECHAZOS REGISTRO AVANCES DE PAGO"

                        FOR v_inicia = 1 TO v_indice_rch
                            OUTPUT TO REPORT reporte_Rechazados(arr_VerRechazos[v_inicia].*,
                                                                v_edit_folio, 
                                                                p_usuario,
                                                                v_fecha_present, 
                                                                v_tit_reporte )
                        END FOR
                      FINISH REPORT reporte_Rechazados 
                      -- Botón de salida de la ventana de Rechazos

                   ON ACTION cancelar
                      CALL f_forma.setElementHidden("gr_detavpag", 1) --Oculta la sección de Detalle de Registro Avance Pagos
                      EXIT DISPLAY              
                 END DISPLAY                     
          
              --Botón para generar el reporte en PDF de la consulta
              ON ACTION reporte
                 IF fgl_report_loadCurrentSettings("DISC012.4rp") THEN 
                    CALL fgl_report_selectDevice ("PDF")
                    LET manejador_rpt = fgl_report_commitCurrentSettings()
                 END IF
                
                 --Inicia el reporte de registros con rechazo
                 CALL fn_obtiene_detalle_reporte()

                 START REPORT rp_avance_pago TO XML HANDLER manejador_rpt               
                   --Asigna el titulo del reporte             
                   LET v_tit_reporte = "Reporte de Registros Avance de Pago"

                   CALL fn_obtiene_nombre_archivo(v_edit_folio)
                  
                   FOR v_ind_for = 1 TO arr_total_reg_periodo.getLength()
                       OUTPUT TO REPORT rp_avance_pago(arr_AvancePagos[v_ind_for].*,
                                                      arr_total_reg_periodo[v_ind_for].*,
                                                      v_edit_folio,
                                                      p_usuario,
                                                      v_fecha_present)
                   END FOR
                 FINISH REPORT rp_avance_pago
                
              --Botón de cancel para salir de la consulta                       
              ON ACTION CANCEL 
                 CALL f_forma.setElementHidden("gr_resavpag",1) --Oculta la Sección Resúmen Registro Av. Pagos
                 CALL DIALOG.setActionHidden("reporte",1)  --Oculta botón Reporte
                 CALL DIALOG.setActionHidden("accept",0)   --Muestra el botón Aceptar 
                 CALL DIALOG.setActionHidden("registro",1) --Oculta botón Registro
                 CALL DIALOG.setActionHidden("rechazos",1) --Oculta el botón Rechazos

                 EXIT DISPLAY
            END DISPLAY
         ELSE
            --Si el arreglo no encuentra informaciòn con los parametros 
            --de folio y fecha proporcionados por el usuario.
            CALL fn_mensaje("INFO", 
                            "No se encontraron registros con los criterios proporcionados", 
                            "information")
            NEXT FIELD f_edit_Folio
         END IF
    END DIALOG 
  CLOSE WINDOW vtn_AvancePagos
END FUNCTION 

#OBJETIVO: Realiza consulta de datos en base a fecha y folio
FUNCTION fn_consulta_AvPago(p_folio, p_fecha, p_proceso_cod) 
  DEFINE arr_Reg_AvPagos     DYNAMIC ARRAY OF RECORD
    folio                    LIKE dis_sum_avance_pago.folio,
    f_presentacion           LIKE dis_sum_avance_pago.f_presentacion,
    tot_registros            DECIMAL(10,0),
    tot_aportacion           LIKE dis_sum_avance_pago.tot_aportacion,
    tot_amortizacion         LIKE dis_sum_avance_pago.tot_amortizacion,
    tot_rechazados           DECIMAL(10,0),
    estado                   CHAR(52)
    END RECORD,

    p_folio                  LIKE dis_sum_avance_pago.folio,          --Parámetro de Folio
    p_fecha                  LIKE dis_sum_avance_pago.f_presentacion, --Parámetro de Fecha
    p_proceso_cod            SMALLINT,
    v_QryTxt                 STRING,          --Cadena para almacenar Query 
    v_indice                 DECIMAL(10,0),   --Variable de indice
    v_folio                  DECIMAL(10,0),
    v_tot_detalle            DECIMAL(10,0)

    LET v_QryTxt = "\n SELECT ds.folio, ds.f_presentacion, ds.tot_registros,",
                   "\n        ds.tot_aportacion, ds.tot_amortizacion,",
                   "\n        ga.estado || '-' || ",
                   "\n        ea.estado_descripcion AS ESTADO",
                   "\n FROM   dis_sum_avance_pago ds, ", 
                   "\n        glo_ctr_archivo ga, ",
                   "\n        cat_edo_archivo ea ",
                   "\n WHERE  ea.estado_cod  = ga.estado",
                   "\n AND    ga.proceso_cod = ", p_proceso_cod,
                   "\n AND    ga.estado      = 2"

    --Si se capturan ambos parametros 
    IF length(p_fecha) > 0 AND length(p_folio) > 0 THEN
       LET v_QryTxt = v_QryTxt || "\n    AND ds.folio = ",p_folio,
                                  "\n    AND ds.f_presentacion = '",p_fecha,"'"
    END IF 
   
    --Consulta por Folio 
    --Valida que exista fecha si es 0, no se considera en la consulta
    IF length(p_fecha) = 0 THEN                       
       LET v_QryTxt = v_QryTxt || "\n    AND ds.folio = ",p_folio
    ELSE 
       --Consulta por Fecha
       --Si el parámetro fecha trae algún valor lo incluye en la consulta   
       LET v_QryTxt = v_QryTxt || "\n    AND ds.f_presentacion = '",p_fecha,"'"                                   
    END IF

    LET v_QryTxt = v_QryTxt || "\n GROUP BY 1,2,3,4,5,6"
    DISPLAY v_QryTxt

    --Prepara la consulta para el display
    PREPARE prp_Reg_AvPagos FROM v_QryTxt

    LET v_indice = 1
 
    --Declara el cursor para la consulta
    DECLARE cur_RegAvancePagos CURSOR FOR prp_Reg_AvPagos
    FOREACH cur_RegAvancePagos INTO arr_Reg_AvPagos[v_indice].folio, 
                                    arr_Reg_AvPagos[v_indice].f_presentacion, 
                                    arr_Reg_AvPagos[v_indice].tot_registros,
                                    arr_Reg_AvPagos[v_indice].tot_aportacion, 
                                    arr_Reg_AvPagos[v_indice].tot_amortizacion,
                                    arr_Reg_AvPagos[v_indice].estado

      LET v_folio = arr_Reg_AvPagos[v_indice].folio
         
      LET v_edit_Folio = v_folio
      DISPLAY "en consulta ", v_edit_Folio

      --Obtenemos total de registros en dis_det
      SELECT COUNT(*), 
             SUM(monto_aportacion),  
             SUM(monto_amortizacion)
      INTO   v_tot_detalle,
             arr_Reg_AvPagos[v_indice].tot_aportacion, 
             arr_Reg_AvPagos[v_indice].tot_amortizacion 
      FROM   dis_det_avance_pago
      WHERE  folio = v_edit_Folio
      IF v_tot_detalle = 0 OR v_tot_detalle IS NULL THEN 
         LET v_tot_detalle                              = 0
         LET arr_Reg_AvPagos[v_indice].tot_aportacion   = 0 
         LET arr_Reg_AvPagos[v_indice].tot_amortizacion = 0
      END IF 

      --Obtenemos total de registros en dis_rch
      SELECT COUNT(*)
      INTO   arr_Reg_AvPagos[v_indice].tot_registros
      FROM   dis_rch_avance_pago
      WHERE  folio = v_edit_Folio
      IF arr_Reg_AvPagos[v_indice].tot_registros     = 0 OR 
         arr_Reg_AvPagos[v_indice].tot_registros IS NULL THEN 
         LET arr_Reg_AvPagos[v_indice].tot_registros = 0
      END IF 

      --Realizamos suma de registros procesados
      LET arr_Reg_AvPagos[v_indice].tot_registros = v_tot_detalle + arr_Reg_AvPagos[v_indice].tot_registros
      
      LET v_QryTxt = "\n SELECT COUNT(folio)",
                     "\n FROM   dis_rch_avance_pago",                        
                     "\n WHERE  folio          = ?",
                     "\n AND    f_presentacion = ?"

      PREPARE prp_sql_count_rch FROM v_QryTxt
      EXECUTE prp_sql_count_rch 
      USING arr_Reg_AvPagos[v_indice].folio,
            arr_Reg_AvPagos[v_indice].f_presentacion  
      INTO arr_Reg_AvPagos[v_indice].tot_rechazados

      LET v_indice = v_indice + 1
    END FOREACH

    CALL arr_Reg_AvPagos.deleteElement(v_indice)

    --Retorna el arreglo con la información de los folios liquidados y el 
    --tamaño del arreglo 
    RETURN arr_Reg_AvPagos, v_indice

END FUNCTION

#OBJETIVO: Validar que el folio capturado exista en el histórico
FUNCTION fn_valida_folio_hist(p_folio)
  DEFINE 
    p_folio                  DECIMAL(9,0),
    r_folio_valido           DECIMAL(10,0) 

  SELECT COUNT(folio) 
  INTO   r_folio_valido
  FROM   dis_det_avance_pago
  WHERE  folio = p_folio
  IF r_folio_valido = 0 OR r_folio_valido IS NULL THEN 
     SELECT COUNT(folio) 
     INTO   r_folio_valido
     FROM   dis_rch_avance_pago
     WHERE  folio = p_folio
  END IF 

  RETURN r_folio_valido

END FUNCTION 

#OBJETIVO: Validar que la fecha capturada exista en el histórico
FUNCTION fn_valida_fecha(p_fecha)
  DEFINE 
    p_fecha                  DATE,
    r_fecha_valida           SMALLINT

  SELECT COUNT(*) 
  INTO   r_fecha_valida
  FROM   dis_det_avance_pago
  WHERE  f_presentacion = p_fecha
  IF r_fecha_valida = 0 THEN 
     SELECT COUNT(*) 
     INTO   r_fecha_valida
     FROM   dis_rch_avance_pago
     WHERE  f_presentacion = p_fecha
  END IF    

  RETURN r_fecha_valida

END FUNCTION 

#OBJETIVO: Muestra registros de Rechazos
FUNCTION fn_MuestraRechazos(p_folio)
  DEFINE r_arr_VerRechazos   DYNAMIC ARRAY OF RECORD --Arreglo para registros rechazados
    nss                      LIKE afi_derechohabiente.nss,                
    num_credito              LIKE dis_det_avance_pago.num_credito,       
    periodo_pago             LIKE dis_det_avance_pago.periodo_pago,      
    f_pago                   LIKE dis_det_avance_pago.f_pago,
    nrp                      LIKE dis_det_avance_pago.nrp,
    monto_aportacion         LIKE dis_det_avance_pago.monto_aportacion,  
    monto_amortizacion       LIKE dis_det_avance_pago.monto_amortizacion,
    estado                   CHAR(52)
    END RECORD,
    
    p_folio                  LIKE dis_sum_avance_pago.folio,--Parámetro de Folio
    v_QryTxt                 STRING,       --Cadena para almacenar Query 
    v_indice                 DECIMAL(10,0) --Variable de indice

    LET v_QryTxt = "\n SELECT  nss, num_credito, periodo_pago, f_pago,",
                   "\n         nrp, monto_aportacion, monto_amortizacion,",
                   "\n         dr.estado||'-'||ca.desc_edo_avance",
                   "\n FROM    dis_rch_avance_pago dr,",
                   "\n         cat_edo_avance_pago ca",
                   "\n WHERE   dr.folio  = ",p_folio,
                   "\n AND     dr.estado = ca.cod_edo_avance" 

    --Prepara la consulta para el display
    PREPARE prp_DetRechazados FROM v_QryTxt

    --Declara el cursor para la consulta
    DECLARE cur_DetRechazados CURSOR FOR prp_DetRechazados
    LET v_indice = 1
    FOREACH cur_DetRechazados INTO r_arr_VerRechazos[v_indice].*
      --Asigna máscara de núm credito
      LET r_arr_VerRechazos[v_indice].num_credito = r_arr_VerRechazos[v_indice].num_credito USING "&&&&&&&&&&"    
      LET v_indice                                = v_indice + 1
    END FOREACH
      
    CALL r_arr_VerRechazos.deleteElement(v_indice)
   
    --Retorna el arreglo con detalles del folio rechazado y tamaño del arreglo
    RETURN r_arr_VerRechazos, v_indice

END FUNCTION 

#OBJETIVO: Genera el reporte de Rechazos
REPORT reporte_Rechazados(arr_rptRechazos, p_edit_folio, p_usuario, p_fecha_present, p_titulo)
  DEFINE arr_rptRechazos     RECORD --Arreglo para registros rechazados
    nss                      LIKE afi_derechohabiente.nss,
    num_credito              LIKE dis_det_avance_pago.num_credito,
    periodo_pago             LIKE dis_det_avance_pago.periodo_pago,
    f_pago                   DATE, 
    nrp                      LIKE dis_det_avance_pago.nrp,
    monto_aportacion         LIKE dis_det_avance_pago.monto_aportacion,  
    monto_amortizacion       LIKE dis_det_avance_pago.monto_amortizacion,
    estado                   CHAR(53)
    END RECORD

  DEFINE 
    v_fecha_reporte          DATE,
    p_edit_folio             VARCHAR(20),
    p_usuario                LIKE seg_usuario.usuario_cod, 
    p_fecha_present          DATE,
    p_titulo                 STRING
       
FORMAT     
  FIRST PAGE HEADER
    LET v_fecha_reporte = TODAY CLIPPED

    PRINTX v_fecha_reporte USING "dd-mm-yyyy" 
    PRINTX p_edit_folio
    PRINTX p_fecha_present USING "dd-mm-yyyy"
    PRINTX p_usuario
    PRINTX v_nombre_archivo

  PAGE HEADER 
    LET v_fecha_reporte = TODAY CLIPPED

    PRINTX v_fecha_reporte USING "dd-mm-yyyy" 
    PRINTX p_edit_folio
    PRINTX p_fecha_present USING "dd-mm-yyyy"
    PRINTX p_usuario

  ON EVERY ROW  
     PRINTX arr_rptRechazos.nss
     PRINTX arr_rptRechazos.num_credito
     PRINTX arr_rptRechazos.periodo_pago
     PRINTX arr_rptRechazos.f_pago USING "dd-mm-yyyy"
     PRINTX arr_rptRechazos.nrp
     PRINTX arr_rptRechazos.monto_aportacion
     PRINTX arr_rptRechazos.monto_amortizacion
     PRINTX arr_rptRechazos.estado

  ON LAST ROW 
     PRINTX p_usuario
     PRINTX v_fecha_reporte USING "dd-mm-yyyy" 
END REPORT

--Objetivo: Obtiene detalle del reporte para complementarlo 
FUNCTION fn_obtiene_detalle_reporte()
  DEFINE   
    v_query_periodo          STRING, 
    v_periodo                DECIMAL(10,0) --Índice de consulta de periodos

  LET v_total_reg_apo     = 0
  LET v_total_reg_amo     = 0
  LET v_total_reg_apo_amo = 0
  LET v_estado_registro   = 0

  DISPLAY "El folio --", v_edit_Folio
   
  SELECT estado 
  INTO   v_estado_registro
  FROM   dis_sum_avance_pago
  WHERE  folio = v_edit_Folio

  DISPLAY "Estado -- ",v_estado_registro

  IF v_estado_registro = 10 OR 
     v_estado_registro = 30 OR 
     v_estado_registro = 40 OR 
     v_estado_registro = 41 THEN --Si no hubo rechazos entonces consulta de dis_det_avance_pago
   
     SELECT COUNT(*) 
     INTO   v_total_reg_apo
     FROM   dis_det_avance_pago
     WHERE  folio              = v_edit_Folio
     AND    monto_aportacion  <> 0.00
     AND    monto_amortizacion = 0.00 
     IF v_total_reg_apo IS NULL THEN 
        LET v_total_reg_apo = 0 
     END IF 
     DISPLAY "Total reg aportaciones -- ",v_total_reg_apo
            
     SELECT COUNT(*) 
     INTO   v_total_reg_amo
     FROM   dis_det_avance_pago
     WHERE  folio               = v_edit_Folio
     AND    monto_amortizacion <> 0.00
     AND    monto_aportacion    = 0.00
     IF v_total_reg_amo IS NULL THEN 
        LET v_total_reg_amo = 0 
     END IF 
     DISPLAY "Total reg amortizaciones -- ",v_total_reg_amo

     --Total de Registros con Aportaciones y Amortizaciones.
     SELECT COUNT(*) 
     INTO   v_total_reg_apo_amo
     FROM   dis_det_avance_pago
     WHERE  folio               = v_edit_Folio
     AND    monto_amortizacion <> 0.00
     AND    monto_aportacion   <> 0.00
     IF v_total_reg_apo_amo IS NULL THEN 
        LET v_total_reg_apo_amo = 0 
     END IF 
     DISPLAY "Total reg apo y amo -- ",v_total_reg_apo_amo
  ELSE --Si no, entonces consulta de dis_rch_avance_pago          
     SELECT COUNT(*) 
     INTO   v_total_reg_apo
     FROM   dis_rch_avance_pago
     WHERE  folio              = v_edit_Folio
     AND    monto_aportacion  <> 0.00
     AND    monto_amortizacion = 0.00
     IF v_total_reg_apo IS NULL THEN 
        LET v_total_reg_apo = 0 
     END IF 
     DISPLAY "Total reg aportaciones -- ",v_total_reg_apo
            
     SELECT COUNT(*) 
     INTO   v_total_reg_amo
     FROM   dis_rch_avance_pago
     WHERE  folio               = v_edit_Folio
     AND    monto_amortizacion <> 0.00
     AND    monto_aportacion    = 0.00
     IF v_total_reg_amo IS NULL THEN 
        LET v_total_reg_amo = 0 
     END IF 
     DISPLAY "Total reg amortizaciones -- ",v_total_reg_amo

     --Total de Registros con Aportaciones y Amortizaciones.
     SELECT COUNT(*) 
     INTO   v_total_reg_apo_amo
     FROM   dis_rch_avance_pago
     WHERE  folio               = v_edit_Folio
     AND    monto_amortizacion <> 0.00
     AND    monto_aportacion   <> 0.00
     IF v_total_reg_apo_amo IS NULL THEN 
        LET v_total_reg_apo_amo = 0 
     END IF 
     DISPLAY "Total reg apo y amo -- ",v_total_reg_apo_amo
  END IF 

  --Total de Registros.
  --SELECT tot_registros INTO v_total_reg
  --FROM dis_sum_avance_pago
  --WHERE folio = v_edit_Folio
  --DISPLAY "Total registros sumario -- ",v_total_reg

  LET v_total_reg = v_total_reg_apo + v_total_reg_amo + v_total_reg_apo_amo

  --Importe Total de Aportaciones.
  --SELECT tot_aportacion
  SELECT SUM(monto_aportacion) 
  INTO   v_total_suma_apo
  --FROM   dis_sum_avance_pago
  FROM   dis_det_avance_pago
  WHERE  folio = v_edit_Folio
  DISPLAY "Total apo ", v_total_suma_apo

  --Importe Total de Amortizaciones. 
  SELECT SUM(monto_amortizacion)
  --SELECT tot_amortizacion 
  INTO   v_total_suma_amo
  FROM   dis_det_avance_pago
  --FROM   dis_sum_avance_pago
  WHERE  folio = v_edit_Folio
  DISPLAY "Total amo ", v_total_suma_amo

  --Total de Registros con Número de Crédito en ceros.
  SELECT COUNT(*) 
  INTO   v_total_reg_num_cred
  FROM   dis_rch_avance_pago
  WHERE  folio  = v_edit_Folio
  AND    estado = 27
  DISPLAY "Total ceros ", v_total_reg_num_cred

  --Total de Registros Rechazados (Por no encontrarse en el maestro de derechohabientes).
  SELECT COUNT(*) 
  INTO   v_total_reg_rch
  FROM   dis_rch_avance_pago
  WHERE  folio  = v_edit_Folio
  AND    estado = 20
  DISPLAY "Total no maestro ", v_total_reg_rch

   --Total de Registros Rechazados (Otros motivos).
   SELECT COUNT(*) 
   INTO   v_total_reg_rch_om
   FROM   dis_rch_avance_pago
   WHERE  folio   = v_edit_Folio
   AND    estado <> 10 
   AND    estado <> 20
   DISPLAY "Total otros ", v_total_reg_rch_om

   --Total de Registros Sin Integrar (Pago Previo).
   {SELECT COUNT(*) 
   INTO   v_total_pago_previo
   FROM   dis_ava_pago_previo
   WHERE  folio   = v_edit_Folio
   DISPLAY "Total Sin Integrar (Pago Previo) ", v_total_pago_previo}

   LET v_periodo = 1
   
   --Obtiene total de registros por periodo
   LET v_query_periodo = "\n SELECT periodo_pago, COUNT(*)",
                         "\n FROM   dis_det_avance_pago ",
                         "\n WHERE  folio = ",v_edit_Folio,
                         "\n GROUP BY 1",
                         "\n UNION ALL",
                         "\n SELECT periodo_pago, COUNT(*)",
                         "\n FROM   dis_rch_avance_pago",
                         "\n WHERE  folio = ",v_edit_Folio,
                         "\n GROUP BY 1"

   DISPLAY "Consulta ", v_query_periodo
                           
   PREPARE prp_periodos_pago FROM v_query_periodo
   DECLARE cur_periodo_pago CURSOR FOR prp_periodos_pago
   FOREACH cur_periodo_pago INTO arr_total_reg_periodo[v_periodo].v_perido,
                                 arr_total_reg_periodo[v_periodo].v_total_reg_p  
     LET v_periodo = v_periodo + 1
   END FOREACH 

   CALL arr_total_reg_periodo.deleteElement(v_periodo)

   --Despliega información en el log
   DISPLAY "\n ############### INTEGRACIÓN REGISTRO AVANCES DE PAGO ###############"
   DISPLAY "Total registros con aportaciones                 : ", v_total_reg_apo CLIPPED 
   DISPLAY "Total registros con amortizaciones               : ", v_total_reg_amo CLIPPED
   DISPLAY "Total registros con aportaciones y amortizaciones: ", v_total_reg_apo_amo CLIPPED
   DISPLAY "Total aportaciones                               : ", v_total_suma_apo CLIPPED
   DISPLAY "Total amortizaciones                             : ", v_total_suma_amo CLIPPED
--   DISPLAY "Total registros sin integrar (Pago previo)       : ", v_total_pago_previo CLIPPED     
   DISPLAY "Total registros                                  : ", v_total_reg
   
END FUNCTION 

#Objetivo: Recupera el nombre del archivo cargado e integrado de avance de pagos
FUNCTION fn_obtiene_nombre_archivo(v_folio_recupera)
  DEFINE  
    v_folio_recupera         LIKE glo_ctr_archivo.folio
   
  SELECT nombre_archivo
  INTO   v_nombre_archivo
  FROM   glo_ctr_archivo
  WHERE  folio  = v_folio_recupera
  AND    estado = 2
   
  DISPLAY "Archivo: ",v_nombre_archivo

END FUNCTION 

#Objetivo: Genera reporte de avances de pago
REPORT rp_avance_pago(arr_ava_pago, arr_total_reg_periodo, p_edit_folio, p_usuario, p_fecha_present)
  DEFINE
    arr_ava_pago             RECORD --Arreglo que recibe los valores del avance de pagos
      folio                  LIKE dis_sum_avance_pago.folio,
      f_presentacion         DATE,
      tot_registros          DECIMAL(10,0),
      tot_aportacion         LIKE dis_sum_avance_pago.tot_aportacion,
      tot_amortizacion       LIKE dis_sum_avance_pago.tot_amortizacion,
      tot_rechazados         DECIMAL(10,0),
      estado                 CHAR(52)
    END RECORD,

    arr_total_reg_periodo    RECORD 
      v_perido               LIKE dis_rch_avance_pago.periodo_pago,
      v_total_reg_p          DECIMAL(10,0)
    END RECORD,
       
    v_fecha_reporte          DATE,
    p_edit_folio             VARCHAR(20),
    p_usuario                LIKE seg_usuario.usuario_cod, 
    p_fecha_present           DATE

  FORMAT
    FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY CLIPPED
      LET p_fecha_present = arr_ava_pago.f_presentacion

      PRINTX v_fecha_reporte USING "dd-mm-yyyy" 
      PRINTX p_edit_folio
      PRINTX p_usuario
      PRINTX p_fecha_present USING "dd-mm-yyyy"
      PRINTX v_nombre_archivo

      --Información general
      PRINTX arr_ava_pago.folio
      PRINTX arr_ava_pago.f_presentacion USING "dd-mm-yyyy" 
      PRINTX arr_ava_pago.tot_registros
      PRINTX arr_ava_pago.tot_aportacion
      PRINTX arr_ava_pago.tot_amortizacion
      PRINTX arr_ava_pago.tot_rechazados
      PRINTX arr_ava_pago.estado
      
      --Información de detalle
      PRINTX v_total_reg_apo 
      PRINTX v_total_reg_amo 
      PRINTX v_total_reg_apo_amo 
      PRINTX v_total_reg 
      PRINTX v_total_suma_apo 
      PRINTX v_total_suma_amo 
      PRINTX v_total_reg_num_cred 
      PRINTX v_total_reg_rch 
      PRINTX v_total_reg_rch_om
--      PRINTX v_total_pago_previo
      
    ON EVERY ROW
       PRINTX arr_total_reg_periodo.*

    ON LAST ROW 
       PRINTX p_usuario
       PRINTX v_fecha_reporte USING "dd-mm-yyyy" 

END REPORT

#OBJETIVO: Genera el reporte de Rechazos
REPORT reporte_Rechazados1(arr_rptRechazos, p_edit_folio, p_usuario, p_fecha_present, p_titulo)
  DEFINE arr_rptRechazos     RECORD --Arreglo para registros rechazados
    nss                      LIKE afi_derechohabiente.nss,
    num_credito              LIKE dis_det_avance_pago.num_credito,
    periodo_pago             LIKE dis_det_avance_pago.periodo_pago,
    f_pago                   DATE,
    nrp                      LIKE dis_det_avance_pago.nrp,
    monto_aportacion         LIKE dis_det_avance_pago.monto_aportacion,  
    monto_amortizacion       LIKE dis_det_avance_pago.monto_amortizacion,
    estado                   CHAR(53)
    END RECORD

  DEFINE 
    v_fecha_reporte          DATE,
    p_edit_folio             VARCHAR(20),
    p_usuario                LIKE seg_usuario.usuario_cod, 
    p_fecha_present          DATE,
    p_titulo                 STRING

  FORMAT
    FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY CLIPPED

      PRINTX v_fecha_reporte USING "dd-mm-yyyy" 
      PRINTX p_edit_folio
      PRINTX p_fecha_present USING "dd-mm-yyyy"
      PRINTX p_usuario
      PRINTX v_nombre_archivo

    PAGE HEADER 
      LET v_fecha_reporte = TODAY CLIPPED

      PRINTX v_fecha_reporte USING "dd-mm-yyyy" 
      PRINTX p_edit_folio
      PRINTX p_fecha_present USING "dd-mm-yyyy"
      PRINTX p_usuario
      PRINTX v_nombre_archivo

    ON EVERY ROW
       PRINTX arr_rptRechazos.nss
       PRINTX arr_rptRechazos.num_credito
       PRINTX arr_rptRechazos.periodo_pago
       PRINTX arr_rptRechazos.f_pago USING "dd-mm-yyyy"
       PRINTX arr_rptRechazos.nrp
       PRINTX arr_rptRechazos.monto_aportacion
       PRINTX arr_rptRechazos.monto_amortizacion
       PRINTX arr_rptRechazos.estado

    ON LAST ROW 
       PRINTX p_usuario
       PRINTX v_fecha_reporte USING "dd-mm-yyyy" 

END REPORT