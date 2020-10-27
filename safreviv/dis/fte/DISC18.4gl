################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 20/03/2018                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISC18                                                   #
#Objetivo          => Programa para consultar o identificar los registros con  #
#                     inconsistencias por numero de crédito igual a cero.      #
#Fecha inicio      => 08/01/2015                                               #
################################################################################
DATABASE safre_viv

GLOBALS
   DEFINE g_sql_txt          STRING,      --Consultas
          g_usuario          VARCHAR(30), --Almacena al usuario
          g_tipo_proceso     SMALLINT,    --Forma como ejecutara el programa
          g_nom_prog         VARCHAR(30), --Almacena opción del menú
          p_proceso_cod      SMALLINT,    --codigo del proceso
          v_proceso_cod      SMALLINT,    --codigo del proceso
          p_opera_cod        SMALLINT,    --codigo de operacion
          p_pid              DECIMAL(9,0)

   DEFINE v_tot_registros    DECIMAL(9,0), -- Total de registros
          v_sum_aivs         DECIMAL(18,6),
          v_sum_aportacion   DECIMAL(12,2), 
          v_sum_amortizacion DECIMAL(12,2)

   DEFINE v_arr_inconsistentes DYNAMIC ARRAY OF RECORD
          nss                CHAR(11),
          num_credito		 DECIMAL(10),
          periodo_pago	     CHAR(6),
          f_pago	         DATE,
          nrp	             CHAR(11),
          aportacion	     DECIMAL(12,2),
          amortizacion	   	 DECIMAL(12,2),
          folio_sua	     	 DECIMAL(6,0),
          inconsistencia     CHAR(30),
          estado             CHAR(30),
          id_derechohabiente DECIMAL(9,0)
  END RECORD

  DEFINE v_arr_cifras_reporte DYNAMIC ARRAY OF RECORD
         v_estado            SMALLINT,
         v_desc_estado       VARCHAR(50),
         v_tot_estado        INTEGER
  END RECORD

  DEFINE v_edo_credito_desc  VARCHAR(40),
         v_desc_tipo_trabajador VARCHAR(40),
         v_desc_estado       VARCHAR(50)

  DEFINE v_nombre_completo   VARCHAR(50), --Nombre del derechohabiente
         v_periodo_cvt1      VARCHAR(6),
         v_periodo_cvt2      VARCHAR(6)

  DEFINE v_mensaje           STRING

  DEFINE     
    v_proc_entra             SMALLINT,
    v_proc_val               SMALLINT,
    v_cod_conv               SMALLINT,
    v_desc_proc_val          CHAR(40),
    v_mensaje_val            STRING

   
END GLOBALS

MAIN
  DEFINE f_folio             DECIMAL(9,0),  --Folio de la liquidación de la dispersión
         f_nss               CHAR(11),      --NSS trabajador
         r_registros         DECIMAL(9,0),  --Bandera de consulta avance de pago
         r_sum_aivs          DECIMAL(18,6),
         r_sum_aportacion    DECIMAL(12,2),
         r_sum_amortizacion  DECIMAL(12,2) 
  
  DEFINE f_ventana           ui.Window, --Define las propìedades de la Ventana
         f_forma             ui.Form ,  --Define las propiedades de la forma
         v_ruta_listados     CHAR(40),
         --v_cuenta_derechohabiente SMALLINT,
         v_cuenta_folio      INTEGER,
         bnd_consulta        SMALLINT,
         r_bnd_periodo       SMALLINT,
         v_qwery_ibx         STRING 

  DEFINE l_comando           STRING
  DEFINE v_ruta_ejecutable   CHAR(40)
  DEFINE v_max_pid           LIKE bat_ctr_proceso.pid
  DEFINE v_folio             LIKE dis_det_avance_pago.folio
  DEFINE l_v_arch_proceso    VARCHAR(100)
   
  DEFINE v_folio_disp        DECIMAL(9,0)
  DEFINE p_programa          CHAR(10),  
         r_bandera           SMALLINT,
         r_nom_archivo       CHAR(40)

  DEFINE v_bnd_liquidados    SMALLINT

  --Recibe valores de argumentos
  LET g_usuario      = ARG_VAL(1)
  LET g_tipo_proceso = ARG_VAL(2)
  LET g_nom_prog     = ARG_VAL(3)
  LET p_proceso_cod  = 920 --903
  LET p_opera_cod    = 1 
  LET r_bnd_periodo  = 0

  CALL STARTLOG (g_usuario CLIPPED||".DISC18.log")

  INITIALIZE l_v_arch_proceso TO NULL

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

  CLOSE WINDOW SCREEN
   
  OPEN WINDOW vtn_inconsistente WITH FORM "DISC181"
    DIALOG ATTRIBUTES(UNBUFFERED) 
      INPUT BY NAME f_folio
        BEFORE INPUT
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma   = f_ventana.getForm()

          CALL f_forma.setElementHidden("gr_nombre", TRUE ) --Oculta el nombre del trabajador
          CALL f_forma.setElementHidden("gr_detalle", TRUE ) --Oculta detalle de la consulta

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

          NEXT FIELD f_folio
          CALL ui.interface.refresh()
          
          ON ACTION ACCEPT 
             --Valida que se inserte al menos un parámetro
             IF (f_folio IS NULL) AND 
                (f_nss   IS NULL) THEN
                CALL fn_mensaje("ATENCIÓN",
                                "Debe capturar un criterio para la búsqueda",
                                "about")
               NEXT FIELD f_folio   
             END IF  

             IF f_folio IS NOT NULL THEN
                --Valida que exista el folio
                SELECT COUNT(*) 
                INTO   v_cuenta_folio
                FROM   dis_arh_num_cred_0
                WHERE  folio = f_folio
                  
                IF v_cuenta_folio  = 0    OR 
                   v_cuenta_folio IS NULL THEN 
                   CALL fn_mensaje("Atención","No existen registros para el folio capturado",
                                   "about")
                    NEXT FIELD f_folio
                 END IF 
             END IF
               
             DISPLAY "f_folio -- ",f_folio

             LET v_bnd_liquidados = 0
             CALL fn_consulta_inconsistencias(f_folio)
             RETURNING r_registros,
                       r_sum_aivs,
                       r_sum_aportacion,
                       r_sum_amortizacion,
                       v_bnd_liquidados
               
              DISPLAY "r_registros -- ",r_registros
      
              IF r_registros > 0 THEN
                 CALL f_forma.setElementHidden("gr_detalle", FALSE ) --muestra detalle de la consulta
              
                 DISPLAY ARRAY v_arr_inconsistentes TO rcd_detalle.* 
                 --DISPLAY ARRAY v_arr_incon_pantalla TO rcd_detalle.* 
                 ATTRIBUTES (ACCEPT=FALSE, CANCEL=FALSE)

                   BEFORE ROW
                     --CALL fn_obtiene_nombre_derechohabiente(v_arr_inconsistentes[ARR_CURR()].id_derechohabiente) 
               
                     BEFORE DISPLAY 
                       DISPLAY r_registros        TO f_total_registros
                       --DISPLAY r_sum_aivs         TO f_suma_aivs
                       DISPLAY r_sum_aportacion   TO f_suma_aportaciones
                       DISPLAY r_sum_amortizacion TO f_suma_amortizaciones

                       DISPLAY "v_bnd_liquidados: ", v_bnd_liquidados
                       IF v_bnd_liquidados = 1 THEN
                          CALL DIALOG.setActionHidden("consulta_liquidacion", 1)
                          CALL DIALOG.setActionHidden("reporte",1)
                          CALL fn_mensaje("DISPERSION", "Existen pagos liquidados.", "information")
                       ELSE
                          CALL DIALOG.setActionHidden("consulta_liquidacion", 0)
                          CALL DIALOG.setActionHidden("reporte",0)
                       END IF

                     AFTER DISPLAY 
                       CALL ui.interface.refresh()
                       --CALL DIALOG.setActionHidden("reporte",0)
                       --CALL DIALOG.setActionHidden("consulta_liquidacion", 0)
                       CALL ui.interface.refresh()

                     ON ACTION cancelar 
                        EXIT PROGRAM 
                    
                     ON ACTION reporte
                        CALL fn_reporte_inconsistencias(f_folio)
                   
                     --Genera archivo
                     ON ACTION archivo
                        CALL fn_genera_archivo_inconsistencias(f_folio)   

                     ON ACTION consulta_liquidacion
                        IF fn_ventana_confirma("ATENCIÓN",
                                                "¿Esta seguro que desea ejecutar la liquidación?",
                                                "question") THEN

                           --Crear folio
                           --pid
                           --g_proceso_cod = 901
                           --g_opera_cod = 1
                           --l_v_arch_proceso

                           --validación que NO se tenga Preliquidación de Dispersión de Pagos ejecutándose
                           {IF f_existe_proceso_operacion_ejecutando(901, 1) THEN
                              MENU "No se puede ejecutar" ATTRIBUTES ( STYLE="dialog", 
                                 COMMENT="Preliquidación de Dispersión de Pagos ejecutándose,\ningrese a esta opción cuando finalice",
                                 IMAGE="information" )

                                 ON ACTION salir
                                    RETURN
                              END MENU
                           END IF}
                     
                           LET v_proceso_cod = 901

                           --Si se acepta la ejecución se genera PID del proceso
                           CALL fn_genera_pid (v_proceso_cod, 1, g_usuario) RETURNING p_pid


                           IF (fn_valida_operacion(p_pid, v_proceso_cod, 1) = 0 ) THEN

                              --Enlaza el folio referencia 
                              LET v_folio_disp = 0
                              CALL fn_genera_folio_dis(v_proceso_cod, 1, 0, g_usuario)
                                             RETURNING v_folio_disp

                              LET p_programa = "DISP03"
                              LET r_nom_archivo = ""

                              DISPLAY "Folio Disp -- ",v_folio_disp
                              --Ejecuta la funcion de preliquida y asigna estatus de LISTO
                              CALL fn_inicializa_proceso (p_pid, v_proceso_cod, 1, v_folio_disp, 
                                                          p_programa, r_nom_archivo, g_usuario)
                              RETURNING r_bandera

                              --Inicia la operación asignando el estatus de PROCESANDO
                              CALL fn_actualiza_opera_ini(p_pid, v_proceso_cod, 1, v_folio_disp, 
                                                          p_programa, r_nom_archivo, g_usuario)
                              RETURNING r_bandera

                              --DISPLAY "r_bandera: ", r_bandera
                              --DISPLAY "ANTES DE MANDAR nohup DISP03"

                                       
                              --Hace el llamado a la función que realiza la preliquidación.
                              LET l_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/DISP03.42r ",
                                             v_folio_disp," ",
                                             0," ",
                                             0," ",
                                             TODAY," ",
                                             g_usuario, " ",
                                             f_folio,
                                             " 1>",v_ruta_listados CLIPPED,"/nohup:",
                                             p_pid USING "&&&&&",":",
                                             v_proceso_cod USING "&&&&&",":",
                                             1 USING "&&&&&" ," 2>&1 &"
                              RUN l_comando
                     
                              LET v_mensaje = "Se ha enviado la preliquidacíon Créditos Ceros con PID: ",
                                                p_pid CLIPPED,
                                                ".\nPuede revisar el avance del proceso en el monitor de ejecución de procesos"

                              CALL fn_mensaje("DISPERSION", v_mensaje, "information")
                                          
                              --EXIT DISPLAY
                              EXIT DIALOG
                           ELSE
                             CALL fn_muestra_inc_operacion(fn_valida_operacion(p_pid, v_proceso_cod, 1))
                           END IF  --De valida operacion

                        ELSE 
                          --Si el usuario Cancela
                          CALL fn_mensaje("DISPERSION", "Se ha cancelado la preliquidación", "information")                     
                          CLEAR FORM
                          EXIT DIALOG                     
                        END IF 
                 END DISPLAY     
              ELSE
                 CALL fn_mensaje("ATENCIÓN",
                                 "No se encontraron registros",
                                 "about")
                 NEXT FIELD f_folio
                 CALL ui.interface.refresh()
              END IF
        END INPUT
      
        ON ACTION cancelar
           EXIT DIALOG
 
    END DIALOG 
  CLOSE WINDOW vtn_inconsistente 
END MAIN

#Objetivo: Consulta para verificar si existe información con los parametros 
#          capturados
FUNCTION fn_consulta_inconsistencias(f_folio)
  DEFINE f_folio             DECIMAL(9,0), --Folio de liquidación dispersión
         v_indice            INTEGER

  DEFINE  j                  INTEGER
  DEFINE v_bnd_liquidados    SMALLINT

  LET g_sql_txt = "\n SELECT cero.nss, ",
                  "\n        cero.num_credito, ",
                  "\n        cero.periodo_pago, ",
                  "\n        cero.f_pago, ",
                  "\n        cero.nrp, ",
                  "\n        NVL(cero.aportacion,0), ",
                  "\n        NVL(cero.amortizacion,0), ",
                  "\n        cero.folio_sua, ",
                  "\n        cero.inconsistencia, ",
                  "\n        cero.estado, ",
                  "\n        cero.id_derechohabiente ",
                  "\n FROM   dis_arh_num_cred_0 cero ",
                  "\n WHERE  cero.folio = ", f_folio

  LET g_sql_txt = g_sql_txt||"\n ORDER BY cero.estado, cero.periodo_pago"

  DISPLAY "g_sql_txt -- ",g_sql_txt
  PREPARE prp_sql_inconsistencia FROM g_sql_txt
  
  LET v_indice           = 1
  LET v_tot_registros    = 1
  LET v_sum_aivs         = 0.00
  LET v_sum_aportacion   = 0.00
  LET v_sum_amortizacion = 0.00
  LET v_bnd_liquidados   = 0

  LET j = 1
  CALL v_arr_cifras_reporte.clear()
  LET v_arr_cifras_reporte[j].v_tot_estado = 0

  --Iteración de registros con base en la consulta temporal
  DECLARE cur_inconsistencia CURSOR FOR prp_sql_inconsistencia
  FOREACH cur_inconsistencia INTO v_arr_inconsistentes[v_indice].nss,
                                  v_arr_inconsistentes[v_indice].num_credito,
                                  v_arr_inconsistentes[v_indice].periodo_pago,
                                  v_arr_inconsistentes[v_indice].f_pago,
                                  v_arr_inconsistentes[v_indice].nrp,
                                  v_arr_inconsistentes[v_indice].aportacion,
                                  v_arr_inconsistentes[v_indice].amortizacion,
                                  v_arr_inconsistentes[v_indice].folio_sua,
                                  v_arr_inconsistentes[v_indice].inconsistencia,
                                  v_arr_inconsistentes[v_indice].estado,
                                  v_arr_inconsistentes[v_indice].id_derechohabiente
                                                                       
    IF v_indice = 1 THEN
       LET v_arr_cifras_reporte[j].v_estado =  v_arr_inconsistentes[v_indice].estado
       LET v_arr_cifras_reporte[j].v_tot_estado = v_arr_cifras_reporte[j].v_tot_estado + 1

       {IF v_arr_cifras_reporte[j].v_estado = 0 THEN
          LET v_arr_cifras_reporte[j].v_desc_estado = "PENDIENTE POR LIQUIDAR"
       ELSE
          IF v_arr_cifras_reporte[j].v_estado = 1 THEN
             LET v_arr_cifras_reporte[j].v_desc_estado = "RECHAZADO"
          ELSE 
             IF v_arr_cifras_reporte[j].v_estado = 2 THEN
                LET v_arr_cifras_reporte[j].v_desc_estado = "LIQUIDADO"
                LET v_bnd_liquidados   = 1
             END IF
          END IF
       END IF}

       CASE v_arr_cifras_reporte[j].v_estado
         WHEN 0
           LET v_arr_cifras_reporte[j].v_desc_estado = "PENDIENTE POR LIQUIDAR"
         WHEN 1
           LET v_arr_cifras_reporte[j].v_desc_estado = "RECHAZADO"
         WHEN 2
           LET v_arr_cifras_reporte[j].v_desc_estado = "LIQUIDADO"
           LET v_bnd_liquidados                      = 1
         WHEN 3
           LET v_arr_cifras_reporte[j].v_desc_estado = "LIQUIDA AMORTIZACIÓN"
           LET v_bnd_liquidados                      = 1
         WHEN 4
           LET v_arr_cifras_reporte[j].v_desc_estado = "LIQUIDA APORTACIÓN"
           LET v_bnd_liquidados                      = 1
         OTHERWISE
       END CASE
    ELSE
       IF v_arr_inconsistentes[v_indice - 1].estado <> v_arr_inconsistentes[v_indice].estado THEN
          LET j = j + 1
          LET v_arr_cifras_reporte[j].v_tot_estado = 0
          LET v_arr_cifras_reporte[j].v_estado     =  v_arr_inconsistentes[v_indice].estado
          LET v_arr_cifras_reporte[j].v_tot_estado = v_arr_cifras_reporte[j].v_tot_estado + 1
            
          {IF v_arr_cifras_reporte[j].v_estado = 0 THEN
             LET v_arr_cifras_reporte[j].v_desc_estado = "PENDIENTE POR LIQUIDAR"
          ELSE
             IF v_arr_cifras_reporte[j].v_estado = 1 THEN
                LET v_arr_cifras_reporte[j].v_desc_estado = "RECHAZADO"
             ELSE
                IF v_arr_cifras_reporte[j].v_estado = 2 THEN
                  LET v_arr_cifras_reporte[j].v_desc_estado = "LIQUIDADO"
                  LET v_bnd_liquidados   = 1
               END IF
             END IF
          END IF}

          CASE v_arr_cifras_reporte[j].v_estado
            WHEN 0
              LET v_arr_cifras_reporte[j].v_desc_estado = "PENDIENTE POR LIQUIDAR"
            WHEN 1
              LET v_arr_cifras_reporte[j].v_desc_estado = "RECHAZADO"
            WHEN 2
              LET v_arr_cifras_reporte[j].v_desc_estado = "LIQUIDADO"
              LET v_bnd_liquidados                      = 1
            WHEN 3
              LET v_arr_cifras_reporte[j].v_desc_estado = "LIQUIDA AMORTIZACIÓN"
              LET v_bnd_liquidados                      = 1
            WHEN 4
              LET v_arr_cifras_reporte[j].v_desc_estado = "LIQUIDA APORTACIÓN"
              LET v_bnd_liquidados                      = 1
            OTHERWISE
          END CASE
       ELSE
          LET v_arr_cifras_reporte[j].v_tot_estado = v_arr_cifras_reporte[j].v_tot_estado + 1
       END IF
    END IF

    LET v_tot_registros    = v_tot_registros    + 1
    --LET v_sum_aivs         = v_sum_aivs         + v_arr_inconsistentes[v_indice].v_aivs
    LET v_sum_aportacion   = v_sum_aportacion   + v_arr_inconsistentes[v_indice].aportacion
    LET v_sum_amortizacion = v_sum_amortizacion + v_arr_inconsistentes[v_indice].amortizacion
    LET v_indice           = v_indice           + 1
  END FOREACH

  IF v_arr_cifras_reporte[v_arr_cifras_reporte.getLength()].v_estado IS NULL THEN
     CALL v_arr_cifras_reporte.deleteElement(v_arr_cifras_reporte.getLength())
  END IF

  CALL v_arr_inconsistentes.deleteElement(v_arr_inconsistentes.getLength())
  LET v_tot_registros    = v_tot_registros - 1

  FOR j = 1 TO v_arr_inconsistentes.getLength()
      CASE v_arr_inconsistentes[j].estado
        WHEN 0
          LET v_arr_inconsistentes[j].estado = v_arr_inconsistentes[j].estado CLIPPED||" - PENDIENTE POR LIQUIDAR"
        WHEN 1
          LET v_arr_inconsistentes[j].estado = v_arr_inconsistentes[j].estado CLIPPED||" - RECHAZADO"
        WHEN 2
          LET v_arr_inconsistentes[j].estado = v_arr_inconsistentes[j].estado CLIPPED||" - LIQUIDADO"
          LET v_bnd_liquidados               = 1     
        WHEN 3
          LET v_arr_inconsistentes[j].estado = v_arr_inconsistentes[j].estado CLIPPED||" - LIQUIDA AMORTIZACIÓN"
          LET v_bnd_liquidados               = 1
        WHEN 4
          LET v_arr_inconsistentes[j].estado = v_arr_inconsistentes[j].estado CLIPPED||" - LIQUIDA APORTACIÓN"
          LET v_bnd_liquidados               = 1
        OTHERWISE
          --ESTADO INCORRECTO
      END CASE
  END FOR

  RETURN v_tot_registros,
         v_sum_aivs,
         v_sum_aportacion,
         v_sum_amortizacion,
         v_bnd_liquidados

END FUNCTION

#Objetivo: Genera reporte de inconsistencias de numero de crédito igual a cero
FUNCTION fn_reporte_inconsistencias(v_rfolio)
  DEFINE v_rfolio            DECIMAL(9,0), --Folio de liquidación de dispersión
         manejador_rpt       om.SaxDocumentHandler, --Contenedor documentos reporte
         v_rep_indice        INTEGER
  
  LET v_rep_indice = 1
  
  -- Botón para generar el reporte en PDF de la consulta
  IF fgl_report_loadCurrentSettings("DISC181.4rp") THEN 
     CALL fgl_report_selectDevice ("PDF")
     LET manejador_rpt = fgl_report_commitCurrentSettings()
  END IF

  --Inicia el reporte de registros con rechazo
  START REPORT rep_cred_cero TO XML HANDLER manejador_rpt
    FOR v_rep_indice = 1 TO v_arr_cifras_reporte.getLength()
        OUTPUT TO REPORT rep_cred_cero(v_arr_cifras_reporte[v_rep_indice].*,
                                       v_tot_registros,
                                       v_sum_aivs,
                                       v_sum_aportacion,
                                       v_sum_amortizacion,
                                       g_usuario,
                                       v_rfolio)
    END FOR
  FINISH REPORT rep_cred_cero
END FUNCTION

#Objetivo: Obtiene nombre del derechohabiente
FUNCTION fn_obtiene_nombre_derechohabiente(v_id_consulta)
  DEFINE v_id_consulta       DECIMAL(9,0)

  SELECT   rtrim(nombre_af) ||" "|| rtrim(ap_paterno_af) ||" "|| rtrim(ap_materno_af)
  INTO     v_nombre_completo
  FROM     afi_derechohabiente
  WHERE    id_derechohabiente = v_id_consulta

  DISPLAY "Nombre: ",v_nombre_completo
  DISPLAY v_nombre_completo TO v_nombre
   
END FUNCTION 

#Objetivo: genera el archivo con la consulta obtenida
FUNCTION fn_genera_archivo_inconsistencias(v_folio)
  DEFINE v_nom_archivo       VARCHAR(40), -- nombre del archivo de salida
         v_ruta_envio_dis    CHAR(40),
         v_ruta_nomarch      VARCHAR(100), -- ruta y nombre del archivo de salida
         v_ch_arch_salida    BASE.CHANNEL,
         v_recorre_arreglo   INTEGER,
         v_comando_dos       STRING,
         v_encabezado        STRING,
         v_detalle           STRING,
         v_sumario           STRING,
         v_folio             DECIMAL(9,0), --Folio liquidación dispersión
         v_nss               CHAR(11) --NSS trabajador

  DEFINE v_fecha_archivo     DATE,  
          v_hora_archivo     DATETIME HOUR TO HOUR ,
          v_min_archivo      DATETIME MINUTE TO MINUTE,
          v_sec_archivo      DATETIME SECOND TO SECOND,
          v_hora             STRING

  LET v_fecha_archivo = TODAY 
  LET v_hora_archivo  = CURRENT HOUR TO HOUR
  LET v_min_archivo   = CURRENT MINUTE TO MINUTE
  LET v_sec_archivo   = CURRENT SECOND TO SECOND
  
  LET v_hora          = v_fecha_archivo USING "ddmmyyyy", "_",v_hora_archivo, v_min_archivo, v_sec_archivo,".dis"
  LET v_nom_archivo   = "/consulta_info_incon", v_hora

  -- se obtienen la ruta envio del módulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = "dis"

  LET v_ruta_nomarch = v_ruta_envio_dis CLIPPED || v_nom_archivo CLIPPED 

  -- se crea el manejador de archivo y se indica que se escribirá en el mismo
  LET v_ch_arch_salida = base.Channel.create()
  CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
  CALL v_ch_arch_salida.setDelimiter("")

  --Imprime encabezado del archivo
  LET v_encabezado = " FECHA DE CORTE ",TODAY
  CALL v_ch_arch_salida.write([v_encabezado])

  --Si se solicitó el folio de dispersión se incluye en el encabezado
  IF v_folio IS NOT NULL THEN
     LET v_encabezado = " FOLIO DE CONSULTA ",v_folio
     CALL v_ch_arch_salida.write([v_encabezado])
  END IF
    
  --Si se solicitó número de seguridad social se incluye en el encabezado
  IF v_nss IS NOT NULL THEN
     LET v_encabezado = " NSS DE CONSULTA ",v_nss
     CALL v_ch_arch_salida.write([v_encabezado])
  END IF

  LET v_encabezado = " NSS |NÚMERO DE CREDITO |PERIODO PAGO |FECHA PAGO |NRP |MONTO APORTACIÓN |MONTO AMORTIZACIÓN |FOLIO SUA |INCONSISTENCIAS |ESTADO DE CONSULTA|"
  CALL v_ch_arch_salida.write([v_encabezado])
   
  FOR v_recorre_arreglo = 1 TO v_arr_inconsistentes.getLength()
      LET v_detalle = v_arr_inconsistentes[v_recorre_arreglo].nss ,"|",
                      v_arr_inconsistentes[v_recorre_arreglo].num_credito USING "&&&&&&&&&&","|",
                      v_arr_inconsistentes[v_recorre_arreglo].periodo_pago USING "&&&&&&","|",
                      v_arr_inconsistentes[v_recorre_arreglo].f_pago USING "dd-mm-yyyy","|",
                      v_arr_inconsistentes[v_recorre_arreglo].nrp,"|",
                      v_arr_inconsistentes[v_recorre_arreglo].aportacion USING "-##,###,###,##&.&&","|",
                      v_arr_inconsistentes[v_recorre_arreglo].amortizacion  USING "-##,###,###,##&.&&","|",
                      v_arr_inconsistentes[v_recorre_arreglo].folio_sua USING "&&&&&&","|",
                      v_arr_inconsistentes[v_recorre_arreglo].inconsistencia  USING "&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&","|",
                      v_arr_inconsistentes[v_recorre_arreglo].estado --,
                      --v_arr_inconsistentes[v_recorre_arreglo].id_derechohabiente USING "&&&&&&&&&","|"
                                  
      CALL v_ch_arch_salida.write([v_detalle])
  END FOR
   
  --Escribe el sumario
  LET v_sumario = "TOTALES|",v_tot_registros,"| | | | ",
                  --v_sum_aivs, "|",
                  v_sum_aportacion, "|",
                  v_sum_amortizacion
  CALL v_ch_arch_salida.write([v_sumario])
  --Cierra el archivo
   
  CALL v_ch_arch_salida.close()
  LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos

  CALL fn_mensaje("Información","Se ha generado el archivo de Consulta Números de Créditos igual a Cero \n en la ruta "||v_ruta_nomarch,"information")
   
END FUNCTION

#Objetivo: Estructura reporte de Numeros de Crédito igual a Cero
REPORT rep_cred_cero(v_rep_inconsist, 
                     v_rep_tot_registros, 
                     v_rep_sum_aivs, 
                     v_rep_sum_aportacion,
                     v_rep_sum_amortizacion, 
                     v_usuario,
                     v_rep_folio)

  DEFINE v_rep_inconsist     RECORD
         v_estado            SMALLINT,
         v_desc_estado       VARCHAR(50),
         v_tot_estado        INTEGER
  END RECORD

  DEFINE v_fecha_consulta    DATE, -- Fecha de proceso
         v_usuario           VARCHAR(30), -- Almacena al usuario
         v_rep_tot_registros DECIMAL(9,0), -- Total de registros
         v_rep_sum_aivs      DECIMAL(18,6),
         v_rep_sum_aportacion DECIMAL(12,2),
         v_rep_sum_amortizacion DECIMAL(12,2),
         v_rep_folio         DECIMAL(9,0)

  FORMAT

  PAGE HEADER
    LET v_fecha_consulta = TODAY
    PRINTX v_usuario
    PRINTX v_fecha_consulta USING "dd-mm-yyyy"
    PRINTX v_rep_folio
      
  FIRST PAGE HEADER
    --Inicializa la fecha de consulta  
    LET v_fecha_consulta = TODAY
    PRINTX v_usuario
    PRINTX v_fecha_consulta USING "dd-mm-yyyy"
    PRINTX v_rep_folio

    ON EVERY ROW
       PRINTX v_rep_inconsist.v_estado
       PRINTX v_rep_inconsist.v_desc_estado
       PRINTX v_rep_inconsist.v_tot_estado

    ON LAST ROW
       PRINTX v_rep_tot_registros
       -- PRINTX v_rep_sum_aivs
       PRINTX v_rep_sum_aportacion
       PRINTX v_rep_sum_amortizacion

END REPORT