################################################################################
#Versión                    => 1.0.0                                           #
#Fecha última modificacion  => 21/03/2018                                      #
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo           => DIS                                                       #
#Programa         => DISC12                                                    #
#Objetivo         => Realizar la consulta avances cerrados y generar archivo   #
#Fecha de Inicio  => 12/06/2013                                                #
################################################################################
#Registro de modificaciones:
#Autor           Fecha         Descrip. cambio
#Eneas Armas     18/12/2013    Se agrega validación, que no se tenga la
#                              Preliquidación de Dispersión de Pagos ejecutándose

DATABASE safre_viv
GLOBALS
  --Sección de variables UI
  DEFINE 
    f_ventana                ui.Window, --provee la interfaz para la ventana
    f_forma                  ui.Form --provee la interfaz para la forma

  --Sección de variables del programa
  DEFINE 
    p_usuario                LIKE seg_usuario.usuario_cod,
    p_nom_prog               VARCHAR(30),
    p_tipo_proceso           SMALLINT
      
  --Arreglo para información del detalle de avances cerrados
  DEFINE 
    arr_avance_pago          DYNAMIC ARRAY OF RECORD 
    v_folio_dis              DECIMAL(9,0),
    v_nss                    CHAR(11),
    v_nrp                    CHAR(11),
    v_periodo_pago           CHAR(6),
    v_monto_apo_avance       DECIMAL(12,2),
    v_monto_amo_avance       DECIMAL(12,2),
    v_folio_pago             DECIMAL(9,0),
    v_monto_apo_pag          DECIMAL(12,2),
    v_monto_amo_pag          DECIMAL(12,2),
    v_folio_sua              DECIMAL(9,0),
    v_f_pago                 DATE,
    v_edo_compensa_apo       SMALLINT,
    v_edo_compensa_amo       SMALLINT,
    v_tipo_pago              VARCHAR(20)
    END RECORD 

  --Variables auxiliares
  DEFINE 
    v_folio_dis              VARCHAR(9),
    v_nss                    CHAR(11),
    v_nrp                    CHAR(11),
    v_periodo_pago           CHAR(6),
    v_monto_apo_avance       VARCHAR(14),
    v_monto_amo_avance       VARCHAR(14),
    v_folio_pago             VARCHAR(9),
    v_monto_apo_pag          VARCHAR(14),
    v_monto_amo_pag          VARCHAR(14),
    v_folio_sua              VARCHAR(9),
    v_f_pago                 VARCHAR(8)
      
  DEFINE 
    f_folio                  DECIMAL(9,0),
    v_total_registros        BIGINT,
    f_tot_ava_apo            DECIMAL(22,2),
    f_tot_ava_amo            DECIMAL(22,2),
    f_tot_pag_apo            DECIMAL(22,2),
    f_tot_pag_amo            DECIMAL(22,2)

  DEFINE 
    v_tot_registros          VARCHAR(22),
    v_tot_ava_apo            VARCHAR(22),
    v_tot_ava_amo            VARCHAR(22),
    v_tot_pag_apo            VARCHAR(22),
    v_tot_pag_amo            VARCHAR(22)

  DEFINE 
    v_folio_reg_pag          DECIMAL(9,0),
    v_etiqueta               STRING,
    g_sql_txt                STRING,
    v_proc_entra             SMALLINT,
    v_proc_val               SMALLINT,
    v_cod_conv               SMALLINT,
    v_desc_proc_val          CHAR(40),
    v_mensaje_val            STRING,
    p_proceso_cod            SMALLINT

END GLOBALS 

MAIN 
  LET p_usuario      = ARG_VAL(1) -- Recibe la variable de usuario
  LET p_tipo_proceso = ARG_VAL(2) -- Recibe el tipo de proceso
  LET p_nom_prog     = ARG_VAL(3) -- Recibe el nombre del programa

  LET p_proceso_cod  = 901

  LET v_total_registros = 0
   
  CLOSE WINDOW SCREEN

  OPEN WINDOW v_consulta WITH FORM "DISC121"
    DIALOG ATTRIBUTES(UNBUFFERED) 
      INPUT BY NAME f_folio
        BEFORE INPUT
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma = f_ventana.getForm()

          --Invocamos la función para asignar el título a la ventana
          CALL ui.Interface.setText(p_nom_prog)

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

          --Oculta las secciones de detalle de pagos
          CALL f_forma.setElementHidden("gr_avances",TRUE) --Oculta la sección de información de avances
          CALL f_forma.setElementHidden("gr_info_reg_pag",TRUE) --Oculta la sección de información de registro de pagos
         
        ON ACTION cancelar 
           EXIT DIALOG

        ON ACTION aceptar
           --Si el folio es nulo, crea la consulta de todos los avances
           IF f_folio IS NULL THEN
              --CALL fn_crea_temporal()
              CALL fn_mensaje("Error", "Debe introducir un folio de Dispersión", "information")
              NEXT FIELD f_folio 
           ELSE --De otra manera, filtra por folio
              CALL fn_crea_temporal_folio()
           END IF 

           --CALL fn_mensaje("Info", "Se ha generado la tabla", "information")
           CALL fn_info_avances() RETURNING v_total_registros

           DISPLAY "Total: ",v_total_registros
            
           IF v_total_registros IS NULL OR v_total_registros <= 0 THEN 
              CALL fn_mensaje("Info", "No existe información a mostrar. Verifique", "information")
           ELSE
              LET v_etiqueta = "Folio de Registro de Pagos:",v_folio_reg_pag

              CALL f_forma.setElementText("lbl_reg_pag",v_etiqueta)
              CALL f_forma.setElementHidden("gr_info_reg_pag",FALSE) --Muestra la sección de información de registro de pagos
              CALL f_forma.setElementHidden("gr_avances",FALSE) --Muestra la sección de información de avances

              DISPLAY ARRAY arr_avance_pago TO src_avances.* ATTRIBUTES (ACCEPT = FALSE)
                BEFORE DISPLAY 
                  DISPLAY BY NAME f_tot_ava_apo,f_tot_ava_amo,f_tot_pag_apo,f_tot_pag_amo, v_total_registros
                  ON ACTION archivo 
                     CALL fn_genera_interfase_avances()
                  ON ACTION reporte 
                     CALL fn_genera_reporte_control()
              END DISPLAY 
           END IF 
      END INPUT 
    END DIALOG 
END MAIN

#Función para crear la temporal con el filtro de entrada
FUNCTION fn_crea_temporal_folio()

  WHENEVER ERROR CONTINUE;
  DROP TABLE tmp_dis_avances_pago_apo;

  SELECT a.folio_dis,
         b.nss,
         a.nrp,
         a.periodo_pago,
         a.monto_apo_avance ava_apo,
         a.monto_amo_avance ava_amo,
         a.folio_pago,
         a.monto_apo_pag pag_apo,
         a.monto_amo_pag pag_amo,
         c.folio_sua,
         a.f_pago,
         a.edo_compensa_apo,
         a.edo_compensa_amo,
         "AVANCES" tpo_pago
  FROM   dis_compensa_avance a,
         afi_derechohabiente b,
         cta_his_pagos       c
  WHERE  a.folio_dis = f_folio
  AND    ((a.edo_compensa_amo IN(0,3) AND
           a.monto_amo_avance <> 0)   OR
          (a.edo_compensa_apo IN(0,3) AND
           a.monto_apo_avance <> 0))
  AND    a.id_derechohabiente = b.id_derechohabiente
  AND    a.folio_pago         = c.folio
  AND    a.id_referencia      = c.id_referencia
  AND    a.id_derechohabiente = c.id_derechohabiente
  INTO TEMP tmp_dis_avances_pago_apo;
      
  UPDATE STATISTICS FOR TABLE tmp_dis_avances_pago_apo;
      
END FUNCTION 

#Llena arreglo con información de la temporal
FUNCTION fn_info_avances()

  DEFINE 
    v_total_avances          BIGINT

  DISPLAY "consulta" 

  LET v_total_avances = 1
  LET f_tot_ava_apo   = 0.00
  LET f_tot_ava_amo   = 0.00
  LET f_tot_pag_apo   = 0.00
  LET f_tot_pag_amo   = 0.00

  DECLARE cur_consulta_avances CURSOR FOR
  SELECT *
  FROM   tmp_dis_avances_pago_apo
  FOREACH cur_consulta_avances INTO arr_avance_pago[v_total_avances].v_folio_dis,
                                    arr_avance_pago[v_total_avances].v_nss,
                                    arr_avance_pago[v_total_avances].v_nrp,
                                    arr_avance_pago[v_total_avances].v_periodo_pago,
                                    arr_avance_pago[v_total_avances].v_monto_apo_avance,
                                    arr_avance_pago[v_total_avances].v_monto_amo_avance,
                                    arr_avance_pago[v_total_avances].v_folio_pago,
                                    arr_avance_pago[v_total_avances].v_monto_apo_pag,
                                    arr_avance_pago[v_total_avances].v_monto_amo_pag,
                                    arr_avance_pago[v_total_avances].v_folio_sua,
                                    arr_avance_pago[v_total_avances].v_f_pago,
                                    arr_avance_pago[v_total_avances].v_edo_compensa_apo,
                                    arr_avance_pago[v_total_avances].v_edo_compensa_amo,
                                    arr_avance_pago[v_total_avances].v_tipo_pago

    --Verifica qu queden en un solo registro los pagos
    IF (arr_avance_pago[v_total_avances].v_edo_compensa_apo <> 0  AND
        arr_avance_pago[v_total_avances].v_edo_compensa_apo <> 3) THEN 
       LET arr_avance_pago[v_total_avances].v_monto_apo_avance = 0.00
       LET arr_avance_pago[v_total_avances].v_monto_apo_pag    = 0.00
    END IF

    IF (arr_avance_pago[v_total_avances].v_edo_compensa_amo <> 0  AND
        arr_avance_pago[v_total_avances].v_edo_compensa_amo <> 3) THEN 
       LET arr_avance_pago[v_total_avances].v_monto_amo_avance = 0.00
       LET arr_avance_pago[v_total_avances].v_monto_amo_pag    = 0.00
    END IF
                                     
    --Evalúa que no haya nulos
    IF arr_avance_pago[v_total_avances].v_monto_apo_avance IS NOT NULL THEN 
       LET f_tot_ava_apo = f_tot_ava_apo + arr_avance_pago[v_total_avances].v_monto_apo_avance
    END IF 
    IF arr_avance_pago[v_total_avances].v_monto_amo_avance IS NOT NULL THEN 
       LET f_tot_ava_amo = f_tot_ava_amo + arr_avance_pago[v_total_avances].v_monto_amo_avance
    END IF
    IF arr_avance_pago[v_total_avances].v_monto_apo_pag IS NOT NULL THEN 
       LET f_tot_pag_apo = f_tot_pag_apo + arr_avance_pago[v_total_avances].v_monto_apo_pag
    END IF 
    IF arr_avance_pago[v_total_avances].v_monto_amo_pag IS NOT NULL THEN 
       LET f_tot_pag_amo = f_tot_pag_amo + arr_avance_pago[v_total_avances].v_monto_amo_pag
    END IF 
                                     
    LET v_total_avances = v_total_avances + 1
  END FOREACH 

  CALL arr_avance_pago.deleteElement(v_total_avances)
  LET v_total_avances = v_total_avances - 1

  --Obtiene folio de registro de pagos
  SELECT folio_referencia
  INTO   v_folio_reg_pag 
  FROM   glo_folio
  WHERE  folio = f_folio

  RETURN v_total_avances
END FUNCTION 

# Genera un archivo txt con los datos del arreglo
FUNCTION fn_genera_interfase_avances()
  DEFINE 
    v_ch_arch_salida         BASE.CHANNEL,
    v_ruta_envio_dis         LIKE seg_modulo.ruta_envio,
    v_modulo_cod             LIKE seg_modulo.modulo_cod,
    v_nom_archivo            VARCHAR(40), -- nombre del archivo de salida
    v_ddmmaaaa               VARCHAR(08), -- fecha del archivo de salida
    v_busca_nom_archivo      STRING, -- busca nombre de archivo
    v_cont_dia               SMALLINT, -- consecutivo por dia de archivo generado
    v_reg_dia                CHAR(03), -- Parametro consecutivo de registro por dia
    v_ruta_nomarch           VARCHAR(100), -- ruta y nombre del archivo de salida
    v_comando_dos            STRING,
    v_encabezado             STRING,
    v_detalle                STRING,
    v_sumario                STRING,
    v_recorre_arreglo        BIGINT

  LET v_modulo_cod = "dis"

  --Se obtienen la ruta envío del módulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = v_modulo_cod

  -- se crea el nombre del archivo y posteriormente se concatena con la ruta
  LET v_nom_archivo       = "/avance_cubierto_" --nombre de archivo
  LET v_ddmmaaaa          = TODAY USING "ddmmyyyy"  --Fecha del archivo sin separadores
  LET v_busca_nom_archivo = "avance_cubierto_" || v_ddmmaaaa  --Concatena nombre a buscar

  --Obtiene consecutivo para archivo por día
  CALL fn_crea_nombre_archivo(v_ruta_envio_dis,v_busca_nom_archivo)
  RETURNING v_cont_dia

  LET v_reg_dia        = v_cont_dia USING "&&&"  --Consecutivo del día de numerico a char
  LET v_nom_archivo    = v_nom_archivo CLIPPED || v_ddmmaaaa || v_reg_dia||"."|| v_modulo_cod
  LET v_ruta_nomarch   = v_ruta_envio_dis CLIPPED || v_nom_archivo
  LET v_ch_arch_salida = base.Channel.create()
   
  -- se crea archivo y se indica que se escribira en el mismo
  CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
  CALL v_ch_arch_salida.setDelimiter("")

  #Imprime encabezado
  LET v_encabezado = "Folio: |",f_folio
  CALL v_ch_arch_salida.write([v_encabezado])
  LET v_encabezado = "Folio Registro Pagos: |",v_folio_reg_pag
  CALL v_ch_arch_salida.write([v_encabezado])

  #Imprime encabezado
  LET v_encabezado = "Folio Dis|NSS|NRP|Periodo Pago|Avance Apo|Avance Amo|Folio Pago|Pago Apo|Pago Amo|Folio SUA|Fecha Pago"
  CALL v_ch_arch_salida.write([v_encabezado])

  #Escribe en archivo datos de los avances
  FOR v_recorre_arreglo = 1 TO arr_avance_pago.getLength()
      LET v_folio_dis        = arr_avance_pago[v_recorre_arreglo].v_folio_dis 
      LET v_nss              = arr_avance_pago[v_recorre_arreglo].v_nss USING "&&&&&&&&&&&"
      LET v_nrp              = arr_avance_pago[v_recorre_arreglo].v_nrp --USING "&&&&&&&&&&&"
      LET v_periodo_pago     = arr_avance_pago[v_recorre_arreglo].v_periodo_pago USING "&&&&&&"
      LET v_monto_apo_avance = arr_avance_pago[v_recorre_arreglo].v_monto_apo_avance --USING "&&&&&&&&&&&&.&&"
      LET v_monto_amo_avance = arr_avance_pago[v_recorre_arreglo].v_monto_amo_avance --USING "&&&&&&&&&&&&.&&"
      LET v_folio_pago       = arr_avance_pago[v_recorre_arreglo].v_folio_pago --USING "&&&&&&&&&"
      LET v_monto_apo_pag    = arr_avance_pago[v_recorre_arreglo].v_monto_apo_pag --USING "&&&&&&&&&&&&.&&"
      LET v_monto_amo_pag    = arr_avance_pago[v_recorre_arreglo].v_monto_amo_pag --USING "&&&&&&&&&&&&.&&"
      LET v_folio_sua        = arr_avance_pago[v_recorre_arreglo].v_folio_sua --USING "&&&&&&&&&"
      LET v_f_pago           = arr_avance_pago[v_recorre_arreglo].v_f_pago USING "yyyymmdd"
      
      LET v_detalle          = v_folio_dis, "|",
                               v_nss, "|",
                               v_nrp, "|",
                               v_periodo_pago, "|",
                               v_monto_apo_avance, "|",
                               v_monto_amo_avance, "|",
                               v_folio_pago, "|",
                               v_monto_apo_pag, "|",
                               v_monto_amo_pag, "|",
                               v_folio_sua, "|",
                               v_f_pago

      CALL v_ch_arch_salida.write([v_detalle])
  END FOR 

  LET v_sumario = "Total Registros|Total Avance Aportación|Total Avance Amortización|Total Pago Aportación|Total Pago Amortización"
  CALL v_ch_arch_salida.write([v_sumario])

  LET v_tot_registros = v_total_registros
  LET v_tot_ava_apo   = f_tot_ava_apo
  LET v_tot_ava_amo   = f_tot_ava_amo
  LET v_tot_pag_apo   = f_tot_pag_apo
  LET v_tot_pag_amo   = f_tot_pag_amo
   
  LET v_sumario       = v_tot_registros , "|",
                        v_tot_ava_apo ,"|",
                        v_tot_ava_amo ,"|", 
                        v_tot_pag_apo ,"|",
                        v_tot_pag_amo ,"|"
  CALL v_ch_arch_salida.write([v_sumario])
   
  --Cierra el archivo
  CALL v_ch_arch_salida.close()
   
  --Cambia el formato del archivo a DOS
  LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos

  CALL fn_mensaje("Información","Se ha generado el archivo de Avances Cubiertos\n en la ruta"||v_ruta_nomarch,"information")
   
END FUNCTION 

#Genera el PDF con las cifras de control de los avances cubiertos
FUNCTION fn_genera_reporte_control()
  DEFINE 
    manejador_rpt            om.SaxDocumentHandler  -- Contenedor de documentos para reporte

  -- Botón para generar el reporte en PDF de la consulta
  IF fgl_report_loadCurrentSettings("DISC121.4rp") THEN 
     CALL fgl_report_selectDevice ("PDF")
     LET manejador_rpt = fgl_report_commitCurrentSettings()

     START REPORT rpt_avances_cubiertos TO XML HANDLER manejador_rpt
       OUTPUT TO REPORT rpt_avances_cubiertos(v_total_registros)
     FINISH REPORT rpt_avances_cubiertos
  ELSE 
     CALL fn_mensaje("Error","Error al generar el reporte. Verifique la ruta","information")
  END IF
   
END FUNCTION 

#Estructura del reporte de avances cubiertos
REPORT rpt_avances_cubiertos(v_total)
  DEFINE 
    v_fecha_reporte          DATE,
    v_total                  INTEGER 

FORMAT
  FIRST PAGE HEADER
    LET v_fecha_reporte = TODAY

    PRINTX v_fecha_reporte USING "dd-mm-yyyy"
    PRINTX p_usuario
    PRINTX v_total
    PRINTX f_folio
    PRINTX f_tot_ava_apo
    PRINTX f_tot_ava_amo
    PRINTX f_tot_pag_apo
    PRINTX f_tot_pag_amo
    PRINTX v_folio_reg_pag

END REPORT 

#Objetivo: genera el número consecutivo por día para el archivo de salida
FUNCTION fn_crea_nombre_archivo(p_ruta_envio_dis,p_busca_nom_archivo)
  DEFINE 
    p_ruta_envio_dis         LIKE seg_modulo.ruta_envio,  --Ruta donde se genera archivo
    p_busca_nom_archivo      VARCHAR(40),  --Nombre del archivo a buscar(nombre||fecha)
    v_cmd                    STRING,  --Cadena de comando a ejecutar
    v_consecutivo            INTEGER  --Consecutivo del archivo por día

  DEFINE 
    fn                       CHAR(22)  --Almacena el nombre completo del nombre del archivo en el servidor con su extensión

  DEFINE 
    ch                       base.Channel  --Canal de lectura

  LET v_cmd = "ls -lrt ",p_ruta_envio_dis CLIPPED,"/ | grep -i '",p_busca_nom_archivo CLIPPED,"' |awk '{print $9}'"

  LET ch = base.Channel.create()

  CALL ch.setDelimiter(".")
  CALL ch.openPipe(v_cmd,"r")

  WHILE ch.read([fn])
    LET v_consecutivo = fn[16,18]  --Posición del consecutivo dentro de la cadena
  END WHILE

  CALL ch.close()
  LET v_consecutivo = v_consecutivo + 1  --Incrementa consecutivo del día

  IF length(v_consecutivo) = 0 THEN  --Si es el primero del día
     LET v_consecutivo = 1
  END IF

  RETURN v_consecutivo  --Regresa el consecutivo del siguiente archivo del día

END FUNCTION