
######################################################################
#Modulo            => AGR                                            #
#Programa          => AGRE18                                         #
#Objetivo          => Programa para integrar el archivo actualzación #
#                     de marcas para solicitud de saldos.            #
#Autor             => Emilio Abarca, EFP                             #
#Fecha inicio      => 29/Agosto/2018                                 #
######################################################################

DATABASE safre_viv

GLOBALS

   DEFINE p_v_usuario         LIKE seg_usuario.usuario               
   DEFINE p_d_pid             LIKE bat_ctr_proceso.pid
   DEFINE p_i_proceso_cod     LIKE cat_proceso.proceso_cod           
   DEFINE p_i_opera_cod       LIKE cat_operacion.opera_cod           

   TYPE rec_total        RECORD
      total          INTEGER,
      aivs92         DECIMAL(16,6),
      aivs97         DECIMAL(16,6),
      porcentaje     CHAR(12)
   END RECORD

   TYPE rec_sspr         RECORD
      marca_orig       SMALLINT,
      marca_act        SMALLINT,
      marca_concatena  CHAR(20),
      total            INTEGER,
      aivs92           DECIMAL(16,6),
      aivs97           DECIMAL(16,6),
      porcentaje       CHAR(12)
   END RECORD

   TYPE rec_detalle_rechazos  RECORD
      marca_origen    SMALLINT,
      marca_prc       SMALLINT,
      total           INTEGER,
      aivs92          DECIMAL(16,6),
      aivs97          DECIMAL(16,6),
      estado          SMALLINT,
      desc_estado     CHAR(40)
   END RECORD

   TYPE arreglo_global        DYNAMIC ARRAY OF rec_sspr
   TYPE arreglo_det_rechazos  DYNAMIC ARRAY OF rec_detalle_rechazos

   -- Define records
   DEFINE r_sspr             rec_total
   DEFINE r_sspr_aceptados   rec_total
   DEFINE r_sspr_rechazados  rec_total
   -- Define Arreglos
   DEFINE arr_sspr           arreglo_global
   DEFINE arr_sspr_acep      arreglo_global
   DEFINE arr_sspr_rech      arreglo_global
   -- Detalle rechazos
   DEFINE arr_det_rechazos  arreglo_det_rechazos

   DEFINE v_aux_porcentaje  DECIMAL(6,2)
   DEFINE v_f_ini_opera     LIKE bat_ctr_operacion.fecha_ini
   DEFINE v_f_fin_opera     LIKE bat_ctr_operacion.fecha_fin
   DEFINE p_v_arch_proceso  VARCHAR(100)

   -- variables archivo de salida
   DEFINE v_nombre_archivo  STRING
   DEFINE v_ruta_envio      CHAR(40)
   DEFINE v_ruta_bin        CHAR(40)
   DEFINE v_ruta_lst        CHAR(40)
   DEFINE p_d_folio         LIKE glo_ctr_archivo.folio          

END GLOBALS

MAIN
 
   DEFINE v_d_cre_ctr_archivo LIKE cre_ctr_archivo.id_cre_ctr_archivo -- identificador del archivo
   DEFINE v_r_cre_ctr_archivo RECORD
      tot_registros    LIKE cre_ctr_archivo.tot_registros,            -- total de registros
      tot_aceptados    LIKE cre_ctr_archivo.tot_aceptados,            -- total aceptados
      tot_rechazados   LIKE cre_ctr_archivo.tot_rechazados           -- total rechazados
   END RECORD
   DEFINE v_s_comando         STRING                                  -- contiene al comando a correr
   DEFINE v_s_qryTxt          STRING                                  -- guarda una sentencia SQL a ejecutar
   DEFINE v_id_operacion      LIKE cre_ctr_archivo.operacion          -- operacion
   DEFINE v_dt_f_lote         LIKE cre_ctr_archivo.f_lote             -- fecha del lote
   DEFINE v_si_lote           LIKE cre_ctr_archivo.lote               -- lote
   DEFINE v_id_proceso        LIKE cre_ctr_archivo.id_proceso         -- identificador del proceso
   DEFINE r_b_valida          SMALLINT                                -- booleana que indica si el proceso se puede ejecutar o no
   DEFINE r_isam_err          INTEGER
   DEFINE r_c_msj             VARCHAR(250)
   DEFINE r_c_nss             LIKE afi_derechohabiente.nss

   -- Parametros que envia el programa lanzador AGRL74
   LET p_v_usuario         = ARG_VAL(1)
   LET p_d_pid             = ARG_VAL(2)
   LET p_i_proceso_cod     = ARG_VAL(3)
   LET p_i_opera_cod       = ARG_VAL(4)
   LET p_d_folio           = ARG_VAL(5)
   LET p_v_arch_proceso    = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".AGRE18.log")

   DISPLAY " "
   DISPLAY "=INICIA AGRE18="
   DISPLAY " USUARIO         : ",p_v_usuario
   DISPLAY " PID             : ",p_d_pid USING "<<<<<<<<<"
   DISPLAY " ARCHIVO:        : ",p_v_arch_proceso

   -- Inicializan variables
   LET v_id_operacion = 1   -- VALIDA ARCHIVO SOLICITUD SALDOS
   LET v_id_proceso   = 350 -- ACTUALIZACIÓN MARCA SOLICITUD SALDO

   SELECT ruta_bin,
          ruta_listados,
          ruta_envio
    INTO v_ruta_bin,
         v_ruta_lst,
         v_ruta_envio
    FROM seg_modulo
   WHERE modulo_cod = 'agr';

   -- Genera el folio
   LET p_d_folio = fn_genera_folio(p_i_proceso_cod, p_i_opera_cod, p_v_usuario)
   DISPLAY " FOLIO           : ",p_d_folio USING "<<<<<<<<<"

   -- Identificador del archivo cargado
   SELECT FIRST 1 id_cre_ctr_archivo
     INTO v_d_cre_ctr_archivo
     FROM cre_ctr_archivo
    WHERE id_proceso = v_id_proceso
      AND operacion  = v_id_operacion
      AND estado     = 10
    ORDER BY id_cre_ctr_archivo DESC;

   IF (v_d_cre_ctr_archivo IS NULL) THEN
      DISPLAY " ERROR           : No fue posible obtener el identificador del archivo"
      EXIT PROGRAM
   END IF

   DISPLAY " ID_ARCHIVO      : ",v_d_cre_ctr_archivo USING "<<<<<<<<<"

   --CALL crea_tab_sol_sdo() -- Crea tabla para el diagnóstico de las solicitudes

   DISPLAY " "
   DISPLAY " INTEGRA ACTUALIZACIÓN MARCA SSPR"
   DISPLAY " "

   -- Ejecuta la función de integración
   LET v_s_qryTxt = "EXECUTE FUNCTION fn_agr_integra_sol_sdo_prc(?,?,?,?)"

   PREPARE prp_integra_sspr FROM v_s_qryTxt
   EXECUTE prp_integra_sspr USING p_v_usuario,
                                  p_v_arch_proceso,
                                  p_d_folio,
                                  v_d_cre_ctr_archivo
                             INTO r_b_valida,
                                  r_isam_err,
                                  r_c_msj,
                                  r_c_nss

   IF r_b_valida <> 0 THEN
      DISPLAY " Ocurrió un error durante el proceso de Integración:"
      DISPLAY "ERROR      : ",r_b_valida
      DISPLAY "ISAM ERR   : ",r_isam_err
      DISPLAY "MENSAJE ERR: ",r_c_msj
      DISPLAY "NSS        : ",r_c_nss

      -- ocurrió un error y se marca como rechazado la operación
      LET r_b_valida = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

      EXIT PROGRAM
   END IF

   -- Obtiene cifras control
   SELECT tot_registros,
          tot_aceptados,
          tot_rechazados
     INTO v_r_cre_ctr_archivo.tot_registros,
          v_r_cre_ctr_archivo.tot_aceptados,
          v_r_cre_ctr_archivo.tot_rechazados
     FROM cre_ctr_archivo
    WHERE id_cre_ctr_archivo = v_d_cre_ctr_archivo;

   DISPLAY " TOTAL REGISTROS :",v_r_cre_ctr_archivo.tot_registros
   DISPLAY " TOTAL ACEPTADOS :",v_r_cre_ctr_archivo.tot_aceptados
   DISPLAY " TOTAL RECHAZADOS:",v_r_cre_ctr_archivo.tot_rechazados
   DISPLAY " "

   -- Función que genera el archivo de salida
   CALL genera_arh_salida()

   DISPLAY " GENERA ARCHIVO DE SALIDA... COMPLETADO"
   DISPLAY " El archivo podrá ser recuperado en la ruta: ",v_ruta_envio
   DISPLAY " con el nombre: ",v_nombre_archivo
   DISPLAY " "
   
   -- Finaliza Proceso
   LET r_b_valida = fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

   -- se verifica si fue posible finalizar la operacion
   IF(r_b_valida <> 0) THEN
      -- en caso de error se muestra un mensaje a usuario
      CALL fn_desplega_inc_operacion(r_b_valida)
      --EXIT PROGRAM
   END IF


   -- Función que genera el reporte
   CALL genera_rpt_pdf()

   DISPLAY " GENERA REPORTE PDF... COMPLETADO"

   DISPLAY " "
   DISPLAY " = FIN AGRE18 ="
   DISPLAY " "

END MAIN

FUNCTION genera_arh_salida()

   DEFINE v_qry_arh       STRING
   DEFINE r_arh_sspr      RECORD
      nss         CHAR(11),
      aivs92      CHAR(15),
      aivs97      CHAR(15),
      periodo_pag CHAR(6),
      tpo_sol     CHAR(2),
      marca_orig  CHAR(3),
      marca_prc   CHAR(1),
      diagnostico CHAR(3)
   END RECORD
   DEFINE archivo          base.channel
   DEFINE v_salida_arh     STRING
   DEFINE v_detalle        STRING

   LET v_nombre_archivo = "AMSSPR_",TODAY USING "yyyymmdd","_resp.ctl" CLIPPED
   LET v_salida_arh = v_ruta_envio CLIPPED,"/",v_nombre_archivo
   LET archivo = base.Channel.create()

   LET v_qry_arh = "SELECT nss,\n
                           aivs_92,\n
                           aivs_97,\n
                           periodo_pago,\n
                           tpo_solicitud,\n
                           marca_orig,\n
                           marca_prc,\n
                           diagnostico
                      FROM cre_act_marca_sspr
                     WHERE diagnostico <> 0 
                       AND folio = ",p_d_folio

   PREPARE prp_salida FROM v_qry_arh
   DECLARE crs_salida CURSOR FOR prp_salida

   INITIALIZE r_arh_sspr.* TO NULL
   LET v_detalle       = NULL

   -- Abre archivo de salida para escritura
   CALL archivo.openFile(v_salida_arh,"w")

   FOREACH crs_salida INTO r_arh_sspr.nss,
                           r_arh_sspr.aivs92,
                           r_arh_sspr.aivs97,
                           r_arh_sspr.periodo_pag,
                           r_arh_sspr.tpo_sol,
                           r_arh_sspr.marca_orig,
                           r_arh_sspr.marca_prc,
                           r_arh_sspr.diagnostico

      LET v_detalle = r_arh_sspr.nss,
                      r_arh_sspr.aivs92      USING "&&&&&&&&&.&&&&&",
                      r_arh_sspr.aivs97      USING "&&&&&&&&&.&&&&&",
                      r_arh_sspr.periodo_pag USING "&&&&&&",
                      r_arh_sspr.tpo_sol,
                      r_arh_sspr.marca_orig  USING "&&&",
                      r_arh_sspr.marca_prc   USING "&",
                      r_arh_sspr.diagnostico USING "&&&"

      CALL archivo.writeLine(v_detalle)

   END FOREACH

   --cierra archivo
   CALL archivo.close()

END FUNCTION

FUNCTION genera_rpt_pdf()

   DEFINE v_reporte_bin    STRING
   DEFINE v_ruta_rpt       STRING
   DEFINE v_manejador_rpt  OM.SaxDocumentHandler
   DEFINE v_k              INTEGER

   SELECT fecha_ini,
          fecha_fin
     INTO v_f_ini_opera,
          v_f_fin_opera
     FROM bat_ctr_operacion
    WHERE pid = p_d_pid
      AND proceso_cod = p_i_proceso_cod
      AND opera_cod   = p_i_opera_cod;

   -- Recupera información de todos los registross
   DECLARE crs_sspr CURSOR FOR
   SELECT marca_orig,
          marca_act,
          SUM(aivs_92),
          SUM(aivs_97),
          COUNT(*)
     FROM cre_act_marca_sspr
    WHERE folio = p_d_folio
     GROUP BY 1,2
     ORDER BY marca_orig;

   LET v_aux_porcentaje = 0

   -- Inicializa records de totales globales
   LET r_sspr.total  = 0
   LET r_sspr.aivs92 = 0
   LET r_sspr.aivs97 = 0

   --inicializa el arreglos
   LET arr_sspr[1].marca_concatena = "Sin registros"
   LET arr_sspr[1].marca_act       = 0
   LET arr_sspr[1].total           = 0
   LET arr_sspr[1].aivs92          = 0
   LET arr_sspr[1].aivs92          = 0

   LET v_k = 1     -- Inicializa contador

   FOREACH crs_sspr INTO arr_sspr[v_k].marca_orig,
                         arr_sspr[v_k].marca_act,
                         arr_sspr[v_k].aivs92,
                         arr_sspr[v_k].aivs97,
                         arr_sspr[v_k].total

      -- Incrementa total global
      LET r_sspr.total = r_sspr.total + arr_sspr[v_k].total

      -- Incrementa aivs del total global
      LET r_sspr.aivs92 = r_sspr.aivs92 + arr_sspr[v_k].aivs92
      LET r_sspr.aivs97 = r_sspr.aivs97 + arr_sspr[v_k].aivs97

      -- Concatena marca en conciliación
      LET arr_sspr[v_k].marca_concatena = "Marca C.",arr_sspr[v_k].marca_orig CLIPPED

      LET v_k = v_k + 1
      
   END FOREACH

   IF(v_k > 1) THEN
      -- Elimina fila en blanco
      IF(arr_sspr[arr_sspr.getLength()].marca_orig IS NULL) THEN
         CALL arr_sspr.deleteElement(arr_sspr.getLength())
      END IF
   END IF

   -- Porcentaje total global recurrente
   LET v_aux_porcentaje = (r_sspr.total / r_sspr.total) * 100
   LET r_sspr.porcentaje = v_aux_porcentaje CLIPPED,"%"

   -- Recupera información de todos los registros ACEPTADOS
   DECLARE crs_sspr_aceptados CURSOR FOR
   SELECT marca_orig,
          marca_act,
          SUM(aivs_92),
          SUM(aivs_97),
          COUNT(*)
     FROM cre_act_marca_sspr
    WHERE diagnostico = 0
      AND folio = p_d_folio
    GROUP BY 1,2
    ORDER BY marca_orig;

   -- Inicializa records de totales globales
   LET r_sspr_aceptados.total  = 0
   LET r_sspr_aceptados.aivs92 = 0
   LET r_sspr_aceptados.aivs97 = 0

   --inicializa el arreglos
   LET arr_sspr_acep[1].marca_concatena = "Sin registros"
   LET arr_sspr_acep[1].marca_act       = 0
   LET arr_sspr_acep[1].total           = 0
   LET arr_sspr_acep[1].aivs92          = 0
   LET arr_sspr_acep[1].aivs97          = 0

   LET v_k = 1     -- Inicializa contador

   FOREACH crs_sspr_aceptados INTO arr_sspr_acep[v_k].marca_orig,
                                   arr_sspr_acep[v_k].marca_act,
                                   arr_sspr_acep[v_k].aivs92,
                                   arr_sspr_acep[v_k].aivs97,
                                   arr_sspr_acep[v_k].total

      -- Incrementa total global recurrente
      LET r_sspr_aceptados.total = r_sspr_aceptados.total + arr_sspr_acep[v_k].total

      -- Incrementa aivs
      LET r_sspr_aceptados.aivs92 = r_sspr_aceptados.aivs92 + arr_sspr_acep[v_k].aivs92
      LET r_sspr_aceptados.aivs97 = r_sspr_aceptados.aivs97 + arr_sspr_acep[v_k].aivs97

      -- Concatena marca en conciliación
      LET arr_sspr_acep[v_k].marca_concatena = "Marca C.",arr_sspr_acep[v_k].marca_orig CLIPPED

      LET v_k = v_k + 1
      
   END FOREACH

    IF(v_k > 1) THEN
      -- Elimina fila en blanco
      IF(arr_sspr_acep[arr_sspr_acep.getLength()].marca_orig IS NULL) THEN
         CALL arr_sspr_acep.deleteElement(arr_sspr_acep.getLength())
      END IF
   END IF

   -- Porcentaje total global recurrente
   LET v_aux_porcentaje = (r_sspr_aceptados.total / r_sspr_aceptados.total) * 100
   LET r_sspr_aceptados.porcentaje = v_aux_porcentaje CLIPPED,"%"

   -- Recupera información de todos los registros RECHAZADOS
   DECLARE crs_sspr_rechazados CURSOR FOR
   SELECT marca_orig,
          marca_act,
          SUM(aivs_92),
          SUM(aivs_97),
          COUNT(*)
     FROM cre_act_marca_sspr
    WHERE diagnostico <> 0
      AND folio = p_d_folio
    GROUP BY 1,2
    ORDER BY marca_orig

   -- Inicializa records de totales globales
   LET r_sspr_rechazados.total  = 0
   LET r_sspr_rechazados.aivs92 = 0
   LET r_sspr_rechazados.aivs97 = 0

   --inicializa el arreglos
   LET arr_sspr_rech[1].marca_concatena = "Sin registros"
   LET arr_sspr_rech[1].marca_act       = 0
   LET arr_sspr_rech[1].total           = 0
   LET arr_sspr_rech[1].aivs92          = 0
   LET arr_sspr_rech[1].aivs97          = 0

   LET v_k = 1     -- Inicializa contador

   FOREACH crs_sspr_rechazados INTO arr_sspr_rech[v_k].marca_orig,
                                    arr_sspr_rech[v_k].marca_act,
                                    arr_sspr_rech[v_k].aivs92,
                                    arr_sspr_rech[v_k].aivs97,
                                    arr_sspr_rech[v_k].total

      -- Incrementa total global recurrente
      LET r_sspr_rechazados.total = r_sspr_rechazados.total + arr_sspr_rech[v_k].total

      -- Incrementa aivs
      LET r_sspr_rechazados.aivs92 = r_sspr_rechazados.aivs92 + arr_sspr_rech[v_k].aivs92
      LET r_sspr_rechazados.aivs97 = r_sspr_rechazados.aivs97 + arr_sspr_rech[v_k].aivs97

      -- Concatena marca en conciliación
      LET arr_sspr_rech[v_k].marca_concatena = "Marca C.",arr_sspr_rech[v_k].marca_orig CLIPPED

      LET v_k = v_k + 1
      
   END FOREACH

   IF(v_k > 1) THEN
      -- Elimina fila en blanco
      IF(arr_sspr_rech[arr_sspr_rech.getLength()].marca_orig IS NULL) THEN
         CALL arr_sspr_rech.deleteElement(arr_sspr_rech.getLength())
      END IF
   END IF

   -- Porcentaje total global recurrente
   LET v_aux_porcentaje = (r_sspr_rechazados.total / r_sspr_rechazados.total) * 100
   LET r_sspr_rechazados.porcentaje = v_aux_porcentaje CLIPPED,"%"

   ## DETALLE RECHAZOS

   -- Inicializa arreglo
   LET arr_det_rechazos[1].marca_origen = 0
   LET arr_det_rechazos[1].marca_prc    = 0
   LET arr_det_rechazos[1].total        = 0
   LET arr_det_rechazos[1].aivs92       = 0
   LET arr_det_rechazos[1].aivs97       = 0
   LET arr_det_rechazos[1].estado       = 0
   LET arr_det_rechazos[1].desc_estado  = " "

   DECLARE crs_det_rechazos CURSOR FOR
   SELECT t.marca_orig,
          t.marca_prc,
          SUM(t.aivs_92),
          SUM(t.aivs_97),
          t.diagnostico,
          c.desc_estado,
          COUNT(*)
     FROM cre_act_marca_sspr t,
          cat_rch_acreditado c
    WHERE t.diagnostico = c.estado
      AND t.diagnostico <> 0
      AND t.folio = p_d_folio
      GROUP BY 1,2,5,6
      ORDER BY t.marca_orig;

   LET v_k = 1

   FOREACH crs_det_rechazos INTO arr_det_rechazos[v_k].marca_origen,
                                 arr_det_rechazos[v_k].marca_prc,
                                 arr_det_rechazos[v_k].aivs92,
                                 arr_det_rechazos[v_k].aivs97,
                                 arr_det_rechazos[v_k].estado,
                                 arr_det_rechazos[v_k].desc_estado,
                                 arr_det_rechazos[v_k].total

      LET v_k = v_k + 1

   END FOREACH


   #################################################
   #   CONFIGURACION PARA SALIDA DEL REPORTE PDF   #
   #################################################

   LET v_reporte_bin = v_ruta_bin CLIPPED,"/AGRE181.4rp"
   LET v_ruta_rpt    = v_ruta_lst CLIPPED,"/",
                       p_v_usuario CLIPPED,"-AGRL74-",
                       p_d_pid USING "&&&&&","-",
                       p_i_proceso_cod USING "&&&&&","-",
                       p_i_opera_cod USING "&&&&&",".pdf"

   IF (fgl_report_loadCurrentSettings(v_reporte_bin)) THEN
      CALL fgl_report_selectDevice ("PDF")
      CALL fgl_report_selectPreview(0)
      CALL fgl_report_setOutputFileName(v_ruta_rpt)
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()

      IF (v_manejador_rpt IS NOT NULL) THEN

         START REPORT genera_PDF TO XML HANDLER v_manejador_rpt

            OUTPUT TO REPORT genera_PDF()

         FINISH REPORT genera_PDF

      END IF
   ELSE
       DISPLAY " ERROR: No fue posible abrir plantilla del reporte"
   END IF

END FUNCTION

REPORT genera_PDF()

   DEFINE v_f_presentacion DATE
   DEFINE f                INTEGER

   FORMAT
   FIRST PAGE HEADER
      LET v_f_presentacion = TODAY

      #ENCABEZADO
      PRINTX p_v_usuario
      PRINTX v_f_presentacion USING "dd/mm/yyyy"
      PRINTX p_v_arch_proceso
      PRINTX v_f_ini_opera
      PRINTX v_f_fin_opera
   
      --> Records totales
      PRINTX r_sspr.total
      PRINTX r_sspr.aivs92
      PRINTX r_sspr.aivs97
      PRINTX r_sspr.porcentaje
      PRINTX r_sspr_aceptados.total
      PRINTX r_sspr_aceptados.aivs92
      PRINTX r_sspr_aceptados.aivs97
      PRINTX r_sspr_aceptados.porcentaje
      PRINTX r_sspr_rechazados.total
      PRINTX r_sspr_rechazados.aivs92
      PRINTX r_sspr_rechazados.aivs97
      PRINTX r_sspr_rechazados.porcentaje

   ON EVERY ROW

      LET v_aux_porcentaje = 0

      ---> ARREGLO GLOBAL
      -- Total global
      FOR f = 1 TO arr_sspr.getLength()
         PRINTX arr_sspr[f].marca_concatena
         PRINTX arr_sspr[f].marca_act
         PRINTX arr_sspr[f].total
         PRINTX arr_sspr[f].aivs92
         PRINTX arr_sspr[f].aivs97

         -- Obtiene porcentaje
         LET v_aux_porcentaje = (arr_sspr[f].total / r_sspr.total) * 100
         LET arr_sspr[f].porcentaje = v_aux_porcentaje CLIPPED,"%"

         PRINTX arr_sspr[f].porcentaje
      END FOR

      -- Total Aceptados
      FOR f = 1 TO arr_sspr_acep.getLength()
         PRINTX arr_sspr_acep[f].marca_concatena
         PRINTX arr_sspr_acep[f].marca_act
         PRINTX arr_sspr_acep[f].total
         PRINTX arr_sspr_acep[f].aivs92
         PRINTX arr_sspr_acep[f].aivs97

         -- Obtiene porcentaje
         LET v_aux_porcentaje = (arr_sspr_acep[f].total / r_sspr_aceptados.total) * 100
         LET arr_sspr_acep[f].porcentaje = v_aux_porcentaje CLIPPED,"%"

         PRINTX arr_sspr_acep[f].porcentaje
      END FOR

      -- Total Rechazados
      FOR f = 1 TO arr_sspr_rech.getLength()
         PRINTX arr_sspr_rech[f].marca_concatena
         PRINTX arr_sspr_rech[f].marca_act
         PRINTX arr_sspr_rech[f].total
         PRINTX arr_sspr_rech[f].aivs92
         PRINTX arr_sspr_rech[f].aivs97

         -- Obtiene porcentaje
         LET v_aux_porcentaje = (arr_sspr_rech[f].total / r_sspr_rechazados.total) * 100
         LET arr_sspr_rech[f].porcentaje = v_aux_porcentaje CLIPPED,"%"

         PRINTX arr_sspr_rech[f].porcentaje
      END FOR

      --Detalle rechazos
      FOR f = 1 TO arr_det_rechazos.getLength()
         PRINTX arr_det_rechazos[f].marca_origen
         PRINTX arr_det_rechazos[f].marca_prc
         PRINTX arr_det_rechazos[f].total
         PRINTX arr_det_rechazos[f].aivs92
         PRINTX arr_det_rechazos[f].aivs97
         PRINTX arr_det_rechazos[f].estado
         PRINTX arr_det_rechazos[f].desc_estado
      END FOR
      
END REPORT
{
FUNCTION crea_tab_sol_sdo()

   WHENEVER ERROR CONTINUE
      -- se elimina la tabla
      DROP TABLE tmp_act_marca_sspr

   -- al encontrar un error detiene el programa
   WHENEVER ERROR STOP
   CREATE TABLE tmp_act_marca_sspr(
                   nss                CHAR(11),
                   id_derechohabiente DECIMAL(9,0),
                   aivs_92            DECIMAL(12,6),
                   aivs_97            DECIMAL(12,6),
                   periodo_pago       CHAR(6),
                   tpo_solicitud      CHAR(2),
                   marca_orig         SMALLINT,
                   marca_prc          SMALLINT,
                   marca_act          SMALLINT,
                   diagnostico        SMALLINT);

END FUNCTION
}


