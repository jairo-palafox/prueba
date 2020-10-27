--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

#########################################################################
#Modulo            =>GRT                                                #
#Programa          =>GRTP17                                             #
#Objetivo          =>Programa que realiza la conciliación de la infor-  #
#                    mación de deudor vs tmp deudor no atendidas para   #
#                    el módulo de Uso de Garantía 43 bis                #
#Autor             =>Daniel Buendia, EFP                                #
#Fecha inicio      =>26 Abril 2012                                      #
#Autor modifica    =>Emilio Abarca, EFP                                 #
#Fecha modifica    =>16 Mayo 2018.                                      #
#Desc. modifica    =>Adecuación al reporte PDF y generación de archivo  #
#                    de salida para las solicitudes no atendidas.       #
#########################################################################

DATABASE safre_viv
GLOBALS "GRTG01.4gl"

   DEFINE p_v_usuario           LIKE seg_usuario.usuario -- nombre del usuario
   DEFINE p_d_pid               LIKE bat_ctr_proceso.pid -- pid
   DEFINE p_i_proceso_cod       LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE p_i_opera_cod         LIKE cat_operacion.opera_cod -- codigo de la operacion
   DEFINE p_d_folio             VARCHAR(10)
   DEFINE p_v_arch_proceso      LIKE cre_ctr_archivo.nom_archivo -- nombre del archivo a integrar

   --variables para el reporte
   DEFINE v_ruta_bin            LIKE seg_modulo.ruta_bin
   DEFINE v_ruta_listados       LIKE seg_modulo.ruta_listados
   DEFINE v_si_operacion        LIKE cre_ctr_archivo.operacion -- operacion del proceso
   DEFINE v_d_id_cre_ctr_arch   LIKE cre_ctr_archivo.id_cre_ctr_archivo -- id del archiovo
   DEFINE v_r_cre_ctr_arch      RECORD LIKE cre_ctr_archivo.* -- registro de cre ctr archivo
   DEFINE v_r_bat_ctr_opera     RECORD LIKE bat_ctr_operacion.* -- registro de bat ctr operación
   DEFINE v_f_movimiento        DATE
   DEFINE v_ruta_rpt            STRING 
   DEFINE v_c_programa_cod      LIKE bat_ctr_operacion.programa_cod -- nombrel del programa
   DEFINE v_aux_porcentaje      DECIMAL(6,2)
   DEFINE v_d_id_proceso        LIKE cre_ctr_archivo.id_proceso -- identificador del proceso

   #RECORDS Y ARREGLOS PARA LA INFORMACIÓN DEL REPORTE
   DEFINE r_total_global    RECORD 
      t_registros     INTEGER,
      aivs97          DECIMAL(20,2),
      pesos97         DECIMAL(20,2)
   END RECORD

   DEFINE r_total_marca   RECORD
      t_registros     INTEGER,
      aivs97          DECIMAL(20,2),
      pesos97         DECIMAL(20,2),
      porc_concatena  CHAR(12)
   END RECORD

   DEFINE r_total_saldo    RECORD
      t_registros     INTEGER,
      aivs97          DECIMAL(20,2),
      pesos97         DECIMAL(20,2),
      porc_concatena  CHAR(12)
   END RECORD

   DEFINE arr_marca DYNAMIC ARRAY OF RECORD 
      estado          SMALLINT,
      estado_desc     CHAR(40),
      t_registros     INTEGER,
      aivs97          DECIMAL(20,2),
      pesos97         DECIMAL(20,2),
      porc_concatena  CHAR(12)
   END RECORD

   DEFINE arr_saldo DYNAMIC ARRAY OF RECORD 
      estado          SMALLINT,
      estado_desc     CHAR(40),
      t_registros     INTEGER,
      aivs97          DECIMAL(20,2),
      pesos97         DECIMAL(20,2),
      porc_concatena  CHAR(12)
   END RECORD

   DEFINE v_s_titulo_correo      STRING -- contiene el titulo del correo
   DEFINE v_s_archivo_correo     STRING -- ruta y nombre del archivo adjunto en el correo
   DEFINE v_s_mens_correo        STRING -- contiene el cuerpo del correo
   DEFINE r_b_valida             SMALLINT
   --variables para el archivo de salida
   DEFINE v_arh_salida           STRING
   DEFINE v_ruta_envio           CHAR(40)
   DEFINE v_s_qryTxt             STRING
   DEFINE v_d_pid_arch           LIKE bat_ctr_operacion.pid
   DEFINE v_reporte_bin          STRING
   DEFINE v_manejador_rpt        OM.SaxDocumentHandler -- Contenedor de Documentos para el reporte
   
# Objetivo: Conciliar la información de Rechazo de Saldos
MAIN
   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario         = ARG_VAL(1)
   LET p_d_pid             = ARG_VAL(2)
   LET p_i_proceso_cod     = ARG_VAL(3)
   LET p_i_opera_cod       = ARG_VAL(4)
   LET p_d_folio           = ARG_VAL(5)
   LET p_v_arch_proceso    = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".GRTP17.log")

   DISPLAY " "
   DISPLAY "=INICIA GRTP17="
   DISPLAY " CONCILIACIÓN DE SOLICITUDES NO ATENDIDAS GRT"
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"
   DISPLAY " ARCHIVO       : ",p_v_arch_proceso
   DISPLAY " "

   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'grt';
    
   -- Se inicializan variables
   LET v_si_operacion = 14 -- Solicitudes no atendidas

   -- se asigna el nombre del programa segun el lanzador del proceso para crear el
   -- nombre del reporte y que este se pueda mostrar en el monitor de procesos
   IF p_i_proceso_cod = g_proc_cod_grt_uso_cocilia THEN
      --LET v_c_programa_cod = "GRTL29" -- Conciliación
      LET v_d_id_proceso = g_id_proceso_grt_uso
   ELSE
      IF p_i_proceso_cod = g_proc_cod_grt_no_atendidas THEN
         --LET v_c_programa_cod = "GRTL10" -- Integración No Atendidas GRT
         LET v_d_id_proceso = g_id_proceso_grt
      ELSE
         --LET v_c_programa_cod = "GRTL24" -- Integración No Atendidas USO
         LET v_d_id_proceso = g_id_proceso_grt_uso
      END IF
   END IF

   -- se obtiene el nombrel del programa correspondiente
   LET v_c_programa_cod = fn_obten_nom_programa(p_i_proceso_cod , p_i_opera_cod)

   -- recupera la ruta de listados en el que se enviara el archivo
   CALL fn_rutas("grt") RETURNING v_ruta_bin, v_ruta_listados

    -- se consulta el folio del archivo
   LET v_s_qryTxt = " SELECT MAX(id_cre_ctr_archivo)\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE id_proceso = ",v_d_id_proceso,"\n",
                    "    AND operacion = ",v_si_operacion

   PREPARE prp_folio_archivo FROM v_s_qryTxt
   EXECUTE prp_folio_archivo INTO v_d_id_cre_ctr_arch

   DISPLAY " Identificador del archivo: ",v_d_id_cre_ctr_arch
   DISPLAY " "

   -- se valida el identificador del archivo
   IF v_d_id_cre_ctr_arch IS NULL THEN
      DISPLAY " ERROR: No fue posible obtener el identificador del archivo"
      EXIT PROGRAM
   END IF

   -- se crea la sentencia sql que busca la información del archivo cargado
   LET v_s_qryTxt = " SELECT *\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE id_cre_ctr_archivo = ",v_d_id_cre_ctr_arch

   PREPARE prp_cre_ctr_arch FROM v_s_qryTxt
   EXECUTE prp_cre_ctr_arch INTO v_r_cre_ctr_arch.*
   
   LET v_d_pid_arch = fn_max_pid(p_i_proceso_cod, p_i_opera_cod)
   DISPLAY " PID del archivo: ",v_d_pid_arch

   #Crea temporal para el PDF
   CALL crea_temporal()

   CALL obtiene_info_rpt()
   DISPLAY " > OBTIENE INFORMACIÓN PARA EL REPORTE PDF ...COMPLETADO"
   DISPLAY ""
   CALL genera_archivo_salida()
   DISPLAY " > GENERA ARCHIVO DE SALIDA ...COMPLETADO"
   DISPLAY "  El archivo de salida de ha generado en /safreviv_int/grt/envio"
   DISPLAY "  con nombre: ",v_arh_salida           
   DISPLAY " "
   DISPLAY " > GENERA REPORTE PDF"

   -- actualiza la operacion a finalizada
   CALL fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, p_i_opera_cod) RETURNING r_b_valida

   IF(r_b_valida <> 0)THEN
      # En caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)
      CALL fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod) RETURNING r_b_valida
   END IF

   -- se crea la sentencia sql que busca la información de la operación
   LET v_s_qryTxt = " SELECT *\n",
                    "   FROM bat_ctr_operacion\n",
                    "  WHERE pid = ",v_d_pid_arch,"\n",
                    "    AND proceso_cod = ",p_i_proceso_cod,"\n",
                    "    AND opera_cod = 2"

   PREPARE prp_bat_ctr_opera FROM v_s_qryTxt
   EXECUTE prp_bat_ctr_opera INTO v_r_bat_ctr_opera.*

   #################################################
   #   CONFIGURACIÓN PARA SALIDA DEL REPORTE PDF   #
   #################################################   

   LET v_reporte_bin = v_ruta_bin CLIPPED,"/GRTP171.4rp"
   LET v_ruta_rpt    = v_ruta_listados CLIPPED,"/",
                       p_v_usuario CLIPPED,"-",v_c_programa_cod CLIPPED,"-",
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
       DISPLAY " Advertencia: No fue posible abrir plantilla del reporte"
   END IF

   -- se actualiza el folio en el registro correspondiente a las tablas de control del monitor del proceso
   UPDATE bat_ctr_operacion
      SET folio = p_d_folio
    WHERE pid = p_d_pid
      AND proceso_cod = p_i_proceso_cod
      AND opera_cod   = p_i_opera_cod

   DISPLAY " ENVIA CORREO DEL REPORTE"
   -- se asigna el titulo del correo
   LET v_s_titulo_correo = "Proceso: CONCILIACIÓN DE SOLICITUDES NO ATENDIDAS GRT"

   -- se asigna el archivo a adjuntar
   LET v_s_archivo_correo = v_ruta_rpt

   -- se asigna el cuerpo del correo
   LET v_s_mens_correo =  "ID Proceso   : ",p_d_pid,"\n",
                          "Proceso      : CONCILIACIÓN\n",
                          "Operacion    : SOLICITUDES NO ATENDIDAS GRT\n",
                          "Fecha Inicio : ",TODAY,"\n",
                          "Fecha Fin    : ",TODAY

   -- se invoca la función que envía por correo el elemento generado
   CALL fn_correo_proceso(p_d_pid,
                          p_i_proceso_cod,
                          p_i_opera_cod,
                          v_s_archivo_correo,
                          v_s_titulo_correo,
                          v_s_mens_correo)

   DISPLAY "=FIN="

END MAIN

FUNCTION obtiene_info_rpt()

   -- Record total global
   DEFINE r_op14  RECORD 
      id_cre_acreditado  DECIMAL(9,0),
      id_derechohabiente DECIMAL(9,0),
      nss                CHAR(11),
      apor_viv97         DECIMAL(15,0),
      per_pago           CHAR(6),
      estado             SMALLINT,
      tpo_transferencia  CHAR(2),
      afore              CHAR(3)
   END RECORD

   DEFINE a              INTEGER
   DEFINE v_ax_aivs97    DECIMAL(16,6)
   DEFINE v_ax_ap_viv97  DECIMAL(13,2)
   DEFINE v_precio_fondo DECIMAL(19,14)

   ##################################
   # RECUPERA INFORMACIÓN PROCESADA #
   ##################################

   SELECT UNIQUE f_movimiento
     INTO v_f_movimiento
     FROM safre_tmp:tmp_no_atend_det_uso
    WHERE f_movimiento IS NOT NULL;
    
   SELECT precio_fondo
     INTO v_precio_fondo
     FROM glo_valor_fondo
    WHERE f_valuacion = v_f_movimiento
      AND fondo = 11;
   
   # Operación 16-MARCA-->tpo_transferencia = 16
   DECLARE crs_op14_16 CURSOR FOR 
   SELECT his.id_cre_acreditado,
           acr.id_derechohabiente,
           afi.nss,
           tmp.apor_viv97,
           tmp.per_pago,
           his.estado,
           his.tpo_transferencia,
           tmp.cve_ent_cede
     FROM cre_his_acreditado his,
          cre_acreditado acr,
          afi_derechohabiente afi,
          safre_tmp:tmp_no_atend_det_uso tmp
    WHERE his.id_cre_acreditado  = acr.id_cre_acreditado
      AND his.edo_procesar       = 110 --NO ATENDIDAS
      AND acr.id_derechohabiente = afi.id_derechohabiente
      AND afi.nss                = tmp.nss
      AND his.id_cre_ctr_archivo = v_d_id_cre_ctr_arch
      AND his.tpo_transferencia  = "16";
   
   INITIALIZE r_op14.* TO NULL
   LET v_ax_ap_viv97 = 0
   LET v_ax_aivs97   = 0
   
   FOREACH crs_op14_16 INTO r_op14.id_cre_acreditado,
                             r_op14.id_derechohabiente,
                             r_op14.nss,
                             r_op14.apor_viv97,
                             r_op14.per_pago,
                             r_op14.estado,
                             r_op14.tpo_transferencia,
                             r_op14.afore

      --Pesos 97
      LET v_ax_ap_viv97 = (r_op14.apor_viv97 / 100)

      --Calcula IVS97
      LET v_ax_aivs97 = (v_ax_ap_viv97 / v_precio_fondo)
      
      -- Guarda en temporal para el archivo de salida
      INSERT INTO safre_tmp:tmp_op14_grt(
                                id_cre_acreditado ,
                                id_derechohabiente,
                                nss               ,
                                aivs97            ,
                                pesos97           ,
                                per_pago          ,
                                estado            ,
                                tpo_transferencia ,
                                afore)
                       VALUES (r_op14.id_cre_acreditado,
                                r_op14.id_derechohabiente,
                                r_op14.nss,
                                v_ax_aivs97,
                                v_ax_ap_viv97,
                                r_op14.per_pago,
                                r_op14.estado,
                                r_op14.tpo_transferencia,
                                r_op14.afore);

   END FOREACH 

   # Operación 18 y 48 tpo_transferencia (18,48)
   DECLARE crs_op14_18 CURSOR FOR 
   SELECT uso.id_cre_uso_garantia,
           uso.id_derechohabiente,
           afi.nss,
           uso.importe_v97, --ya está calculado en cre_uso_garantia
           uso.periodo_pago,
           uso.estado,
           uso.tpo_transferencia,
           tmp.cve_ent_cede
     FROM cre_uso_garantia uso,
          afi_derechohabiente afi,
          safre_tmp:tmp_no_atend_det_uso tmp
    WHERE uso.edo_procesar      = 110  --NO ATENDIDAS
      AND uso.id_derechohabiente = afi.id_derechohabiente
      AND afi.nss = tmp.nss
      AND uso.tpo_transferencia IN ("18","48")
      AND uso.id_cre_ctr_archivo = v_d_id_cre_ctr_arch
     
   INITIALIZE r_op14.* TO NULL
   LET v_ax_ap_viv97 = 0
   LET v_ax_aivs97   = 0

   FOREACH crs_op14_18 INTO r_op14.id_cre_acreditado,
                             r_op14.id_derechohabiente,
                             r_op14.nss,
                             v_ax_ap_viv97,
                             r_op14.per_pago,
                             r_op14.estado,
                             r_op14.tpo_transferencia,
                             r_op14.afore

      --Calcula IVS97
      LET v_ax_aivs97 = (v_ax_ap_viv97 / v_precio_fondo)
      
      -- Guarda en temporal para el archivo de salida
      INSERT INTO safre_tmp:tmp_op14_grt(
                                id_cre_acreditado ,
                                id_derechohabiente,
                                nss               ,
                                aivs97            ,
                                pesos97           ,
                                per_pago          ,
                                estado            ,
                                tpo_transferencia ,
                                afore)
                       VALUES (r_op14.id_cre_acreditado,
                                r_op14.id_derechohabiente,
                                r_op14.nss,
                                v_ax_aivs97,
                                v_ax_ap_viv97,
                                r_op14.per_pago,
                                r_op14.estado,
                                r_op14.tpo_transferencia,
                                r_op14.afore);

   END FOREACH 
   
   ##################################
   #   INFORMACIÓN PARA EL REPORTE  #
   ##################################

   --Inicializa variables en caso de no obtener información
   --Total global
   LET r_total_global.t_registros = 0
   LET r_total_global.aivs97      = 0
   LET r_total_global.pesos97     = 0
   --Total Sol Marca
   LET r_total_marca.t_registros = 0
   LET r_total_marca.aivs97      = 0
   LET r_total_marca.pesos97     = 0
   LET r_total_marca.porc_concatena = "0.0 %"
   --Total Sol Saldo
   LET r_total_saldo.t_registros = 0
   LET r_total_saldo.aivs97      = 0
   LET r_total_saldo.pesos97     = 0
   LET r_total_saldo.porc_concatena = "0.0 %"
   --Arreglo para MARCA
   LET arr_marca[1].estado         = NULL
   LET arr_marca[1].estado_desc    = NULL 
   LET arr_marca[1].t_registros    = 0
   LET arr_marca[1].aivs97         = 0
   LET arr_marca[1].pesos97        = 0
   LET arr_marca[1].porc_concatena = "0.0 %"
   --Arreglo para SALDO
   LET arr_saldo[1].estado         = NULL
   LET arr_saldo[1].estado_desc    = NULL 
   LET arr_saldo[1].t_registros    = 0
   LET arr_saldo[1].aivs97         = 0
   LET arr_saldo[1].pesos97         = 0
   LET arr_saldo[1].porc_concatena = "0.0 %"

   --Info para la solicitud de marca
   DECLARE crs_marca CURSOR FOR 
   SELECT cat.estado,
           cat.estado_desc,
           SUM(tmp.aivs97),
           SUM(tmp.pesos97),
           COUNT(*)
     FROM safre_tmp:tmp_op14_grt tmp,
          cat_maq_credito cat
   WHERE tmp.estado = cat.estado
     AND tmp.tpo_transferencia = "16"
     GROUP BY 1,2;

   LET a  = 1
   
   FOREACH crs_marca INTO arr_marca[a].estado,
                           arr_marca[a].estado_desc,
                           arr_marca[a].aivs97,
                           arr_marca[a].pesos97,
                           arr_marca[a].t_registros
   
      --Incrementa total global
      LET r_total_global.t_registros = r_total_global.t_registros + arr_marca[a].t_registros
      LET r_total_global.aivs97      = r_total_global.aivs97 + arr_marca[a].aivs97
      LET r_total_global.pesos97     = r_total_global.pesos97 + arr_marca[a].pesos97

      --Incremente total para marcas
      LET r_total_marca.t_registros = r_total_marca.t_registros + arr_marca[a].t_registros
      LET r_total_marca.aivs97      = r_total_marca.aivs97 + arr_marca[a].aivs97
      LET r_total_marca.pesos97      = r_total_marca.pesos97 + arr_marca[a].pesos97
   
      LET a = a + 1
      
   END FOREACH 

   --Elimina fila en blanco del arreglo
   IF(arr_marca[arr_marca.getLength()].estado IS NULL) THEN
      CALL arr_marca.deleteElement(arr_marca.getLength()) 
   END IF 

   --Info solicitud de saldo
   DECLARE crs_saldo CURSOR FOR 
   SELECT cat.estado,
           cat.estado_desc,
           SUM(tmp.aivs97),
           SUM(tmp.pesos97),
           COUNT(*)
     FROM safre_tmp:tmp_op14_grt tmp,
          cat_maq_credito cat
   WHERE tmp.estado = cat.estado
     AND tmp.tpo_transferencia <> "16"
     GROUP BY 1,2;

   LET a  = 1

   FOREACH crs_saldo INTO arr_saldo[a].estado,
                           arr_saldo[a].estado_desc,
                           arr_saldo[a].aivs97,
                           arr_saldo[a].aivs97,
                           arr_saldo[a].t_registros

      --Incrementa total global
      LET r_total_global.t_registros = r_total_global.t_registros + arr_saldo[a].t_registros
      LET r_total_global.aivs97      = r_total_global.aivs97 + arr_saldo[a].aivs97
      LET r_total_global.pesos97     = r_total_global.pesos97 + arr_saldo[a].pesos97

      --Incremente total Saldo
      LET r_total_saldo.t_registros = r_total_saldo.t_registros + arr_saldo[a].t_registros
      LET r_total_saldo.aivs97      = r_total_saldo.aivs97 + arr_saldo[a].aivs97
      LET r_total_saldo.pesos97     = r_total_saldo.pesos97 + arr_saldo[a].pesos97
   
      LET a = a + 1
      
   END FOREACH 

   --Elimina fila en blanco del arreglo
   IF(arr_saldo[arr_saldo.getLength()].estado IS NULL) THEN
      CALL arr_saldo.deleteElement(arr_saldo.getLength()) 
   END IF 
     
   #PORCENTAJES
   LET v_aux_porcentaje = 0

   --prc marcas
   LET v_aux_porcentaje = (r_total_marca.t_registros / r_total_marca.t_registros) * 100
   LET r_total_marca.porc_concatena = v_aux_porcentaje CLIPPED,"%"

   --prc saldos
   LET v_aux_porcentaje = (r_total_saldo.t_registros / r_total_saldo.t_registros) * 100
   LET r_total_saldo.porc_concatena = v_aux_porcentaje CLIPPED,"%"
   
END FUNCTION 

#OBJETIVO: Genera el reporte de Rechazos
REPORT genera_PDF()

   DEFINE v_f_presenta     DATE
   DEFINE v_desc_operacion CHAR(60)
   DEFINE f                INTEGER
   DEFINE r_rch_integra    RECORD
      nss         CHAR(11),
      estado      SMALLINT,
      estado_desc CHAR(40) 
   END RECORD 

   FORMAT 
   FIRST PAGE HEADER
      LET v_f_presenta = TODAY
      LET v_desc_operacion = "Recepción No Atendidas Solicitud Saldo GRT"  

      #ENCABEZADO
      PRINTX p_v_usuario
      PRINTX p_d_folio
      PRINTX v_f_presenta USING "dd-mm-yyyy"
      PRINTX v_f_movimiento USING "dd-mm-yyyy" 
      
      #RESUMEN
      PRINTX p_v_arch_proceso            --Nombre del archivo
      PRINTX v_r_bat_ctr_opera.fecha_ini --fecha inicio proceso
      PRINTX v_r_bat_ctr_opera.fecha_fin --Fecha fin del proceso
      PRINTX v_desc_operacion            --Desc. operación
      #Totales globales
      PRINTX r_total_global.t_registros
      PRINTX r_total_global.aivs97       
      PRINTX r_total_global.pesos97
      #Totales TA
      PRINTX r_total_marca.t_registros
      PRINTX r_total_marca.aivs97       
      PRINTX r_total_marca.pesos97 
      PRINTX r_total_marca.porc_concatena
      #Totales AG
      PRINTX r_total_saldo.t_registros
      PRINTX r_total_saldo.aivs97       
      PRINTX r_total_saldo.pesos97       
      PRINTX r_total_saldo.porc_concatena 
   
   ON EVERY ROW
      #Detalle TA
      FOR f=1 TO arr_marca.getLength()
         PRINTX arr_marca[f].estado_desc  
         PRINTX arr_marca[f].t_registros       
         PRINTX arr_marca[f].aivs97        
         PRINTX arr_marca[f].pesos97   
         #calcula porcentaje
         LET v_aux_porcentaje = 0
         LET v_aux_porcentaje = (arr_marca[f].t_registros / r_total_marca.t_registros) * 100
         LET arr_marca[f].porc_concatena = v_aux_porcentaje CLIPPED,"%"
         PRINTX arr_marca[f].porc_concatena
      END FOR 

      #Detalle AG
      FOR f=1 TO arr_saldo.getLength()
         PRINTX arr_saldo[f].estado_desc  
         PRINTX arr_saldo[f].t_registros       
         PRINTX arr_saldo[f].aivs97        
         PRINTX arr_saldo[f].pesos97   
         #calcula porcentaje
         LET v_aux_porcentaje = 0
         LET v_aux_porcentaje = (arr_saldo[f].t_registros / r_total_saldo.t_registros) * 100
         LET arr_saldo[f].porc_concatena = v_aux_porcentaje CLIPPED,"%"
         PRINTX arr_saldo[f].porc_concatena
      END FOR 
      
      #Detalle rechazos que ocurrió en la integración
      DECLARE crs_rch_integra CURSOR FOR 
      SELECT r.nss,
              r.estado,
              c.desc_estado
        FROM cre_rch_acreditado r,
              cat_rch_acreditado c
       WHERE r.id_cre_ctr_archivo = v_d_id_cre_ctr_arch
         AND r.estado = c.estado 

      FOREACH crs_rch_integra INTO  r_rch_integra.nss,
                                     r_rch_integra.estado,
                                     r_rch_integra.estado_desc
            PRINTX r_rch_integra.nss
            PRINTX r_rch_integra.estado_desc
      END FOREACH 

END REPORT

#OBJETIVO: Genera archivo de salida detalle rechazos.
FUNCTION genera_archivo_salida()

   DEFINE v_qry_salida    STRING
   DEFINE v_detalle       STRING
   DEFINE v_ruta_archivo  STRING 
   DEFINE archivo         base.Channel
   
   DEFINE r_arh_op14     RECORD
      nss          CHAR(11),
      aivs97       DECIMAL(13,2),
      pesos97      DECIMAL(13,2),
      periodo_pago CHAR(6),
      estado       SMALLINT,
      tipo         CHAR(2),
      num_intento  CHAR(4),
      tpo_transf   CHAR(2),
      afore        CHAR(3)
   END RECORD  

   LET v_arh_salida   = "Detalle_43Bis14_",TODAY USING "yyyymmdd",".cgt" CLIPPED 
   LET v_ruta_archivo = v_ruta_envio CLIPPED,"/",v_arh_salida

   #Recupera información de la temporal

   LET v_qry_salida = "SELECT nss,
                              aivs97,
                              pesos97,
                              per_pago,
                              estado,
                              tpo_transferencia,
                              afore
                         FROM safre_tmp:tmp_op14_grt;"
                            
      
   PREPARE prp_arh_op14 FROM v_qry_salida
   DECLARE crs_arh_op14 CURSOR FOR prp_arh_op14

   LET archivo = base.Channel.create()
   CALL archivo.openFile(v_ruta_archivo,"w")

   LET r_arh_op14.nss          = NULL
   LET r_arh_op14.aivs97       = 0
   LET r_arh_op14.pesos97      = 0 
   LET r_arh_op14.estado       = NULL
   LET r_arh_op14.tipo         = NULL
   LET r_arh_op14.num_intento  ="   1"
   LET r_arh_op14.periodo_pago = NULL 
   LET r_arh_op14.tpo_transf   = NULL
   LET r_arh_op14.afore        = NULL 
   LET v_detalle               = NULL
   
   FOREACH crs_arh_op14 INTO r_arh_op14.nss,
                              r_arh_op14.aivs97,
                              r_arh_op14.pesos97,
                              r_arh_op14.periodo_pago,
                              r_arh_op14.estado,
                              r_arh_op14.tpo_transf,
                              r_arh_op14.afore
      
      CASE 
         WHEN  r_arh_op14.estado = 20
            LET r_arh_op14.tipo  = "UG" 
         WHEN  r_arh_op14.estado = 140 
            LET r_arh_op14.tipo  = "CA"
         WHEN  r_arh_op14.estado = 142
            LET r_arh_op14.tipo  = "CE"
      END CASE

      LET v_detalle = v_r_cre_ctr_arch.f_proceso USING "yyyymmdd",
                      r_arh_op14.nss,
                      r_arh_op14.aivs97,
                      r_arh_op14.pesos97,
                      r_arh_op14.periodo_pago,
                      r_arh_op14.tipo,
                      r_arh_op14.num_intento,
                      r_arh_op14.tpo_transf,
                      r_arh_op14.afore

      --Escribe en archivo de salida
      CALL archivo.writeLine(v_detalle)
      
   END FOREACH

   CALL archivo.close()
   
END FUNCTION 

FUNCTION crea_temporal()

   DATABASE safre_tmp
   
   WHENEVER ERROR CONTINUE 
      DROP TABLE tmp_op14_grt
   WHENEVER ERROR STOP
      CREATE TABLE tmp_op14_grt(id_cre_acreditado  DECIMAL(9,0),
                                 id_derechohabiente DECIMAL(9,0),
                                 nss                CHAR(11),
                                 aivs97             DECIMAL(16,6),
                                 pesos97            DECIMAL(13,2),
                                 per_pago           CHAR(6),
                                 estado             SMALLINT,
                                 tpo_transferencia  CHAR(2),
                                 afore              CHAR(3));
   DATABASE safre_viv

END FUNCTION 