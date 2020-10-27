--===============================================================
-- Versión: 1.0.0
-- Fecha última modificación:
--===============================================================

#########################################################################
#Modulo            =>ACR                                                #
#Programa          =>ACRP28                                             #
#Objetivo          =>Programa que realiza la conciliación de la infor-  #
#                    mación de deudor vs tmp deudor no atendidas ACR    #
#Autor             =>Daniel Buendia, EFP                                #
#Fecha inicio      =>02 Febrero 2012                                    #
#Autor modifica    =>Emilio Abarca, EFP.                                #
#Fecha modifica    =>14 Mayo 2018.                                      #
#Objetivo modifica =>Adecuación a reporte PDF y generación archivo de   #
#                    de salida rechazos Operación 14 ACR.               #
#########################################################################

DATABASE safre_viv

GLOBALS "ACRG10.4gl"


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

   #RECORDS Y ARREGLOS PARA LA INFORMACIÓN DEL REPORTE
   DEFINE r_total_global    RECORD
      t_registros     INTEGER,
      aivs92          DECIMAL(20,2),
      aivs97          DECIMAL(20,2)
   END RECORD

   DEFINE r_total_ta    RECORD
      t_registros     INTEGER,
      aivs92          DECIMAL(20,2),
      aivs97          DECIMAL(20,2),
      porc_concatena  CHAR(12)
   END RECORD

   DEFINE r_total_ag    RECORD
      t_registros     INTEGER,
      aivs92          DECIMAL(20,2),
      aivs97          DECIMAL(20,2),
      porc_concatena  CHAR(12)
   END RECORD

   DEFINE arr_ta DYNAMIC ARRAY OF RECORD 
      estado          SMALLINT,
      estado_desc     CHAR(40),
      t_registros     INTEGER,
      aivs92          DECIMAL(20,2),
      aivs97          DECIMAL(20,2),
      porc_concatena  CHAR(12)
   END RECORD

   DEFINE arr_ag DYNAMIC ARRAY OF RECORD 
      estado          SMALLINT,
      estado_desc     CHAR(40),
      t_registros     INTEGER,
      aivs92          DECIMAL(20,2),
      aivs97          DECIMAL(20,2),
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
   CALL STARTLOG(p_v_usuario CLIPPED|| ".ACRP28.log")

   DISPLAY " "
   DISPLAY "=INICIA ACRP28="
   DISPLAY " CONCILIACIÓN DE SOLICITUDES NO ATENDIDAS ACR"
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"
   DISPLAY " ARCHIVO       : ",p_v_arch_proceso
   DISPLAY " "

   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'acr';
    
   -- Se inicializan variables
   LET v_si_operacion = 14 -- Solicitudes no atendidas

   -- se obtiene el nombrel del programa correspondiente
   LET v_c_programa_cod = fn_obten_nom_programa(p_i_proceso_cod , p_i_opera_cod)

   -- recupera la ruta de listados en el que se enviara el archivo
   CALL fn_rutas("acr") RETURNING v_ruta_bin, v_ruta_listados

    -- se consulta el folio del archivo
   LET v_s_qryTxt = " SELECT MAX(id_cre_ctr_archivo)\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE id_proceso = ",g_id_proceso_acr,"\n",
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
   
   LET v_d_pid_arch = fn_max_pid(g_proc_cod_acr_no_atendidas, 2)

   DISPLAY " PID del archivo: ",v_d_pid_arch
   DISPLAY " "

   CALL crea_temporal()

   CALL obtiene_info_rpt()
   DISPLAY " > OBTIENE INFORMACIÓN PARA EL REPORTE PDF ...COMPLETADO"
   DISPLAY ""
   CALL genera_archivo_salida()
   DISPLAY " > GENERA ARCHIVO DE SALIDA ...COMPLETADO"
   DISPLAY "  El archivo de salida de ha generado en /safreviv_int/acr/envio"
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
                    "    AND proceso_cod = ",g_proc_cod_acr_no_atendidas,"\n",
                    "    AND opera_cod = 2"

   PREPARE prp_bat_ctr_opera FROM v_s_qryTxt
   EXECUTE prp_bat_ctr_opera INTO v_r_bat_ctr_opera.*

   #################################################
   #   CONFIGURACIÓN PARA SALIDA DEL REPORTE PDF   #
   #################################################   

   LET v_reporte_bin = v_ruta_bin CLIPPED,"/ACRP281.4rp"
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
  
   DISPLAY " "
   DISPLAY " ENVIA CORREO DEL REPORTE"
   DISPLAY " "
  
   -- se asigna el titulo del correo
   LET v_s_titulo_correo = "Proceso: CONCILIACIÓN DE SOLICITUDES NO ATENDIDAS ACREDITADOS"
  
   -- se asigna el archivo a adjuntar
   LET v_s_archivo_correo = v_ruta_rpt
  
   -- se asigna el cuerpo del correo
   LET v_s_mens_correo =  "ID Proceso   : ",p_d_pid,"\n",
                          "Proceso      : CONCILIACIÓN\n",
                          "Operacion    : SOLICITUDES NO ATENDIDAS ACR\n",
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
   DEFINE r_op14_ta  RECORD 
      id_cre_acreditado  DECIMAL(9,0),
      id_derechohabiente DECIMAL(9,0),
      nss                CHAR(11),
      estado             SMALLINT,
      afore              CHAR(3),
      tpo_transferencia  CHAR(2),
      periodo_pago       CHAR(6) 
   END RECORD

   DEFINE r_op14_ag  RECORD 
      id_cre_acreditado  DECIMAL(9,0),
      id_derechohabiente DECIMAL(9,0),
      nss                CHAR(11),
      estado             SMALLINT,
      afore              CHAR(3),
      tpo_transferencia  CHAR(2),
      periodo_pago       CHAR(6),
      acciones92         DECIMAL(20,2),
      acciones97         DECIMAL(20,2)
   END RECORD

   DEFINE r_saldo_92    RECORD
      subcuenta          SMALLINT,
      fondo_inversion    SMALLINT,
      acciones           DECIMAL(20,2),
      pesos              DECIMAL(20,2)
   END RECORD

   DEFINE r_saldo_97             RECORD
      subcuenta          SMALLINT,
      fondo_inversion    SMALLINT,
      acciones           DECIMAL(20,2),
      pesos              DECIMAL(20,2)
   END RECORD

   DEFINE v_acciones92   DECIMAL(20,2)
   DEFINE v_pesos92      DECIMAL(20,2)
   DEFINE v_acciones97   DECIMAL(20,2)
   DEFINE v_pesos97      DECIMAL(20,2)
   DEFINE a              INTEGER
   DEFINE v_precio_fondo DECIMAL(19,14)

   ##################################
   # RECUPERA INFORMACIÓN PROCESADA #
   ##################################

   --Obtenemos la fecha de movimiento
   LET v_f_movimiento = TODAY 

   SELECT precio_fondo
     INTO v_precio_fondo
     FROM glo_valor_fondo
    WHERE f_valuacion = TODAY
      AND fondo = 11;
   

   # TRANSFERENCIA DE ACREDITADOS (TA)
   DECLARE crs_op14_ta CURSOR FOR 
   SELECT his.id_cre_acreditado,
           acr.id_derechohabiente,
           afi.nss,
           his.estado,
           tmp.cve_ent_ceden,
           his.tpo_transferencia,
           tmp.periodo_pago
     FROM cre_his_acreditado his,
          cre_acreditado acr,
          afi_derechohabiente afi,
          safre_tmp:tmp_acr_no_at tmp
    WHERE his.id_cre_acreditado  = acr.id_cre_acreditado
      AND his.edo_procesar       = 110 --NO ATENDIDAS
      AND acr.id_derechohabiente = afi.id_derechohabiente
      AND afi.nss                = tmp.nss_infonavit
      AND his.id_cre_ctr_archivo = v_d_id_cre_ctr_arch
      AND his.tpo_transferencia  = "03";

   INITIALIZE r_op14_ta.* TO NULL
   
   FOREACH crs_op14_ta INTO r_op14_ta.id_cre_acreditado,
                             r_op14_ta.id_derechohabiente,
                             r_op14_ta.nss,
                             r_op14_ta.estado,
                             r_op14_ta.afore,
                             r_op14_ta.tpo_transferencia,
                             r_op14_ta.periodo_pago
      
      --Inicializa variables
      LET v_acciones92 = 0 
      LET v_pesos92    = 0
      LET v_acciones97 = 0 
      LET v_pesos97    = 0
      
      --Obtiene aivs 92 
     
      PREPARE prp_saldo_92 FROM "EXECUTE FUNCTION fn_Saldo_actual(?,8,TODAY)"
      DECLARE crs_saldo_92 CURSOR FOR prp_saldo_92

      FOREACH crs_saldo_92 USING r_op14_ta.nss 
                            INTO r_saldo_92.subcuenta,
                                 r_saldo_92.fondo_inversion,
                                 r_saldo_92.acciones,
                                 r_saldo_92.pesos
                                 
         IF(r_saldo_92.fondo_inversion = 11) THEN
            LET v_acciones92 = r_saldo_92.acciones
            LET v_pesos92    = r_saldo_92.pesos
            EXIT FOREACH
         END IF
         
      END FOREACH 
      
      PREPARE prp_saldo_97 FROM "EXECUTE FUNCTION fn_Saldo_actual(?,4,TODAY)"
      DECLARE crs_saldo_97 CURSOR FOR prp_saldo_97

      FOREACH crs_saldo_97 USING r_op14_ta.nss 
                            INTO r_saldo_97.subcuenta,
                                 r_saldo_97.fondo_inversion,
                                 r_saldo_97.acciones,
                                 r_saldo_97.pesos
                                 
         IF(r_saldo_97.fondo_inversion = 11) THEN
            LET v_acciones97 = r_saldo_97.acciones
            LET v_pesos97    = r_saldo_97.pesos
            EXIT FOREACH
         END IF
         
      END FOREACH 

      -- Guarda en temporal
      INSERT INTO safre_tmp:tmp_op14_acr(
                                id_cre_acreditado ,
                                id_derechohabiente,
                                nss               ,
                                aivs92            ,
                                pesos92           ,
                                aivs97            ,
                                pesos97           ,
                                estado            ,
                                afore             ,
                                tpo_transferencia ,
                                periodo_pago)
                       VALUES (r_op14_ta.id_cre_acreditado,
                                r_op14_ta.id_derechohabiente,
                                r_op14_ta.nss,
                                v_acciones92,
                                v_pesos92,
                                v_acciones97,
                                v_pesos97,
                                r_op14_ta.estado,
                                r_op14_ta.afore,
                                r_op14_ta.tpo_transferencia,
                                r_op14_ta.periodo_pago);
                                
   END FOREACH 

   # ANUALIDADES GARANTIZADAS (AG)
   DECLARE crs_op14_ag CURSOR FOR 
   SELECT his.id_cre_acreditado,
           sal.id_derechohabiente,
           sal.nss,
           his.estado,
           tmp.cve_ent_ceden,
           his.tpo_transferencia,
           tmp.periodo_pago,
           sal.aivs92,
           sal.aivs97
     FROM cre_his_acreditado his,
          safre_tmp:tmp_agr_solic_sdo2 sal,
          safre_tmp:tmp_acr_no_at tmp
    WHERE his.id_cre_acreditado  = sal.id_referencia
      AND his.edo_procesar       = 110 --NO ATENDIDAS
      AND sal.nss                = tmp.nss_infonavit
      AND his.id_cre_ctr_archivo = v_d_id_cre_ctr_arch
      AND his.tpo_transferencia  = "43";

   INITIALIZE r_op14_ag.* TO NULL 
   
   LET v_pesos92    = 0
   LET v_pesos97    = 0
   
   FOREACH crs_op14_ag INTO r_op14_ag.id_cre_acreditado,
                             r_op14_ag.id_derechohabiente,
                             r_op14_ag.nss,
                             r_op14_ag.estado,
                             r_op14_ag.afore,
                             r_op14_ag.tpo_transferencia,
                             r_op14_ag.periodo_pago,
                             r_op14_ag.acciones92,
                             r_op14_ag.acciones97

      IF(r_op14_ag.acciones92 IS NULL) THEN
         LET r_op14_ag.acciones92 = 0
      END IF 

      IF(r_op14_ag.acciones97 IS NULL) THEN
         LET r_op14_ag.acciones97 = 0
      END IF 
      
      --Calcula pesos
      LET v_pesos92 = (r_op14_ag.acciones92 * v_precio_fondo)
      LET v_pesos97 = (r_op14_ag.acciones97 * v_precio_fondo)

       -- Guarda en temporal
      INSERT INTO safre_tmp:tmp_op14_acr(
                                id_cre_acreditado ,
                                id_derechohabiente,
                                nss               ,
                                aivs92            ,
                                pesos92           ,
                                aivs97            ,
                                pesos97           ,
                                estado            ,
                                afore             ,
                                tpo_transferencia ,
                                periodo_pago)
                       VALUES (r_op14_ag.id_cre_acreditado,
                                r_op14_ag.id_derechohabiente,
                                r_op14_ag.nss,
                                r_op14_ag.acciones92,
                                v_pesos92,
                                r_op14_ag.acciones97,
                                v_pesos97,
                                r_op14_ag.estado,
                                r_op14_ag.afore,
                                r_op14_ag.tpo_transferencia,
                                r_op14_ag.periodo_pago);
                                
      
   END FOREACH 

   # ANUALIDADES GARANTIZADAS (UA)
   DECLARE crs_op14_ua CURSOR FOR
   SELECT ug.id_cre_uso_garantia,
           ug.id_derechohabiente,
           sal.nss,
           ug.estado,
           tmp.cve_ent_ceden,
           ug.tpo_transferencia,
           tmp.periodo_pago,
           sal.aivs92,
           sal.aivs97
     FROM cre_uso_garantia ug,
           safre_tmp:tmp_agr_solic_sdo_ua sal,
           safre_tmp:tmp_acr_no_at tmp
    WHERE ug.edo_procesar       = 110  -- NO ATENDIDAS
      AND ug.id_cre_ctr_archivo = v_d_id_cre_ctr_arch
      AND ug.id_derechohabiente = sal.id_derechohabiente
      AND sal.nss               = tmp.nss_infonavit;


   INITIALIZE r_op14_ag.* TO NULL 
   
   LET v_pesos92    = 0
   LET v_pesos97    = 0

   FOREACH crs_op14_ua INTO r_op14_ag.id_cre_acreditado,
                             r_op14_ag.id_derechohabiente,
                             r_op14_ag.nss,
                             r_op14_ag.estado,
                             r_op14_ag.afore,
                             r_op14_ag.tpo_transferencia,
                             r_op14_ag.periodo_pago,
                             r_op14_ag.acciones92,
                             r_op14_ag.acciones97

      IF(r_op14_ag.acciones92 IS NULL) THEN
         LET r_op14_ag.acciones92 = 0
      END IF 

      IF(r_op14_ag.acciones97 IS NULL) THEN
         LET r_op14_ag.acciones97 = 0
      END IF 
      
      --Calcula pesos
      LET v_pesos92 = (r_op14_ag.acciones92 * v_precio_fondo)
      LET v_pesos97 = (r_op14_ag.acciones97 * v_precio_fondo)

       -- Guarda en temporal
      INSERT INTO safre_tmp:tmp_op14_acr(
                                id_cre_acreditado ,
                                id_derechohabiente,
                                nss               ,
                                aivs92            ,
                                pesos92           ,
                                aivs97            ,
                                pesos97           ,
                                estado            ,
                                afore             ,
                                tpo_transferencia ,
                                periodo_pago)
                       VALUES (r_op14_ag.id_cre_acreditado,
                                r_op14_ag.id_derechohabiente,
                                r_op14_ag.nss,
                                r_op14_ag.acciones92,
                                v_pesos92,
                                r_op14_ag.acciones97,
                                v_pesos97,
                                r_op14_ag.estado,
                                r_op14_ag.afore,
                                r_op14_ag.tpo_transferencia,
                                r_op14_ag.periodo_pago);
                                
      
   END FOREACH 
   
   ##################################
   #   INFORMACIÓN PARA EL REPORTE  #
   ##################################
   
   --Total global
   LET r_total_global.t_registros = 0
   LET r_total_global.aivs92      = 0
   LET r_total_global.aivs97      = 0
   --Total TA
   LET r_total_ta.t_registros = 0
   LET r_total_ta.aivs92      = 0
   LET r_total_ta.aivs97      = 0
   LET r_total_ta.porc_concatena = "0.0 %"
   --Total AG
   LET r_total_ag.t_registros = 0
   LET r_total_ag.aivs92      = 0
   LET r_total_ag.aivs97      = 0
   LET r_total_ag.porc_concatena = "0.0 %"
   --Arreglo para TA
   LET arr_ta[1].estado         = NULL
   LET arr_ta[1].estado_desc    = NULL 
   LET arr_ta[1].t_registros    = 0
   LET arr_ta[1].aivs92         = 0
   LET arr_ta[1].aivs97         = 0
   LET arr_ta[1].porc_concatena = "0.0 %"
   --Arreglo para AG
   LET arr_ag[1].estado         = NULL
   LET arr_ag[1].estado_desc    = NULL 
   LET arr_ag[1].t_registros    = 0
   LET arr_ag[1].aivs92         = 0
   LET arr_ag[1].aivs97         = 0
   LET arr_ag[1].porc_concatena = "0.0 %"

   --Info para TA de la temporal
   DECLARE crs_ta CURSOR FOR 
   SELECT cat.estado,
           cat.estado_desc,
           SUM(tmp.aivs92),
           SUM(tmp.aivs97),
           COUNT(*)
     FROM safre_tmp:tmp_op14_acr tmp,
          cat_maq_credito cat
   WHERE tmp.estado = cat.estado
     AND tmp.tpo_transferencia =  "03"
     GROUP BY 1,2;

   LET a  = 1
   
   FOREACH crs_ta INTO arr_ta[a].estado,
                        arr_ta[a].estado_desc,
                        arr_ta[a].aivs92,
                        arr_ta[a].aivs97,
                        arr_ta[a].t_registros

      --Incrementa total global
      LET r_total_global.t_registros = r_total_global.t_registros + arr_ta[a].t_registros
      LET r_total_global.aivs92      = r_total_global.aivs92 + arr_ta[a].aivs92
      LET r_total_global.aivs97      = r_total_global.aivs97 + arr_ta[a].aivs97

      --Incremente total TA
      LET r_total_ta.t_registros = r_total_ta.t_registros + arr_ta[a].t_registros
      LET r_total_ta.aivs92      = r_total_ta.aivs92 + arr_ta[a].aivs92
      LET r_total_ta.aivs97      = r_total_ta.aivs97 + arr_ta[a].aivs97
   
      LET a = a + 1
      
   END FOREACH 

   --Elimina fila en blanco del arreglo
   IF(arr_ta[arr_ta.getLength()].estado IS NULL) THEN
      CALL arr_ta.deleteElement(arr_ta.getLength()) 
   END IF 

   --Info para AG de la temporal
   DECLARE crs_ag CURSOR FOR 
   SELECT cat.estado,
           cat.estado_desc,
           SUM(tmp.aivs92),
           SUM(tmp.aivs97),
           COUNT(*)
     FROM safre_tmp:tmp_op14_acr tmp,
          cat_maq_credito cat
   WHERE tmp.estado = cat.estado
     AND tmp.tpo_transferencia <> "03"
     GROUP BY 1,2;

   LET a  = 1

   FOREACH crs_ag INTO arr_ag[a].estado,
                        arr_ag[a].estado_desc,
                        arr_ag[a].aivs92,
                        arr_ag[a].aivs97,
                        arr_ag[a].t_registros

      --Incrementa total global
      LET r_total_global.t_registros = r_total_global.t_registros + arr_ag[a].t_registros
      LET r_total_global.aivs92      = r_total_global.aivs92 + arr_ag[a].aivs92
      LET r_total_global.aivs97      = r_total_global.aivs97 + arr_ag[a].aivs97

      --Incremente total TA
      LET r_total_ag.t_registros = r_total_ag.t_registros + arr_ag[a].t_registros
      LET r_total_ag.aivs92      = r_total_ag.aivs92 + arr_ag[a].aivs92
      LET r_total_ag.aivs97      = r_total_ag.aivs97 + arr_ag[a].aivs97
   
      LET a = a + 1
      
   END FOREACH 

   --Elimina fila en blanco del arreglo
   IF(arr_ta[arr_ta.getLength()].estado IS NULL) THEN
      CALL arr_ta.deleteElement(arr_ta.getLength()) 
   END IF 
     
   #PORCENTAJES
   LET v_aux_porcentaje = 0

   --prc total global
   LET v_aux_porcentaje = (r_total_global.t_registros / r_total_global.t_registros) * 100

   --prc TA
   LET v_aux_porcentaje = (r_total_ta.t_registros / r_total_ta.t_registros) * 100
   LET r_total_ta.porc_concatena = v_aux_porcentaje CLIPPED,"%"

   --prc AG
   LET v_aux_porcentaje = (r_total_ag.t_registros / r_total_ag.t_registros) * 100
   LET r_total_ag.porc_concatena = v_aux_porcentaje CLIPPED,"%"

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
      LET v_desc_operacion = "Recepción No Atendidas ACR"  

      #ENCABEZADO
      PRINTX p_v_usuario
      PRINTX p_d_folio
      PRINTX v_f_presenta USING "dd-mm-yyyy"
      PRINTX v_f_movimiento USING "dd-mm-yyyy" 
      
      #RESUMEN
      PRINTX p_v_arch_proceso            --Nombre del archivo
      PRINTX v_r_bat_ctr_opera.fecha_ini --fecha inicio proceso
      PRINTX v_r_bat_ctr_opera.fecha_fin --fecha fin del proceso
      PRINTX v_desc_operacion            --Desc. operación
      
      #Totales globales
      PRINTX r_total_global.t_registros
      PRINTX r_total_global.aivs92       
      PRINTX r_total_global.aivs97
      #Totales TA
      PRINTX r_total_ta.t_registros
      PRINTX r_total_ta.aivs92       
      PRINTX r_total_ta.aivs97 
      PRINTX r_total_ta.porc_concatena
      #Totales AG
      PRINTX r_total_ag.t_registros
      PRINTX r_total_ag.aivs92       
      PRINTX r_total_ag.aivs97       
      PRINTX r_total_ag.porc_concatena 
   
   ON EVERY ROW
      #Detalle TA
      FOR f=1 TO arr_ta.getLength()
         PRINTX arr_ta[f].estado_desc  
         PRINTX arr_ta[f].t_registros       
         PRINTX arr_ta[f].aivs92        
         PRINTX arr_ta[f].aivs97   
         #calcula porcentaje
         LET v_aux_porcentaje = 0
         LET v_aux_porcentaje = (arr_ta[f].t_registros / r_total_ta.t_registros) * 100
         LET arr_ta[f].porc_concatena = v_aux_porcentaje CLIPPED,"%"
         PRINTX arr_ta[f].porc_concatena
      END FOR 

      #Detalle AG
      FOR f=1 TO arr_ag.getLength()
         PRINTX arr_ag[f].estado_desc  
         PRINTX arr_ag[f].t_registros       
         PRINTX arr_ag[f].aivs92        
         PRINTX arr_ag[f].aivs97   
         #calcula porcentaje
         LET v_aux_porcentaje = 0
         LET v_aux_porcentaje = (arr_ag[f].t_registros / r_total_ag.t_registros) * 100
         LET arr_ag[f].porc_concatena = v_aux_porcentaje CLIPPED,"%"
         PRINTX arr_ag[f].porc_concatena
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

   LET v_arh_salida   = "Detalle_TA14_",TODAY USING "yyyymmdd",".cta" CLIPPED 
   LET v_ruta_archivo = v_ruta_envio CLIPPED,"/",v_arh_salida

   #Recupera información de la temporal

   LET v_qry_salida = "SELECT nss,
                              aivs97,
                              pesos97,
                              periodo_pago,
                              estado,
                              tpo_transferencia,
                              afore
                         FROM safre_tmp:tmp_op14_acr;"
                            
      
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
         WHEN  r_arh_op14.estado = 20 OR r_arh_op14.estado = 140 
            LET r_arh_op14.tipo  = "DE"
         WHEN  r_arh_op14.estado = 25 OR r_arh_op14.estado = 145
            LET r_arh_op14.tipo  = "SR"
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
      DROP TABLE tmp_op14_acr
   WHENEVER ERROR STOP
      CREATE TABLE tmp_op14_acr(id_cre_acreditado  DECIMAL(9,0),
                                 id_derechohabiente DECIMAL(9,0),
                                 nss                CHAR(11),
                                 aivs92             DECIMAL(20,2),
                                 pesos92            DECIMAL(20,2),
                                 aivs97             DECIMAL(20,2),
                                 pesos97            DECIMAL(20,2),
                                 estado             SMALLINT,
                                 afore              CHAR(3),
                                 tpo_transferencia  CHAR(2),
                                 periodo_pago       CHAR(6));

   DATABASE safre_viv

END FUNCTION 
