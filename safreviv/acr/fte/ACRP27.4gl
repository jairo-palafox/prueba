--===============================================================
-- Versión: 1.0.0
-- Fecha última modificación:
--===============================================================

#########################################################################
#Modulo            =>ACR                                                #
#Programa          =>ACRP27                                             #
#Objetivo          =>Programa que realiza la conciliación de la infor-  #
#                    mación de deudor vs tmp deudor devoluciones para   #
#                    el módulo de Transferencia de Acreditados          #
#Autor             =>Daniel Buendia, EFP                                #
#Fecha inicio      =>02 Febrero 2012                                    #
#Autor modifica    =>Emilio Abarca, EFP.                                #
#Fecha modifica    =>11 Mayo 2018.                                      #
#Objetivo modifica =>Adecuación a reporte PDF y generación archivo de   #
#                    de salida rechazos Operación 06 ACR.               #
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
      aivs97          DECIMAL(20,2),
      porc_concatena  CHAR(12)
   END RECORD

   DEFINE arr_resumen DYNAMIC ARRAY OF RECORD 
      estado          SMALLINT,
      estado_desc     CHAR(40),
      t_registros     INTEGER,
      aivs92          DECIMAL(20,2),
      aivs97          DECIMAL(20,2),
      porc_concatena  CHAR(12)
   END RECORD
   DEFINE arr_det DYNAMIC ARRAY OF RECORD 
      estado          SMALLINT,
      estado_desc     CHAR(40),
      aivs97          DECIMAL(20,2),
      pesos97         DECIMAL(20,2),
      causal          CHAR(3),
      causal_desc     CHAR(100),
      t_registros     INTEGER
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
   CALL STARTLOG(p_v_usuario CLIPPED|| ".ACRP27.log")

   DISPLAY " "
   DISPLAY "=INICIA ACRP27="
   DISPLAY " CONCILIACIÓN DEVOLUCION SOLICITUDES ACR"
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
   LET v_si_operacion = 6 -- Devolución de solicitudes

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
   
   LET v_d_pid_arch = fn_max_pid(g_proc_cod_acr_devol_solic, 2)
   DISPLAY " PID del archivo: ",v_d_pid_arch

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
                    "    AND proceso_cod = ",g_proc_cod_acr_devol_solic,"\n",
                    "    AND opera_cod = 2"

   PREPARE prp_bat_ctr_opera FROM v_s_qryTxt
   EXECUTE prp_bat_ctr_opera INTO v_r_bat_ctr_opera.*

   #################################################
   #   CONFIGURACIÓN PARA SALIDA DEL REPORTE PDF   #
   #################################################   

   LET v_reporte_bin = v_ruta_bin CLIPPED,"/ACRP271.4rp"
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
   LET v_s_titulo_correo = "Proceso: CONCILIACIÓN DE DEVOLUCIÓN DE SOLICITUDES TRANSFERENCIA DE ACREDITADOS"

   -- se asigna el archivo a adjuntar
   LET v_s_archivo_correo = v_ruta_rpt

   -- se asigna el cuerpo del correo
   LET v_s_mens_correo =  "ID Proceso   : ",p_d_pid,"\n",
                          "Proceso      : CONCILIACIÓN\n",
                          "Operacion    : DEVOLUCIÓN DE SOLICITUDES ACR\n",
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
   DEFINE r_op06_dev  RECORD 
      id_cre_acreditado  DECIMAL(9,0),
      id_derechohabiente DECIMAL(9,0),
      nss                CHAR(11),
      diagnostico        CHAR(3),
      estado             SMALLINT,
      afore              CHAR(3)      
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
   DEFINE v_acciones92  DECIMAL(20,2)
   DEFINE v_pesos92     DECIMAL(20,2)
   DEFINE v_acciones97  DECIMAL(20,2)
   DEFINE v_pesos97     DECIMAL(20,2)
   DEFINE a             INTEGER
   DEFINE v_aux_diag    CHAR(3)

   ##################################
   # RECUPERA INFORMACIÓN PROCESADA #
   ##################################
   DECLARE crs_op06 CURSOR FOR 
   SELECT his.id_cre_acreditado,
           acr.id_derechohabiente,
           afi.nss,
           his.diagnostico,
           his.estado,
           tmp.cve_entidad_cedente
     FROM cre_his_acreditado his,
          cre_acreditado acr,
          afi_derechohabiente afi,
          safre_tmp:tmp_acr_devol tmp
    WHERE his.id_cre_acreditado  = acr.id_cre_acreditado
      AND his.edo_procesar       = 100 --SOLICITUDE DEVUELTA
      AND acr.id_derechohabiente = afi.id_derechohabiente
      AND afi.nss                = tmp.nss
      AND his.id_cre_ctr_archivo = v_d_id_cre_ctr_arch

   INITIALIZE r_op06_dev.* TO NULL
   LET v_aux_diag = NULL 
   
   FOREACH crs_op06 INTO r_op06_dev.id_cre_acreditado,
                          r_op06_dev.id_derechohabiente,
                          r_op06_dev.nss,
                          v_aux_diag,
                          r_op06_dev.estado,
                          r_op06_dev.afore

      LET r_op06_dev.diagnostico = v_aux_diag USING "&&&"
      
      --Inicializa variables
      LET v_acciones92 = 0 
      LET v_pesos92    = 0
      LET v_acciones97 = 0 
      LET v_pesos97    = 0
      
      --Obtiene aivs 92 
     
      PREPARE prp_saldo_92 FROM "EXECUTE FUNCTION fn_Saldo_actual(?,8,TODAY)"
      DECLARE crs_saldo_92 CURSOR FOR prp_saldo_92

      FOREACH crs_saldo_92 USING r_op06_dev.nss 
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

      FOREACH crs_saldo_97 USING r_op06_dev.nss 
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
      INSERT INTO safre_tmp:tmp_op06_acr(
                                id_cre_acreditado ,
                                id_derechohabiente,
                                nss               ,
                                aivs92            ,
                                pesos92           ,
                                aivs97            ,
                                pesos97           ,
                                diagnostico       ,
                                estado            ,
                                afore)
                       VALUES (r_op06_dev.id_cre_acreditado,
                                r_op06_dev.id_derechohabiente,
                                r_op06_dev.nss,
                                v_acciones92,
                                v_pesos92,
                                v_acciones97,
                                v_pesos97,
                                r_op06_dev.diagnostico,
                                r_op06_dev.estado,
                                r_op06_dev.afore);
                                
   END FOREACH 
   
   ##################################
   #   INFORMACIÓN DETALLE GLOBAL   #
   ##################################

   --Inicializa valores
   LET r_total_global.t_registros = 0
   LET r_total_global.aivs92      = 0
   LET r_total_global.aivs97      = 0
   LET r_total_global.porc_concatena = "0.0 %"

   LET arr_resumen[1].estado         = NULL
   LET arr_resumen[1].estado_desc    = NULL 
   LET arr_resumen[1].t_registros    = 0
   LET arr_resumen[1].aivs92         = 0
   LET arr_resumen[1].aivs97         = 0
   LET arr_resumen[1].porc_concatena = "0.0 %"
   
   --Recupera la información de la temporal
   DECLARE crs_dev_tmp CURSOR FOR 
   SELECT cat.estado,
           cat.estado_desc,
           SUM(tmp.aivs92),
           SUM(tmp.aivs97),
           COUNT(*)
   FROM safre_tmp:tmp_op06_acr tmp,
        cat_maq_credito cat
  WHERE tmp.estado = cat.estado
  GROUP BY 1,2
  ORDER BY cat.estado;

   LET a  = 1
   
   FOREACH crs_dev_tmp INTO arr_resumen[a].estado,
                             arr_resumen[a].estado_desc,
                             arr_resumen[a].aivs92,
                             arr_resumen[a].aivs97,
                             arr_resumen[a].t_registros

      LET r_total_global.t_registros = r_total_global.t_registros + arr_resumen[a].t_registros
      LET r_total_global.aivs92      = r_total_global.aivs92 + arr_resumen[a].aivs92
      LET r_total_global.aivs97      = r_total_global.aivs97 + arr_resumen[a].aivs97
   
      LET a = a + 1
      
   END FOREACH 

   --Elimina fila en blanco del arreglo
   IF(arr_resumen[arr_resumen.getLength()].estado IS NULL) THEN
      CALL arr_resumen.deleteElement(arr_resumen.getLength()) 
   END IF 

   --calcula el porcentaje global
   LET v_aux_porcentaje = 0

   --prc total global
   LET v_aux_porcentaje = (r_total_global.t_registros / r_total_global.t_registros) * 100
   LET r_total_global.porc_concatena = v_aux_porcentaje CLIPPED,"%"

   
   #############################
   #   DETALLE DE DIAGNOSTICO  #
   #############################

   LET arr_det[1].estado      = NULL 
   LET arr_det[1].estado_desc = NULL 
   LET arr_det[1].aivs97      = 0
   LET arr_det[1].pesos97     = 0
   LET arr_det[1].causal      = "S/C"
   LET arr_det[1].causal_desc = "SIN CAUSAL"
   LET arr_det[1].t_registros = 0
   
   --Obtenemos la fecha de movimiento 
   LET v_f_movimiento = TODAY 

   DECLARE crs_detalle_dev CURSOR FOR 
   SELECT cat.estado,
           cat.estado_desc,
           SUM(tmp.aivs97),
           SUM(tmp.pesos97),
           rch.causal,
           rch.desc_causal,
           COUNT(*)
     FROM safre_tmp:tmp_op06_acr tmp,
          cat_maq_credito cat,
          cat_rechazo_causal rch
    WHERE tmp.estado      = cat.estado
      AND tmp.estado      <> 240
      AND tmp.diagnostico = rch.causal
      AND rch.entidad     = 'AFO'
    GROUP BY 1,2,5,6
    ORDER BY cat.estado;

   LET a = 1

   FOREACH crs_detalle_dev INTO arr_det[a].estado,
                                 arr_det[a].estado_desc,
                                 arr_det[a].aivs97,   
                                 arr_det[a].pesos97, 
                                 arr_det[a].causal,     
                                 arr_det[a].causal_desc, 
                                 arr_det[a].t_registros

      LET a = a + 1
      
   END FOREACH
   
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
      LET v_desc_operacion = "Recepción Devolución Solicitudes ACR"  

      #ENCABEZADO
      PRINTX p_v_usuario
      PRINTX p_d_folio
      PRINTX v_f_presenta USING "dd-mm-yyyy"
      PRINTX v_f_movimiento USING "dd-mm-yyyy" 
      
      #RESUMEN
      PRINTX p_v_arch_proceso            --Nombre del archivo
      PRINTX v_r_bat_ctr_opera.fecha_ini --fecha inicio proceso
      PRINTX v_r_bat_ctr_opera.fecha_fin --Fecha fin proceso
      PRINTX v_desc_operacion            --Desc. operación
      
      #Totales globales
      PRINTX r_total_global.t_registros
      PRINTX r_total_global.aivs92       
      PRINTX r_total_global.aivs97           
      PRINTX r_total_global.porc_concatena 
   
   ON EVERY ROW
      #Detalle global
      FOR f=1 TO arr_resumen.getLength()
         PRINTX arr_resumen[f].estado_desc  
         PRINTX arr_resumen[f].t_registros       
         PRINTX arr_resumen[f].aivs92        
         PRINTX arr_resumen[f].aivs97   
         #calcula porcentaje
         LET v_aux_porcentaje = 0
         LET v_aux_porcentaje = (arr_resumen[f].t_registros / r_total_global.t_registros) * 100
         LET arr_resumen[f].porc_concatena = v_aux_porcentaje CLIPPED,"%"
         PRINTX arr_resumen[f].porc_concatena
      END FOR 
      
      #Detalle diagnosticos
      FOR f=1 TO arr_det.getLength()
         PRINTX arr_det[f].estado_desc
         PRINTX arr_det[f].t_registros
         PRINTX arr_det[f].aivs97 
         PRINTX arr_det[f].pesos97
         PRINTX arr_det[f].causal   
         PRINTX arr_det[f].causal_desc 
      END FOR 

      #Detalle rechazos
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
   
   DEFINE r_arh_op06      RECORD
      nss         CHAR(11),
      aivs92      DECIMAL(13,2),
      aivs97      DECIMAL(13,2),
      estado      SMALLINT,
      tipo        CHAR(2),
      num_intento CHAR(4),
      causal_rch  CHAR(3),
      afore       CHAR(3)
   END RECORD  

   LET v_arh_salida   = "Detalle_TA06_",TODAY USING "yyyymmdd",".cta" CLIPPED 
   LET v_ruta_archivo = v_ruta_envio CLIPPED,"/",v_arh_salida

   #Recupera información de la temporal

   LET v_qry_salida = "SELECT nss,
                              aivs92,
                              aivs97,
                              estado,
                              diagnostico,
                              afore
                         FROM safre_tmp:tmp_op06_acr;"
                            
      
   PREPARE prp_arh_op06 FROM v_qry_salida
   DECLARE crs_arh_op06 CURSOR FOR prp_arh_op06

   LET archivo = base.Channel.create()
   CALL archivo.openFile(v_ruta_archivo,"w")

   LET r_arh_op06.nss         = NULL
   LET r_arh_op06.aivs92      = 0 
   LET r_arh_op06.aivs97      = 0
   LET r_arh_op06.estado      = NULL
   LET r_arh_op06.tipo        = NULL
   LET r_arh_op06.num_intento = "   1" 
   LET r_arh_op06.causal_rch  = NULL
   LET v_detalle              = NULL 
   
   FOREACH crs_arh_op06 INTO r_arh_op06.nss,
                              r_arh_op06.aivs92,
                              r_arh_op06.aivs97,
                              r_arh_op06.estado,
                              r_arh_op06.causal_rch,
                              r_arh_op06.afore

      IF(r_arh_op06.aivs92 IS NULL) THEN
         LET r_arh_op06.aivs92 = 0
      END IF 

      IF(r_arh_op06.aivs97 IS NULL) THEN
         LET r_arh_op06.aivs97 = 0
      END IF 
      
      CASE 
         WHEN  r_arh_op06.estado = 20 OR r_arh_op06.estado = 140 
            LET r_arh_op06.tipo  = "DE"
         WHEN  r_arh_op06.estado = 25 OR r_arh_op06.estado = 145
            LET r_arh_op06.tipo  = "SR"
         WHEN  r_arh_op06.estado = 142
            LET r_arh_op06.tipo  = "CE"
      END CASE

      LET v_detalle = v_r_cre_ctr_arch.f_proceso USING "yyyymmdd",
                      r_arh_op06.nss,
                      r_arh_op06.aivs92,
                      r_arh_op06.aivs97,
                      r_arh_op06.tipo,
                      r_arh_op06.num_intento,
                      r_arh_op06.causal_rch USING "&&&",
                      r_arh_op06.afore USING "&&&"

      --Escribe en archivo de salida
      CALL archivo.writeLine(v_detalle)
      
   END FOREACH

   CALL archivo.close()
   
END FUNCTION 

FUNCTION crea_temporal()

   DATABASE safre_tmp
   
   WHENEVER ERROR CONTINUE 
      DROP TABLE tmp_op06_acr
   WHENEVER ERROR STOP
      CREATE TABLE tmp_op06_acr(id_cre_acreditado  DECIMAL(9,0),
                                 id_derechohabiente DECIMAL(9,0),
                                 nss                CHAR(11),
                                 aivs92             DECIMAL(20,2),
                                 pesos92            DECIMAL(20,2),
                                 aivs97             DECIMAL(20,2),
                                 pesos97            DECIMAL(20,2),
                                 diagnostico        CHAR(3),
                                 estado             SMALLINT,
                                 afore              CHAR(3));

   DATABASE safre_viv

END FUNCTION 
