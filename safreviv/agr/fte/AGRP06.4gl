#########################################################################
#Módulo            =>AGR                                                #
#Programa          =>AGRP06                                             #
#Objetivo          =Programa que realiza la conciliación de la informa- #
#                   ción de deudor vs tmp deudor saldo AG               #
#Autor             =>Daniel Buendia, EFP                                #
#Fecha inicio      =>04 Abril 2012                                      #
#Autor modifica    =>Emilio Abarca, EFP                                 #
#Fecha modifica    =>19 Abril 2018                                      #
#Desc. modifica    =>Adecuación al reporte PDF y generación de archivo  #
#                    de salida para los saldos transferidos AGR.        #
#########################################################################

DATABASE safre_viv
GLOBALS "AGRG01.4gl"

   DEFINE p_v_usuario           LIKE seg_usuario.usuario -- nNombre del usuario
   DEFINE p_d_pid               LIKE bat_ctr_proceso.pid -- pid
   DEFINE p_i_proceso_cod       LIKE cat_proceso.proceso_cod -- Código del proceso
   DEFINE p_i_opera_cod         LIKE cat_operacion.opera_cod -- Código de la operacion
   DEFINE p_d_folio             LIKE glo_ctr_archivo.folio -- Número de folio
   DEFINE p_v_arch_proceso      VARCHAR(100) -- Nombre del archivo a integrar
   DEFINE v_c_programa_cod      LIKE cat_operacion.programa_cod -- nombrel del programa
   --Variables para el reporte
   DEFINE v_ruta_bin            LIKE seg_modulo.ruta_bin 
   DEFINE v_ruta_listados       LIKE seg_modulo.ruta_listados
   DEFINE v_d_id_cre_ctr_arch   LIKE cre_ctr_archivo.id_cre_ctr_archivo 
   DEFINE v_r_cre_ctr_arch      RECORD LIKE cre_ctr_archivo.*
   DEFINE v_r_bat_ctr_opera     RECORD LIKE bat_ctr_operacion.*
   DEFINE v_ruta_rpt            STRING
   DEFINE v_f_movimiento        DATE
   --variables para el archivo de salida
   DEFINE v_arh_salida          STRING
   DEFINE v_ruta_envio          CHAR(40)

   #RECORDS PARA LA INFORMACIÓN DEL REPORTE
   DEFINE r_total_global    RECORD
      t_registros     INTEGER,
      aivs92          DECIMAL(16,2),
      aivs97          DECIMAL(16,2),
      porc_concatena  CHAR(12)
   END RECORD

    --Solicitud nueva
   DEFINE r_total_sol_nueva RECORD
      t_registros     INTEGER,
      aivs92          DECIMAL(16,2),
      aivs97          DECIMAL(16,2),
      porc_concatena  CHAR(12)
   END RECORD 
   
   --Saldo remanente
   DEFINE r_total_sal_rema RECORD
      t_registros     INTEGER,
      aivs92          DECIMAL(16,2),
      aivs97          DECIMAL(16,2),
      porc_concatena  CHAR(12)
   END RECORD 
   
   --Conciliación adelantos
   DEFINE r_total_adelantos RECORD
      t_registros     INTEGER,
      aivs92          DECIMAL(16,2),
      aivs97          DECIMAL(16,2),
      porc_concatena  CHAR(12)
   END RECORD
   
   --Saldo remanente liquidado
   DEFINE r_total_sal_rema_liq RECORD
      t_registros     INTEGER,
      aivs92          DECIMAL(16,2),
      aivs97          DECIMAL(16,2),
      porc_concatena  CHAR(12)
   END RECORD
   
   --Casos especiales
   DEFINE r_total_especial RECORD
      t_registros     INTEGER,
      aivs92          DECIMAL(16,2),
      aivs97          DECIMAL(16,2),
      porc_concatena  CHAR(12)
   END RECORD

   DEFINE r_b_valida            SMALLINT  -- estatus de retorno de las funciones globales
   DEFINE v_s_mens_correo       STRING -- contiene el cuerpo del correo
   DEFINE v_s_titulo_correo     STRING -- contiene el titulo del correo
   DEFINE v_s_archivo_correo    STRING -- ruta y nombre del archivo adjunto en el correo
   DEFINE v_s_qryTxt            STRING
   DEFINE v_si_operacion        LIKE cre_ctr_archivo.operacion --Operación del proceso
   DEFINE v_d_pid_arch          LIKE bat_ctr_operacion.pid
   DEFINE v_reporte_bin         STRING
   DEFINE v_manejador_rpt       OM.SaxDocumentHandler -- Contenedor de Documentos para el reporte

#Objetivo: Conciliar la información de saldos transferidos
MAIN

   -- Se recuperan los parámetros que envia el programa lanzador
   LET p_v_usuario       = ARG_VAL(1)
   LET p_d_pid           = ARG_VAL(2)
   LET p_i_proceso_cod   = ARG_VAL(3)
   LET p_i_opera_cod     = ARG_VAL(4)
   LET p_d_folio         = ARG_VAL(5)
   LET p_v_arch_proceso  = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".AGRP06.log")

   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'agr';

   DISPLAY " "
   DISPLAY "=INICIA AGRP06="
   DISPLAY "=CONCILIACIÓN DE SALDOS TRANSFERIDOS AGR="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"
   DISPLAY " ARCHIVO       : ",p_v_arch_proceso
   DISPLAY " "

   -- Recupera la ruta de listados en el que se enviara el archivo
   CALL fn_rutas("agr") RETURNING v_ruta_bin, v_ruta_listados

   -- se obtiene el nombrel del programa correspondiente
   LET v_c_programa_cod = fn_obten_nom_programa(p_i_proceso_cod , p_i_opera_cod)

   LET v_si_operacion = 9 -- Saldos transferidos

    -- se consulta el folio del archivo
   LET v_s_qryTxt = " SELECT MAX(id_cre_ctr_archivo)\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE id_proceso = ",g_id_proceso_agr,"\n",
                    "    AND operacion = ",v_si_operacion

   PREPARE prp_folio_archivo FROM v_s_qryTxt
   EXECUTE prp_folio_archivo INTO v_d_id_cre_ctr_arch
   DISPLAY " Identificador Archivo: ",v_d_id_cre_ctr_arch

   -- se valida el identificador del archivo
   IF (v_d_id_cre_ctr_arch IS NULL) THEN
      DISPLAY " ERROR: No fue posible obtener el identificador del archivo"
      EXIT PROGRAM 
   END IF

   -- se crea la sentencia sql que busca la información del archivo cargado
   LET v_s_qryTxt = " SELECT *\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE id_cre_ctr_archivo = ",v_d_id_cre_ctr_arch

   PREPARE prp_cre_ctr_arch FROM v_s_qryTxt
   EXECUTE prp_cre_ctr_arch INTO v_r_cre_ctr_arch.* 

   LET v_d_pid_arch = fn_max_pid(g_proc_cod_agr_sdos_transf, 2)
   DISPLAY " PID del archivo: ",v_d_pid_arch


   DISPLAY " "
   CALL obtiene_info_rpt()
   DISPLAY " > OBTIENE INFORMACIÓN PARA EL REPORTE PDF ...COMPLETADO"
   DISPLAY ""
   CALL genera_archivo_salida()
   DISPLAY " > GENERA ARCHIVO DE SALIDA ..."
   DISPLAY "  El archivo de salida de ha generado en /safreviv_int/agr/envio"
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
                    "    AND proceso_cod = ",g_proc_cod_agr_sdos_transf,"\n",
                    "    AND opera_cod = 2"

   PREPARE prp_bat_ctr_opera FROM v_s_qryTxt
   EXECUTE prp_bat_ctr_opera INTO v_r_bat_ctr_opera.*

   #################################################
   #   CONFIGURACIÓN PARA SALIDA DEL REPORTE PDF   #
   #################################################   

   LET v_reporte_bin = v_ruta_bin CLIPPED,"/AGRP061.4rp"
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
       DISPLAY "Advertencia: No fue posible abrir plantilla del reporte"
   END IF

   DISPLAY " "
   DISPLAY " > ENVÍA CORREO DEL REPORTE"
   DISPLAY " "
   -- se asigna el titulo del correo
   LET v_s_titulo_correo = "Proceso: CONCILIACIÓN DE SALDOS TRANSFERIDOS ANUALIDADES GARANTIZADAS"
   
   -- se asigna el archivo a adjuntar
   LET v_s_archivo_correo = v_ruta_rpt
   
   -- se asigna el cuerpo del correo
   LET v_s_mens_correo =  "ID Proceso   : ",p_d_pid,"\n",
                       "Proceso      : CONCILIACIÓN\n",
                       "Operacion    : SALDOS TRANSFERIDOS AGR\n",
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

   DEFINE cont                   INTEGER 
   DEFINE k                      INTEGER  
   DEFINE v_aux_porcentaje       DECIMAL(6,2)
   
   DEFINE r_op09  RECORD 
      id_cre_acreditado           DECIMAL(9,0),
      id_derechohabiente          DECIMAL(9,0),
      aivs92                      DECIMAL(12,2),
      aivs97                      DECIMAL(12,2),
      estado                      SMALLINT
   END RECORD

   ##################################
   #   INFORMACIÓN DETALLE GLOBAL   #
   ##################################

   --Inicializa total global
   LET r_total_global.t_registros          = 0
   LET r_total_global.aivs92               = 0
   LET r_total_global.aivs97               = 0
   LET r_total_global.porc_concatena       = 0

   LET r_total_sol_nueva.t_registros       = 0
   LET r_total_sol_nueva.aivs92            = 0
   LET r_total_sol_nueva.aivs97            = 0
   LET r_total_sol_nueva.porc_concatena    = 0

   LET r_total_sal_rema.t_registros        = 0
   LET r_total_sal_rema.aivs92             = 0
   LET r_total_sal_rema.aivs97             = 0
   LET r_total_sal_rema.porc_concatena     = 0

   LET r_total_adelantos.t_registros       = 0
   LET r_total_adelantos.aivs92            = 0
   LET r_total_adelantos.aivs97            = 0
   LET r_total_adelantos.porc_concatena    = 0

   LET r_total_sal_rema_liq.t_registros    = 0
   LET r_total_sal_rema_liq.aivs92         = 0
   LET r_total_sal_rema_liq.aivs97         = 0
   LET r_total_sal_rema_liq.porc_concatena = 0

   LET r_total_especial.t_registros        = 0
   LET r_total_especial.aivs92             = 0
   LET r_total_especial.aivs97             = 0
   LET r_total_especial.porc_concatena     = 0
   
   #Se recupera las solicitudes tipo "AG" de cre_acreditado
   DECLARE crs_op09_ag CURSOR FOR
   SELECT cre.id_cre_acreditado,
           cre.id_derechohabiente,
           tms.aivs92,
           tms.aivs97,
           cre.estado
     FROM safre_tmp:tmp_sdo_transf_det_agr tmp,
          safre_tmp:tmp_agr_solic_sdo tms, 
          cre_acreditado cre,
          cre_his_acreditado his
    WHERE tmp.nss_infonavit      = tms.nss
      AND tms.id_referencia      = cre.id_cre_acreditado
      AND cre.id_cre_acreditado  = his.id_cre_acreditado
      AND his.id_cre_ctr_archivo = v_d_id_cre_ctr_arch
      AND his.edo_procesar       = 120 --SALDOS TRANSFERIDOS
      AND tms.modulo_cod         = "AG";

   INITIALIZE r_op09.* TO NULL

   LET cont = 1 
   
   FOREACH crs_op09_ag INTO r_op09.id_cre_acreditado,
                             r_op09.id_derechohabiente,
                             r_op09.aivs92,
                             r_op09.aivs97,
                             r_op09.estado

      IF(r_op09.aivs92 IS NULL) THEN
         LET r_op09.aivs92 = 0
      END IF 

      IF(r_op09.aivs97 IS NULL) THEN
         LET r_op09.aivs97 = 0
      END IF 
      
      --contabiliza todos los registros para totales globales
      LET r_total_global.t_registros = r_total_global.t_registros  + 1
      LET r_total_global.aivs92      = r_total_global.aivs92 + r_op09.aivs92
      LET r_total_global.aivs97      = r_total_global.aivs97 + r_op09.aivs97
    
      --Evalua el tipo de solicitud
      CASE 
         --Solicitudes nuevas
         WHEN r_op09.estado = 20
            LET r_total_sol_nueva.t_registros = r_total_sol_nueva.t_registros + 1
            LET r_total_sol_nueva.aivs92      = r_total_sol_nueva.aivs92 + r_op09.aivs92
            LET r_total_sol_nueva.aivs97      = r_total_sol_nueva.aivs97 + r_op09.aivs97
            
         --Saldo remanente
         WHEN r_op09.estado = 25
            LET r_total_sal_rema.t_registros = r_total_sal_rema.t_registros  + 1
            LET r_total_sal_rema.aivs92      = r_total_sal_rema.aivs92 + r_op09.aivs92
            LET r_total_sal_rema.aivs97      = r_total_sal_rema.aivs97 + r_op09.aivs97
        
         --Conciliación por adelanto
         WHEN r_op09.estado = 140
            LET r_total_adelantos.t_registros = r_total_adelantos.t_registros + 1
            LET r_total_adelantos.aivs92      = r_total_adelantos.aivs92 + r_op09.aivs92
            LET r_total_adelantos.aivs97      = r_total_adelantos.aivs97 + r_op09.aivs97
           
         --Saldo deudor liquidado
         WHEN r_op09.estado = 145
            LET r_total_sal_rema_liq.t_registros = r_total_sal_rema_liq.t_registros + 1
            LET r_total_sal_rema_liq.aivs92      = r_total_sal_rema_liq.aivs92 + r_op09.aivs92
            LET r_total_sal_rema_liq.aivs97      = r_total_sal_rema_liq.aivs97 + r_op09.aivs97
            
         --Casos especiales
         WHEN r_op09.estado = 142
            LET r_total_especial.t_registros = r_total_especial.t_registros + 1
            LET r_total_especial.aivs92      = r_total_especial.aivs92 + r_op09.aivs92
            LET r_total_especial.aivs97      = r_total_especial.aivs97 + r_op09.aivs97
         
      END CASE 
      
      LET cont = cont + 1
      
   END FOREACH 
   
   #Nota: La información se obtiene de cre_uso_garantia, ya que en la integración se crea un nuevo registro en esa tabla de acuerdo al
   #      id_cre_ctr_archivo cargado
   INITIALIZE r_op09.* TO NULL 

   DECLARE crs_op09_ua CURSOR FOR 
   SELECT ug.id_cre_uso_garantia,
           ug.id_derechohabiente,
           tms.aivs92,
           tms.aivs97,
           ug.estado
     FROM safre_tmp:tmp_sdo_transf_det_agr tmp,
          safre_tmp:tmp_agr_solic_sdo tms,
          cre_uso_garantia ug
    WHERE tmp.nss_infonavit     = tms.nss
      AND tms.id_referencia     = ug.id_cre_uso_garantia
      AND ug.edo_procesar       = 120 --SALDO TRANSFERIDO
      AND tms.modulo_cod        = "UA";

   FOREACH crs_op09_ua INTO r_op09.id_cre_acreditado,
                             r_op09.id_derechohabiente,
                             r_op09.aivs92,
                             r_op09.aivs97,
                             r_op09.estado                   

      IF(r_op09.aivs92 IS NULL) THEN
         LET r_op09.aivs92 = 0
      END IF 

      IF(r_op09.aivs97 IS NULL) THEN
         LET r_op09.aivs97 = 0
      END IF 

      --Totales globales 
      LET r_total_global.t_registros = r_total_global.t_registros + 1
      LET r_total_global.aivs92      = r_total_global.aivs92 + r_op09.aivs92
      LET r_total_global.aivs97      = r_total_global.aivs97 + r_op09.aivs97

      --Evalúa la tipo de solicitud
      CASE 
         --Solicitudes nuevas
         WHEN r_op09.estado = 20
            LET r_total_sol_nueva.t_registros = r_total_sol_nueva.t_registros + 1
            LET r_total_sol_nueva.aivs92      = r_total_sol_nueva.aivs92 + r_op09.aivs92
            LET r_total_sol_nueva.aivs97      = r_total_sol_nueva.aivs97 + r_op09.aivs97
            
         --Saldo remanente
         WHEN r_op09.estado = 25
            LET r_total_sal_rema.t_registros = r_total_sal_rema.t_registros  + 1
            LET r_total_sal_rema.aivs92      = r_total_sal_rema.aivs92 + r_op09.aivs92
            LET r_total_sal_rema.aivs97      = r_total_sal_rema.aivs97 + r_op09.aivs97
        
         --Conciliación por adelanto
         WHEN r_op09.estado = 140
            LET r_total_adelantos.t_registros = r_total_adelantos.t_registros + 1
            LET r_total_adelantos.aivs92      = r_total_adelantos.aivs92 + r_op09.aivs92
            LET r_total_adelantos.aivs97      = r_total_adelantos.aivs97 + r_op09.aivs97
           
         --Saldo deudor liquidado
         WHEN r_op09.estado = 145
            LET r_total_sal_rema_liq.t_registros = r_total_sal_rema_liq.t_registros + 1
            LET r_total_sal_rema_liq.aivs92      = r_total_sal_rema_liq.aivs92 + r_op09.aivs92
            LET r_total_sal_rema_liq.aivs97      = r_total_sal_rema_liq.aivs97 + r_op09.aivs97
            
         --Casos especiales
         WHEN r_op09.estado = 142
            LET r_total_especial.t_registros = r_total_especial.t_registros + 1
            LET r_total_especial.aivs92      = r_total_especial.aivs92 + r_op09.aivs92
            LET r_total_especial.aivs97      = r_total_especial.aivs97 + r_op09.aivs97
         
      END CASE 
      
      LET cont = cont + 1
      
   END FOREACH 

   --Calcula porcentajes para los totales globales y las solicitudes.
   LET v_aux_porcentaje = 0

   --prc total global
   LET v_aux_porcentaje = (r_total_global.t_registros / r_total_global.t_registros) * 100
   LET r_total_global.porc_concatena = v_aux_porcentaje CLIPPED,"%"

   --prc solicitudes nuevas
   LET v_aux_porcentaje = (r_total_sol_nueva.t_registros / r_total_global.t_registros) * 100
   LET r_total_sol_nueva.porc_concatena = v_aux_porcentaje CLIPPED,"%"

   --prc saldo remanente 
   LET v_aux_porcentaje = (r_total_sal_rema.t_registros / r_total_global.t_registros) * 100
   LET r_total_sal_rema.porc_concatena = v_aux_porcentaje CLIPPED,"%"
   
   --prc adelantos
   LET v_aux_porcentaje = (r_total_adelantos.t_registros / r_total_global.t_registros) * 100
   LET r_total_adelantos.porc_concatena = v_aux_porcentaje CLIPPED,"%"

   --prc saldo deudor liquidado
   LET v_aux_porcentaje = (r_total_sal_rema_liq.t_registros / r_total_global.t_registros) * 100
   LET r_total_sal_rema_liq.porc_concatena = v_aux_porcentaje CLIPPED,"%"

   --prc especiales
   LET v_aux_porcentaje = (r_total_especial.t_registros / r_total_global.t_registros) * 100
   LET r_total_especial.porc_concatena= v_aux_porcentaje CLIPPED,"%"

   
END FUNCTION 

#OBJETIVO: Genera el reporte de Devoluciones
REPORT genera_PDF()

   DEFINE v_f_presenta     DATE
   DEFINE v_desc_operacion CHAR(60)
   DEFINE f                INTEGER 

   FORMAT 
   FIRST PAGE HEADER
      #Obtiene la fecha de movimiento de como se calcularon las aivs
      SELECT MAX(f_movimiento)
        INTO v_f_movimiento
        FROM cre_uso_garantia
       WHERE edo_procesar = 115
         AND id_cre_ctr_archivo = v_d_id_cre_ctr_arch;
      
      LET v_f_presenta = TODAY
      LET v_desc_operacion = "Recepción Saldos Transferidos AG"  

      #ENCABEZADO
      PRINTX p_v_usuario
      PRINTX p_d_folio
      PRINTX v_f_presenta USING "dd-mm-yyyy"
      PRINTX v_f_movimiento USING "dd-mm-yyyy" 
      #RESUMEN
      PRINTX p_v_arch_proceso            --Nombre del archivo
      PRINTX v_r_bat_ctr_opera.fecha_ini --fecha inicio proceso
      PRINTX v_r_bat_ctr_opera.fecha_fin --Fecha fin de la operación
      PRINTX v_desc_operacion            --Desc. operación
      #Totales globales
      PRINTX r_total_global.t_registros
      PRINTX r_total_global.aivs92       
      PRINTX r_total_global.aivs97           
      PRINTX r_total_global.porc_concatena   
      --Total solicitudes nuevas
      PRINTX r_total_sol_nueva.t_registros
      PRINTX r_total_sol_nueva.aivs92    
      PRINTX r_total_sol_nueva.aivs97         
      PRINTX r_total_sol_nueva.porc_concatena 
      --Total adelantos
      PRINTX r_total_adelantos.t_registros
      PRINTX r_total_adelantos.aivs92    
      PRINTX r_total_adelantos.aivs97         
      PRINTX r_total_adelantos.porc_concatena
      --Total especial
      PRINTX r_total_especial.t_registros
      PRINTX r_total_especial.aivs92     
      PRINTX r_total_especial.aivs97          
      PRINTX r_total_especial.porc_concatena
      --Total saldo remanente
      PRINTX r_total_sal_rema.t_registros
      PRINTX r_total_sal_rema.aivs92    
      PRINTX r_total_sal_rema.aivs97         
      PRINTX r_total_sal_rema.porc_concatena
      --Total saldo remanente liquidado
      PRINTX r_total_sal_rema_liq.t_registros
      PRINTX r_total_sal_rema_liq.aivs92    
      PRINTX r_total_sal_rema_liq.aivs97         
      PRINTX r_total_sal_rema_liq.porc_concatena 
   
END REPORT

#OBJETIVO: Genera archivo de salida detalle de rechazos
FUNCTION genera_archivo_salida()

   DEFINE v_qry_salida    STRING
   DEFINE v_detalle       STRING
   DEFINE v_ruta_archivo  STRING 
   DEFINE archivo         base.Channel
   
   DEFINE r_arh_op09      RECORD
      nss         CHAR(11),
      aivs92      DECIMAL(13,2),
      aivs97      DECIMAL(13,2),
      estado      SMALLINT,
      tipo        CHAR(2)
   END RECORD  

   LET v_arh_salida   = "Detalle_AG09_",TODAY USING "yyyymmdd",".cag" CLIPPED 
   LET v_ruta_archivo = v_ruta_envio CLIPPED,"/",v_arh_salida

   #############################
   # Detalle tipo solicitud AG #
   #############################

   LET v_qry_salida = "SELECT tp.nss_infonavit,
                              ts.aivs92,
                              ts.aivs97,
                              ac.estado
                         FROM safre_tmp:tmp_sdo_transf_det_agr tp,
                              safre_tmp:tmp_agr_solic_sdo ts,
                              cre_acreditado ac,
                              cre_his_acreditado hs
                        WHERE tp.nss_infonavit      = ts.nss
                          AND ts.id_referencia      = ac.id_cre_acreditado
                          AND ac.id_cre_acreditado  = hs.id_cre_acreditado
                          AND hs.id_cre_ctr_archivo = ",v_d_id_cre_ctr_arch,
                        " AND hs.edo_procesar       = 120 
                          AND ts.modulo_cod         = 'AG';"


   PREPARE prp_arh_ag FROM v_qry_salida
   DECLARE crs_arh_ag CURSOR FOR prp_arh_ag

   LET archivo = base.Channel.create()
   CALL archivo.openFile(v_ruta_archivo,"w")

   INITIALIZE r_arh_op09.* TO NULL 
   LET v_detalle = NULL 
   
   FOREACH crs_arh_ag INTO r_arh_op09.nss,
                            r_arh_op09.aivs92,
                            r_arh_op09.aivs97,
                            r_arh_op09.estado

      IF(r_arh_op09.aivs92 IS NULL) THEN
         LET r_arh_op09.aivs92 = 0
      END IF 

      IF(r_arh_op09.aivs97 IS NULL) THEN
         LET r_arh_op09.aivs97 = 0
      END IF 
      
      CASE 
         WHEN  r_arh_op09.estado = 20
            LET r_arh_op09.tipo  = "UG"
         WHEN  r_arh_op09.estado = 25 OR r_arh_op09.estado = 145
            LET r_arh_op09.tipo  = "SR"
         WHEN  r_arh_op09.estado = 140
            LET r_arh_op09.tipo  = "CA"
         WHEN  r_arh_op09.estado = 142
            LET r_arh_op09.tipo  = "CE"
      END CASE 

      LET v_detalle = v_r_cre_ctr_arch.f_proceso USING "yyyymmdd",
                      r_arh_op09.nss,
                      r_arh_op09.aivs92,
                      r_arh_op09.aivs97,
                      r_arh_op09.tipo

      --Escribe en archivo de salida
      CALL archivo.writeLine(v_detalle)
      
   END FOREACH

   #############################
   # Detalle tipo solicitud UA #
   #############################

   INITIALIZE r_arh_op09.* TO NULL 
   LET v_detalle = NULL 
   

   LET v_qry_salida = "SELECT tp.nss_infonavit,
                              ts.aivs92,
                              ts.aivs97,
                              ug.estado      
                         FROM safre_tmp:tmp_sdo_transf_det_agr tp,
                              safre_tmp:tmp_agr_solic_sdo ts,
                              cre_uso_garantia ug
                        WHERE tp.nss_infonavit      = ts.nss
                          AND ts.id_referencia      = ug.id_cre_uso_garantia
                          AND ug.edo_procesar       = 120
                          AND ts.modulo_cod         = 'UA';"

    
   PREPARE prp_arh_ua FROM v_qry_salida
   DECLARE crs_arh_ua CURSOR FOR prp_arh_ua

   FOREACH crs_arh_ua INTO r_arh_op09.nss,
                            r_arh_op09.aivs92,
                            r_arh_op09.aivs97,
                            r_arh_op09.estado

      IF(r_arh_op09.aivs92 IS NULL) THEN
         LET r_arh_op09.aivs92 = 0
      END IF 

      IF(r_arh_op09.aivs97 IS NULL) THEN
         LET r_arh_op09.aivs97 = 0
      END IF 
      
      CASE 
         WHEN  r_arh_op09.estado = 20
            LET r_arh_op09.tipo  = "UG"
         WHEN  r_arh_op09.estado = 25 OR r_arh_op09.estado = 145
            LET r_arh_op09.tipo  = "SR"
         WHEN  r_arh_op09.estado = 140
            LET r_arh_op09.tipo  = "CA"
         WHEN  r_arh_op09.estado = 142
            LET r_arh_op09.tipo  = "CE"
      END CASE 

      LET v_detalle = v_r_cre_ctr_arch.f_proceso USING "yyyymmdd",
                      r_arh_op09.nss,
                      r_arh_op09.aivs92,
                      r_arh_op09.aivs97,
                      r_arh_op09.tipo

      --Escribe en archivo de salida
      CALL archivo.writeLine(v_detalle)
      
   END FOREACH

   CALL archivo.close()
   
END FUNCTION 
