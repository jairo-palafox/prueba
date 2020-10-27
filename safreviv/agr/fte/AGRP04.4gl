#########################################################################
#Modulo            =>AGR                                                #
#Programa          =>AGRP04                                             #
#Objetivo          =>Programa que realiza la conciliación de la infor-  #
#                    mación de deudor vs tmp deudor rechazo AGR         #
#Autor             =>Daniel Buendia, EFP                                #
#Fecha inicio      =>26 Marzo 2012                                      #
#Autor modifica    =>Emilio Abarca, EFP.                                #
#Fecha modifica    =>13 Marzo 2018.                                     #
#Objetivo modifica =>Adecuación a reporte PDF y generación archivo de   #
#                    de salida rechazos Operación 01 AG.                #
#########################################################################

DATABASE safre_viv

GLOBALS "AGRG01.4gl"

    DEFINE p_v_usuario           LIKE seg_usuario.usuario                -- nombre del usuario
    DEFINE p_d_pid               LIKE bat_ctr_proceso.pid                -- pid
    DEFINE p_i_proceso_cod       LIKE cat_proceso.proceso_cod            -- código del proceso
    DEFINE p_i_opera_cod         LIKE cat_operacion.opera_cod            -- código de la operaciÓn
    DEFINE p_d_folio             VARCHAR(10)                            -- número de folio
    DEFINE p_v_arch_proceso      LIKE cre_ctr_archivo.nom_archivo        -- nombre del archivo a integrar
   --variables del reporte
   DEFINE v_d_id_cre_ctr_arch    LIKE cre_ctr_archivo.id_cre_ctr_archivo -- id del archiovo
   DEFINE v_r_cre_ctr_arch       RECORD LIKE cre_ctr_archivo.*          -- registro de cre ctr archivo
   DEFINE v_r_bat_ctr_opera      RECORD LIKE bat_ctr_operacion.*        -- registro de bat ctr operación
   DEFINE v_f_movimiento         DATE  
   DEFINE v_precio_fondo         DECIMAL(19,14)
   DEFINE v_ruta_bin             LIKE seg_modulo.ruta_bin
   DEFINE v_ruta_listados        LIKE seg_modulo.ruta_listados
   DEFINE v_ruta_rpt             STRING
   DEFINE v_c_programa_cod       LIKE cat_operacion.programa_cod -- nombrel del programa
   DEFINE r_b_valida             SMALLINT
   --variables archivo de salida
   DEFINE v_arh_salida           STRING
   DEFINE v_ruta_envio           CHAR(40)
   --variables envio de correo
   DEFINE v_s_titulo_correo      STRING -- contiene el titulo del correo
   DEFINE v_s_archivo_correo     STRING -- ruta y nombre del archivo adjunto en el correo
   DEFINE v_s_mens_correo        STRING -- contiene el cuerpo del correo

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

   #ARREGLOS PARA INFORMACIÓN RECHAZOS REPORTE
   DEFINE r_inf_det_rch    RECORD
      estado          SMALLINT,
      diagnostico     CHAR(3),
      desc_causal     CHAR(200),
      aivs97          DECIMAL(16,2),
      total           INTEGER 
   END RECORD  
   DEFINE arr_rch_sol_nueva DYNAMIC ARRAY OF RECORD
      t_registros     INTEGER,
      aivs97          DECIMAL(16,2),
      pesos           DECIMAL(12,2),
      diagnostico     CHAR(3),
      desc_rechazo    CHAR(200)
   END RECORD
   
   DEFINE arr_rch_sal_rema DYNAMIC ARRAY OF RECORD
      t_registros     INTEGER,
      aivs97          DECIMAL(16,2),
      pesos           DECIMAL(12,2),
      diagnostico     CHAR(3),
      desc_rechazo    CHAR(200)
   END RECORD
   
   DEFINE arr_rch_adelantos DYNAMIC ARRAY OF RECORD
      t_registros     INTEGER,
      aivs97          DECIMAL(16,2),
      pesos           DECIMAL(12,2),
      diagnostico     CHAR(3),
      desc_rechazo    CHAR(200)
   END RECORD
   
   DEFINE arr_rch_sal_rema_liq DYNAMIC ARRAY OF RECORD
      t_registros     INTEGER,
      aivs97          DECIMAL(16,2),
      pesos           DECIMAL(12,2),
      diagnostico     CHAR(3),
      desc_rechazo    CHAR(200)
   END RECORD
   
   DEFINE arr_rch_especial DYNAMIC ARRAY OF RECORD
      t_registros     INTEGER,
      aivs97          DECIMAL(16,2),
      pesos           DECIMAL(12,2),
      diagnostico     CHAR(3),
      desc_rechazo    CHAR(200)
   END RECORD

   DEFINE v_s_qryTxt       STRING
   DEFINE v_d_pid_arch     LIKE bat_ctr_operacion.pid
   DEFINE v_reporte_bin    STRING
   DEFINE v_manejador_rpt  OM.SaxDocumentHandler -- Contenedor de Documentos para el reporte

# Objetivo: Conciliar la información de recurrente
MAIN
   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario       = ARG_VAL(1)
   LET p_d_pid           = ARG_VAL(2)
   LET p_i_proceso_cod   = ARG_VAL(3)
   LET p_i_opera_cod     = ARG_VAL(4)
   LET p_d_folio         = ARG_VAL(5)
   LET p_v_arch_proceso  = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".AGRP04.log")

   DISPLAY " "
   DISPLAY "=INICIA AGRP04="
   DISPLAY "=CONCILIACIÓN DE RECHAZO DE SALDOS AGR="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"
   DISPLAY " ARCHIVO       : ",p_v_arch_proceso
   DISPLAY " "

   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'agr';

   -- Recupera la ruta de listados en el que se enviara el archivo
   CALL fn_rutas("agr") RETURNING V_ruta_bin, V_ruta_listados
   -- se obtiene el nombrel del programa correspondiente
   LET v_c_programa_cod = fn_obten_nom_programa(p_i_proceso_cod , p_i_opera_cod)

    --se obtiene el id cre ctr archivo
   LET v_d_id_cre_ctr_arch = fn_cre_ctr_arch()
   DISPLAY " Identificador del archivo: ",v_d_id_cre_ctr_arch 

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

   --Obtiene PID del proceso
   LET v_d_pid_arch = fn_max_pid(g_proc_cod_agr_rech_saldos, 2)
   DISPLAY " PID: ",v_d_pid_arch


   DISPLAY " "
   CALL obtiene_info_rpt()
   DISPLAY " > OBTIENE INFORMACIÓN PARA EL REPORTE PDF ...COMPLETADO"
   DISPLAY ""
   CALL genera_archivo_salida()
   DISPLAY " > GENERA ARCHIVO DE SALIDA ...COMPLETADO"
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
                    "    AND proceso_cod = ",g_proc_cod_agr_rech_saldos,"\n",
                    "    AND opera_cod = 2"

   PREPARE prp_bat_ctr_opera FROM v_s_qryTxt
   EXECUTE prp_bat_ctr_opera INTO v_r_bat_ctr_opera.*

   #################################################
   #   CONFIGURACIÓN PARA SALIDA DEL REPORTE PDF   #
   #################################################   

   LET v_reporte_bin = v_ruta_bin CLIPPED,"/AGRP041.4rp"
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

   DISPLAY ""
   DISPLAY " > ENVÍA CORREO DEL REPORTE"
   DISPLAY " "

   -- se asigna el titulo del correo
   LET v_s_titulo_correo = "Proceso: CONCILIACIÓN DE RECHAZO DE SALDOS ANUALIDADES GARANTIZADAS"
   -- se asigna el archivo a adjuntar
   LET v_s_archivo_correo = v_ruta_rpt
   -- se asigna el cuerpo del correo
   LET v_s_mens_correo =  "ID Proceso   : ",p_d_pid,"\n",
                          "Proceso      : CONCILIACIÓN\n",
                          "Operacion    : RECHAZO DE SALDOS AGR\n",
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

   DEFINE cont                   INTEGER --contador detalle global
   DEFINE v_aux_porcentaje       DECIMAL(6,2)
   DEFINE v_aux_pesos            DECIMAL(12,2)
   --contadores para arreglos detalle rechazos
   DEFINE c_sol_nvas             INTEGER 
   DEFINE c_sdo_rema             INTEGER
   DEFINE c_adelantos            INTEGER 
   DEFINE c_rema_liq             INTEGER 
   DEFINE c_especial             INTEGER 

   -- Record total global
   DEFINE r_op01_rch  RECORD 
      id_cre_acreditado           DECIMAL(9,0),
      id_derechohabiente          DECIMAL(9,0),
      aivs92                      DECIMAL(12,2),
      aivs97                      DECIMAL(12,2),
      estado                      SMALLINT
   END RECORD

   ##################################
   #   INFORMACIÓN DETALLE GLOBAL   #
   ##################################

   #Nota: Se obtiene información para los registros tipo "AG" que se recuperan cre_his_solic_sdo (Historia petición saldos)
   --    estos se actualizan en cre_Acreditado con historia en cre_his_acreditado
   INITIALIZE r_op01_rch.* TO NULL 

   -- Inicializa records en caso de no recuperar registros.
   LET r_total_global.t_registros    = 0
   LET r_total_global.aivs92         = 0
   LET r_total_global.aivs97         = 0
   LET r_total_global.porc_concatena = 0 

   LET r_total_sol_nueva.t_registros    = 0
   LET r_total_sol_nueva.aivs92         = 0
   LET r_total_sol_nueva.aivs97         = 0
   LET r_total_sol_nueva.porc_concatena = 0

   LET r_total_sal_rema.t_registros    = 0
   LET r_total_sal_rema.aivs92         = 0
   LET r_total_sal_rema.aivs97         = 0
   LET r_total_sal_rema.porc_concatena = 0

   LET r_total_adelantos.t_registros    = 0
   LET r_total_adelantos.aivs92         = 0
   LET r_total_adelantos.aivs97         = 0
   LET r_total_adelantos.porc_concatena = 0

   LET r_total_sal_rema_liq.t_registros    = 0
   LET r_total_sal_rema_liq.aivs92         = 0
   LET r_total_sal_rema_liq.aivs97         = 0
   LET r_total_sal_rema_liq.porc_concatena = 0

   LET r_total_especial.t_registros        = 0
   LET r_total_especial.aivs92             = 0
   LET r_total_especial.aivs97             = 0
   LET r_total_especial.porc_concatena     = 0
   
   DECLARE crs_op01_ag_rch CURSOR FOR 
   SELECT a.id_cre_acreditado,
           a.id_derechohabiente,
           s.aivs92,
           s.aivs97,
           a.estado
     FROM safre_tmp:tmp_rech_sdo_det_agr t,
           safre_tmp:tmp_agr_solic_sdo s,
           cre_acreditado a,
           cre_his_acreditado h
    WHERE t.nss_infonavit      = s.nss
      AND s.id_referencia      = a.id_cre_acreditado
      AND a.id_cre_acreditado  = h.id_cre_acreditado
      AND h.id_cre_ctr_archivo = v_d_id_cre_ctr_arch
      AND h.edo_procesar       = 90   -- SALDO RECHAZADO
      AND s.modulo_cod         = "AG";
           
   LET cont = 1

   FOREACH crs_op01_ag_rch INTO r_op01_rch.id_cre_acreditado,
                                  r_op01_rch.id_derechohabiente,          
                                  r_op01_rch.aivs92,                     
                                  r_op01_rch.aivs97,                     
                                  r_op01_rch.estado                      

      IF(r_op01_rch.aivs92 IS NULL) THEN
         LET r_op01_rch.aivs92 = 0
      END IF 

      IF(r_op01_rch.aivs97 IS NULL) THEN
         LET r_op01_rch.aivs97 = 0
      END IF 

      --Totales globales 
      LET r_total_global.t_registros = r_total_global.t_registros + 1
      LET r_total_global.aivs92      = r_total_global.aivs92 + r_op01_rch.aivs92
      LET r_total_global.aivs97      = r_total_global.aivs97 + r_op01_rch.aivs97

      --Evalúa la tipo de solicitud
      CASE 
         --Solicitudes nuevas
         WHEN r_op01_rch.estado = 20
            LET r_total_sol_nueva.t_registros = r_total_sol_nueva.t_registros + 1
            LET r_total_sol_nueva.aivs92      = r_total_sol_nueva.aivs92 + r_op01_rch.aivs92
            LET r_total_sol_nueva.aivs97      = r_total_sol_nueva.aivs97 + r_op01_rch.aivs97
            
         --Saldo remanente
         WHEN r_op01_rch.estado = 25
            LET r_total_sal_rema.t_registros = r_total_sal_rema.t_registros  + 1
            LET r_total_sal_rema.aivs92      = r_total_sal_rema.aivs92 + r_op01_rch.aivs92
            LET r_total_sal_rema.aivs97      = r_total_sal_rema.aivs97 + r_op01_rch.aivs97
        
         --Conciliación por adelanto
         WHEN r_op01_rch.estado = 140
            LET r_total_adelantos.t_registros = r_total_adelantos.t_registros + 1
            LET r_total_adelantos.aivs92      = r_total_adelantos.aivs92 + r_op01_rch.aivs92
            LET r_total_adelantos.aivs97      = r_total_adelantos.aivs97 + r_op01_rch.aivs97
           
         --Saldo deudor liquidado
         WHEN r_op01_rch.estado = 145
            LET r_total_sal_rema_liq.t_registros = r_total_sal_rema_liq.t_registros + 1
            LET r_total_sal_rema_liq.aivs92      = r_total_sal_rema_liq.aivs92 + r_op01_rch.aivs92
            LET r_total_sal_rema_liq.aivs97      = r_total_sal_rema_liq.aivs97 + r_op01_rch.aivs97
            
         --Casos especiales
         WHEN r_op01_rch.estado = 142
            LET r_total_especial.t_registros = r_total_especial.t_registros + 1
            LET r_total_especial.aivs92      = r_total_especial.aivs92 + r_op01_rch.aivs92
            LET r_total_especial.aivs97      = r_total_especial.aivs97 + r_op01_rch.aivs97
         
      END CASE 
      
      LET cont = cont + 1
      
   END FOREACH 

   #Obtiene registros con tipo de petición "UA" Uso de anualidad, estos se actualizan en cre_uso_garantia
   DECLARE crs_op01_ua_rch CURSOR FOR 
   SELECT s.id_referencia,
           s.id_derechohabiente,
           s.aivs92,
           s.aivs97,
           g.estado
     FROM safre_tmp:tmp_rech_sdo_det_agr t,
           safre_tmp:tmp_agr_solic_sdo s,
           cre_uso_garantia g
    WHERE t.nss_infonavit = s.nss
      AND s.id_derechohabiente = g.id_derechohabiente
      AND g.id_cre_ctr_archivo = v_d_id_cre_ctr_arch
      AND g.edo_procesar = 90   -- SALDO RECHAZADO
      AND s.modulo_cod   = "UA";

   INITIALIZE r_op01_rch.* TO NULL 

   FOREACH crs_op01_ua_rch INTO r_op01_rch.id_cre_acreditado,
                                 r_op01_rch.id_derechohabiente,          
                                 r_op01_rch.aivs92,                     
                                 r_op01_rch.aivs97,                     
                                 r_op01_rch.estado                      

      IF(r_op01_rch.aivs92 IS NULL) THEN
         LET r_op01_rch.aivs92 = 0
      END IF 

      IF(r_op01_rch.aivs97 IS NULL) THEN
         LET r_op01_rch.aivs97 = 0
      END IF 

      --Totales globales 
      LET r_total_global.t_registros = r_total_global.t_registros + 1
      LET r_total_global.aivs92      = r_total_global.aivs92 + r_op01_rch.aivs92
      LET r_total_global.aivs97      = r_total_global.aivs97 + r_op01_rch.aivs97

      --Evalúa la tipo de solicitud
      CASE 
         --Solicitudes nuevas
         WHEN r_op01_rch.estado = 20
            LET r_total_sol_nueva.t_registros = r_total_sol_nueva.t_registros + 1
            LET r_total_sol_nueva.aivs92      = r_total_sol_nueva.aivs92 + r_op01_rch.aivs92
            LET r_total_sol_nueva.aivs97      = r_total_sol_nueva.aivs97 + r_op01_rch.aivs97
            
         --Saldo remanente
         WHEN r_op01_rch.estado = 25
            LET r_total_sal_rema.t_registros = r_total_sal_rema.t_registros  + 1
            LET r_total_sal_rema.aivs92      = r_total_sal_rema.aivs92 + r_op01_rch.aivs92
            LET r_total_sal_rema.aivs97      = r_total_sal_rema.aivs97 + r_op01_rch.aivs97
        
         --Conciliación por adelanto
         WHEN r_op01_rch.estado = 140
            LET r_total_adelantos.t_registros = r_total_adelantos.t_registros + 1
            LET r_total_adelantos.aivs92      = r_total_adelantos.aivs92 + r_op01_rch.aivs92
            LET r_total_adelantos.aivs97      = r_total_adelantos.aivs97 + r_op01_rch.aivs97
           
         --Saldo deudor liquidado
         WHEN r_op01_rch.estado = 145
            LET r_total_sal_rema_liq.t_registros = r_total_sal_rema_liq.t_registros + 1
            LET r_total_sal_rema_liq.aivs92      = r_total_sal_rema_liq.aivs92 + r_op01_rch.aivs92
            LET r_total_sal_rema_liq.aivs97      = r_total_sal_rema_liq.aivs97 + r_op01_rch.aivs97
            
         --Casos especiales
         WHEN r_op01_rch.estado = 142
            LET r_total_especial.t_registros = r_total_especial.t_registros + 1
            LET r_total_especial.aivs92      = r_total_especial.aivs92 + r_op01_rch.aivs92
            LET r_total_especial.aivs97      = r_total_especial.aivs97 + r_op01_rch.aivs97
         
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

   ###########################
   #   DETALLE DE RECHAZOS   #
   ###########################

   --Obtiene la f_movimiento para calcular el valor de fondo
   {SELECT MAX(f_movimiento)
     INTO v_f_movimiento
     FROM cre_uso_garantia
    WHERE edo_procesar = 90
      AND id_cre_ctr_archivo = v_d_id_cre_ctr_arch}

   --Obtenemos la fecha de movimiento 
   
   LET v_f_movimiento = TODAY 

   SELECT precio_fondo
     INTO v_precio_fondo
     FROM glo_valor_fondo
    WHERE f_valuacion = v_f_movimiento
      AND fondo = 11;

   INITIALIZE r_inf_det_rch.* TO NULL 

   -- Inicializa arreglos en caso de no encontrar información.
   LET arr_rch_sol_nueva[1].t_registros     = 0
   LET arr_rch_sol_nueva[1].aivs97          = 0
   LET arr_rch_sol_nueva[1].pesos           = 0
   LET arr_rch_sol_nueva[1].diagnostico     = "s/c" 
   LET arr_rch_sol_nueva[1].desc_rechazo    = NULL

   LET arr_rch_sal_rema[1].t_registros      = 0
   LET arr_rch_sal_rema[1].aivs97           = 0
   LET arr_rch_sal_rema[1].pesos            = 0
   LET arr_rch_sal_rema[1].diagnostico      = "s/c" 
   LET arr_rch_sal_rema[1].desc_rechazo     = NULL
   
   LET arr_rch_adelantos[1].t_registros     = 0
   LET arr_rch_adelantos[1].aivs97          = 0
   LET arr_rch_adelantos[1].pesos           = 0
   LET arr_rch_adelantos[1].diagnostico     = "s/c" 
   LET arr_rch_adelantos[1].desc_rechazo    = NULL 

   LET arr_rch_sal_rema_liq[1].t_registros  = 0
   LET arr_rch_sal_rema_liq[1].aivs97       = 0
   LET arr_rch_sal_rema_liq[1].pesos        = 0
   LET arr_rch_sal_rema_liq[1].diagnostico  = "s/c" 
   LET arr_rch_sal_rema_liq[1].desc_rechazo = NULL 
   
   LET arr_rch_especial[1].t_registros      = 0
   LET arr_rch_especial[1].aivs97           = 0
   LET arr_rch_especial[1].pesos            = 0
   LET arr_rch_especial[1].diagnostico      = "s/c"
   LET arr_rch_especial[1].desc_rechazo     = NULL

   LET v_aux_pesos = 0
   
   ################################
   # DETALLE RECHAZOS TIPO AG     #
   ################################

   DECLARE crs_rch_sol_ag CURSOR FOR 
   SELECT a.estado,
          h.diagnostico,
          r.desc_causal,
          SUM(s.aivs97),
          COUNT(*)
     FROM safre_tmp:tmp_rech_sdo_det_agr t,
          safre_tmp:tmp_agr_solic_sdo s,
          cre_acreditado a,
          cre_his_acreditado h,
          cat_rechazo_causal r
    WHERE t.nss_infonavit      = s.nss
      AND s.id_referencia      = a.id_cre_acreditado
      AND a.id_cre_acreditado  = h.id_cre_acreditado
      AND h.id_cre_ctr_archivo = v_d_id_cre_ctr_arch
      AND h.diagnostico  = r.causal
      AND r.entidad      = "PRC"
      AND h.edo_procesar = 90   -- SALDO RECHAZADO
      AND s.modulo_cod   = "AG"
      GROUP BY 1,2,3;

   --Inicializa contadores 
   LET c_sol_nvas  = 1 
   LET c_sdo_rema  = 1 
   LET c_adelantos = 1
   LET c_rema_liq  = 1
   LET C_especial  = 1
   
   FOREACH crs_rch_sol_ag INTO r_inf_det_rch.estado,   
                               r_inf_det_rch.diagnostico,    
                               r_inf_det_rch.desc_causal,    
                               r_inf_det_rch.aivs97,         
                               r_inf_det_rch.total          

      --calcula pesos de acuerdo a las aivs
      LET v_aux_pesos = (r_inf_det_rch.aivs97 * v_precio_fondo)

      --llena arreglos de acuerdo al tipo de petición
      CASE
         --Solicitudes nuevas
         WHEN r_inf_det_rch.estado = 20
            LET arr_rch_sol_nueva[c_sol_nvas].t_registros  = r_inf_det_rch.total
            LET arr_rch_sol_nueva[c_sol_nvas].aivs97       = r_inf_det_rch.aivs97
            LET arr_rch_sol_nueva[c_sol_nvas].pesos        = v_aux_pesos
            LET arr_rch_sol_nueva[c_sol_nvas].diagnostico  = r_inf_det_rch.diagnostico
            LET arr_rch_sol_nueva[c_sol_nvas].desc_rechazo =  r_inf_det_rch.desc_causal
            LET c_sol_nvas = c_sol_nvas + 1 --incremente contador
            
         --Saldos remanentes
         WHEN r_inf_det_rch.estado = 25
            LET arr_rch_sal_rema[c_sdo_rema].t_registros     = r_inf_det_rch.total
            LET arr_rch_sal_rema[c_sdo_rema].aivs97          = r_inf_det_rch.aivs97
            LET arr_rch_sal_rema[c_sdo_rema].pesos           = v_aux_pesos
            LET arr_rch_sal_rema[c_sdo_rema].diagnostico     = r_inf_det_rch.diagnostico
            LET arr_rch_sal_rema[c_sdo_rema].desc_rechazo    =  r_inf_det_rch.desc_causal
            LET c_sdo_rema = c_sdo_rema + 1 --incremente contador
            
         --conciliación por adelanto
         WHEN r_inf_det_rch.estado = 140
            LET arr_rch_adelantos[c_adelantos].t_registros    = r_inf_det_rch.total
            LET arr_rch_adelantos[c_adelantos].aivs97         = r_inf_det_rch.aivs97
            LET arr_rch_adelantos[c_adelantos].pesos          = v_aux_pesos
            LET arr_rch_adelantos[c_adelantos].diagnostico    = r_inf_det_rch.diagnostico
            LET arr_rch_adelantos[c_adelantos].desc_rechazo   =  r_inf_det_rch.desc_causal
            LET c_adelantos = c_adelantos + 1 --incremente contador
            
         --saldo deudor liquidado   
         WHEN r_inf_det_rch.estado = 145
            LET arr_rch_sal_rema_liq[c_rema_liq].t_registros  = r_inf_det_rch.total
            LET arr_rch_sal_rema_liq[c_rema_liq].aivs97       = r_inf_det_rch.aivs97
            LET arr_rch_sal_rema_liq[c_rema_liq].pesos        = v_aux_pesos
            LET arr_rch_sal_rema_liq[c_rema_liq].diagnostico  = r_inf_det_rch.diagnostico
            LET arr_rch_sal_rema_liq[c_rema_liq].desc_rechazo =  r_inf_det_rch.desc_causal
            LET c_rema_liq = c_rema_liq + 1 --incremente contador
            
         --casos especiales
         WHEN r_inf_det_rch.estado = 142
            LET arr_rch_especial[C_especial].t_registros  = r_inf_det_rch.total
            LET arr_rch_especial[C_especial].aivs97       = r_inf_det_rch.aivs97
            LET arr_rch_especial[C_especial].pesos        = v_aux_pesos
            LET arr_rch_especial[C_especial].diagnostico  = r_inf_det_rch.diagnostico
            LET arr_rch_especial[C_especial].desc_rechazo =  r_inf_det_rch.desc_causal
            LET C_especial = C_especial + 1 --incremente contador
            
      END CASE
      
   END FOREACH 

   ################################
   # DETALLE RECHAZOS TIPO UA     #
   ################################

   LET v_aux_pesos = 0

   DECLARE crs_rch_sol_ua CURSOR FOR  
   SELECT g.estado,
           g.diagnostico,
           r.desc_causal,
           SUM(s.aivs97),
           COUNT(*)
      FROM safre_tmp:tmp_rech_sdo_det_agr t,
           safre_tmp:tmp_agr_solic_sdo s,
           cre_uso_garantia g,
           cat_rechazo_causal r
    WHERE t.nss_infonavit      = s.nss
      AND s.id_derechohabiente = g.id_derechohabiente
      AND g.id_cre_ctr_archivo = v_d_id_cre_ctr_arch
      AND g.diagnostico  = r.causal
      AND r.entidad      = "PRC"
      AND g.edo_procesar = 90   -- SALDO RECHAZADO
      AND s.modulo_cod   = "UA"
      GROUP BY 1,2,3;

   INITIALIZE r_inf_det_rch.* TO NULL 
   
   --LET k = 1 --> No se inicializa contador, ya que se siguen llenando los mismos arreglos

   FOREACH crs_rch_sol_ua INTO r_inf_det_rch.estado,   
                                r_inf_det_rch.diagnostico,    
                                r_inf_det_rch.desc_causal,    
                                r_inf_det_rch.aivs97,         
                                r_inf_det_rch.total          

      --calcula pesos de acuerdo a las aivs
      LET v_aux_pesos = (r_inf_det_rch.aivs97 * v_precio_fondo)

      --llena arreglos de acuerdo al tipo de petición
      CASE
         --Solicitudes nuevas
         WHEN r_inf_det_rch.estado = 20
            LET arr_rch_sol_nueva[c_sol_nvas].t_registros  = r_inf_det_rch.total
            LET arr_rch_sol_nueva[c_sol_nvas].aivs97       = r_inf_det_rch.aivs97
            LET arr_rch_sol_nueva[c_sol_nvas].pesos        = v_aux_pesos
            LET arr_rch_sol_nueva[c_sol_nvas].diagnostico  = r_inf_det_rch.diagnostico
            LET arr_rch_sol_nueva[c_sol_nvas].desc_rechazo =  r_inf_det_rch.desc_causal
            LET c_sol_nvas = c_sol_nvas + 1 --incremente contador
            
         --Saldos remanentes
         WHEN r_inf_det_rch.estado = 25
            LET arr_rch_sal_rema[c_sdo_rema].t_registros     = r_inf_det_rch.total
            LET arr_rch_sal_rema[c_sdo_rema].aivs97          = r_inf_det_rch.aivs97
            LET arr_rch_sal_rema[c_sdo_rema].pesos           = v_aux_pesos
            LET arr_rch_sal_rema[c_sdo_rema].diagnostico     = r_inf_det_rch.diagnostico
            LET arr_rch_sal_rema[c_sdo_rema].desc_rechazo    =  r_inf_det_rch.desc_causal
            LET c_sdo_rema = c_sdo_rema + 1 --incremente contador
            
         --conciliación por adelanto
         WHEN r_inf_det_rch.estado = 140
            LET arr_rch_adelantos[c_adelantos].t_registros    = r_inf_det_rch.total
            LET arr_rch_adelantos[c_adelantos].aivs97         = r_inf_det_rch.aivs97
            LET arr_rch_adelantos[c_adelantos].pesos          = v_aux_pesos
            LET arr_rch_adelantos[c_adelantos].diagnostico    = r_inf_det_rch.diagnostico
            LET arr_rch_adelantos[c_adelantos].desc_rechazo   =  r_inf_det_rch.desc_causal
            LET c_adelantos = c_adelantos + 1 --incremente contador
            
         --saldo deudor liquidado   
         WHEN r_inf_det_rch.estado = 145
            LET arr_rch_sal_rema_liq[c_rema_liq].t_registros  = r_inf_det_rch.total
            LET arr_rch_sal_rema_liq[c_rema_liq].aivs97       = r_inf_det_rch.aivs97
            LET arr_rch_sal_rema_liq[c_rema_liq].pesos        = v_aux_pesos
            LET arr_rch_sal_rema_liq[c_rema_liq].diagnostico  = r_inf_det_rch.diagnostico
            LET arr_rch_sal_rema_liq[c_rema_liq].desc_rechazo =  r_inf_det_rch.desc_causal
            LET c_rema_liq = c_rema_liq + 1 --incremente contador
            
         --casos especiales
         WHEN r_inf_det_rch.estado = 142
            LET arr_rch_especial[C_especial].t_registros  = r_inf_det_rch.total
            LET arr_rch_especial[C_especial].aivs97       = r_inf_det_rch.aivs97
            LET arr_rch_especial[C_especial].pesos        = v_aux_pesos
            LET arr_rch_especial[C_especial].diagnostico  = r_inf_det_rch.diagnostico
            LET arr_rch_especial[C_especial].desc_rechazo =  r_inf_det_rch.desc_causal
            LET C_especial = C_especial + 1 --incremente contador
            
      END CASE

   END FOREACH
   
END FUNCTION 

#OBJETIVO: Genera el reporte de Rechazos
REPORT genera_PDF()

   DEFINE v_f_presenta     DATE
   DEFINE v_desc_operacion CHAR(60)
   DEFINE f                INTEGER 

   FORMAT 
   FIRST PAGE HEADER
      LET v_f_presenta = TODAY
      LET v_desc_operacion = "Recepción rechazos de saldo AG"  

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
   
   ON EVERY ROW
      #Detalle Rechazos

      --rechazos solicitudes nuevas
      FOR f=1 TO arr_rch_sol_nueva.getLength()
         PRINTX arr_rch_sol_nueva[f].t_registros  
         PRINTX arr_rch_sol_nueva[f].aivs97       
         PRINTX arr_rch_sol_nueva[f].pesos        
         PRINTX arr_rch_sol_nueva[f].diagnostico   
         PRINTX arr_rch_sol_nueva[f].desc_rechazo 
      END FOR 

      --rechazos adelantos
      FOR f=1 TO arr_rch_adelantos.getLength()
         PRINTX arr_rch_adelantos[f].t_registros  
         PRINTX arr_rch_adelantos[f].aivs97       
         PRINTX arr_rch_adelantos[f].pesos        
         PRINTX arr_rch_adelantos[f].diagnostico   
         PRINTX arr_rch_adelantos[f].desc_rechazo 
      END FOR 

      --rechazos casos especales
      FOR f=1 TO arr_rch_especial.getLength()
         PRINTX arr_rch_especial[f].t_registros  
         PRINTX arr_rch_especial[f].aivs97       
         PRINTX arr_rch_especial[f].pesos        
         PRINTX arr_rch_especial[f].diagnostico   
         PRINTX arr_rch_especial[f].desc_rechazo 
      END FOR 
      
      --rechazos saldo remanente
      FOR f=1 TO arr_rch_sal_rema.getLength()
         PRINTX arr_rch_sal_rema[f].t_registros  
         PRINTX arr_rch_sal_rema[f].aivs97       
         PRINTX arr_rch_sal_rema[f].pesos        
         PRINTX arr_rch_sal_rema[f].diagnostico   
         PRINTX arr_rch_sal_rema[f].desc_rechazo 
      END FOR 

      --rechazos saldo remanente liquidado
      FOR f=1 TO arr_rch_sal_rema_liq.getLength()
         PRINTX arr_rch_sal_rema_liq[f].t_registros  
         PRINTX arr_rch_sal_rema_liq[f].aivs97       
         PRINTX arr_rch_sal_rema_liq[f].pesos        
         PRINTX arr_rch_sal_rema_liq[f].diagnostico   
         PRINTX arr_rch_sal_rema_liq[f].desc_rechazo 
      END FOR 
      
END REPORT

#OBJETIVO: Genera archivo de salida detalle rechazos.
FUNCTION genera_archivo_salida()

   DEFINE v_qry_salida    STRING
   DEFINE v_detalle       STRING
   DEFINE v_ruta_archivo  STRING 
   DEFINE archivo         base.Channel
   
   DEFINE r_arh_op01      RECORD
      f_proceso   DATE, 
      nss         CHAR(11),
      aivs92      DECIMAL(13,2),
      aivs97      DECIMAL(13,2),
      estado      SMALLINT,
      tipo        CHAR(2),
      num_intento CHAR(4),
      causal_rch  CHAR(3)
   END RECORD  

   LET v_arh_salida   = "Detalle_AG01_",TODAY USING "yyyymmdd",".cag" CLIPPED 
   LET v_ruta_archivo = v_ruta_envio CLIPPED,"/",v_arh_salida

   #############################
   # Detalle tipo solicitud AG #
   #############################

   LET v_qry_salida = "SELECT arh.f_proceso,
                              tmp.nss_infonavit,
                              sol.aivs92,
                              sol.aivs97,
                              acre.estado,
                              his.diagnostico
                         FROM safre_tmp:tmp_rech_sdo_det_agr tmp,
                              safre_tmp:tmp_agr_solic_sdo sol,
                              cre_acreditado acre,
                              cre_his_acreditado his,
                              cre_ctr_archivo arh
                        WHERE tmp.nss_infonavit      = sol.nss
                          AND sol.id_referencia      = acre.id_cre_acreditado
                          AND acre.id_cre_acreditado = his.id_cre_acreditado
                          AND his.id_cre_ctr_archivo = ",v_d_id_cre_ctr_arch,
                        " AND his.id_cre_ctr_archivo = arh.id_cre_ctr_archivo
                          AND his.edo_procesar       = 90
                          AND sol.modulo_cod         = 'AG';"
      
   PREPARE prp_arh_ag FROM v_qry_salida
   DECLARE crs_arh_ag CURSOR FOR prp_arh_ag

   LET archivo = base.Channel.create()
   CALL archivo.openFile(v_ruta_archivo,"w")

   LET r_arh_op01.f_proceso   = NULL 
   LET r_arh_op01.nss         = NULL
   LET r_arh_op01.aivs92      = 0 
   LET r_arh_op01.aivs97      = 0
   LET r_arh_op01.estado      = NULL
   LET r_arh_op01.tipo        = NULL
   LET r_arh_op01.num_intento = "   1" 
   LET r_arh_op01.causal_rch  = NULL
   LET v_detalle              = NULL 
   
   FOREACH crs_arh_ag INTO r_arh_op01.f_proceso,
                            r_arh_op01.nss,
                            r_arh_op01.aivs92,
                            r_arh_op01.aivs97,
                            r_arh_op01.estado,
                            r_arh_op01.causal_rch

      IF(r_arh_op01.aivs92 IS NULL) THEN
         LET r_arh_op01.aivs92 = 0
      END IF 

      IF(r_arh_op01.aivs97 IS NULL) THEN
         LET r_arh_op01.aivs97 = 0
      END IF 
      
      CASE 
         WHEN  r_arh_op01.estado = 20
            LET r_arh_op01.tipo  = "UG"
         WHEN  r_arh_op01.estado = 25 OR r_arh_op01.estado = 145
            LET r_arh_op01.tipo  = "SR"
         WHEN  r_arh_op01.estado = 140
            LET r_arh_op01.tipo  = "CA"
         WHEN  r_arh_op01.estado = 142
            LET r_arh_op01.tipo  = "CE"
      END CASE 

      LET v_detalle = r_arh_op01.f_proceso USING "yyyymmdd",
                      r_arh_op01.nss,
                      r_arh_op01.aivs92,
                      r_arh_op01.aivs97,
                      r_arh_op01.tipo,
                      r_arh_op01.num_intento,
                      r_arh_op01.causal_rch USING "&&&"

      --Escribe en archivo de salida
      CALL archivo.writeLine(v_detalle)
      
   END FOREACH

   #############################
   # Detalle tipo solicitud UA #
   #############################

   LET r_arh_op01.f_proceso   = NULL 
   LET r_arh_op01.nss         = NULL
   LET r_arh_op01.aivs92      = 0 
   LET r_arh_op01.aivs97      = 0
   LET r_arh_op01.estado      = NULL
   LET r_arh_op01.tipo        = NULL
   LET r_arh_op01.num_intento = "   1" 
   LET r_arh_op01.causal_rch  = NULL
   LET v_detalle              = NULL 
   

   LET v_qry_salida = "SELECT arh.f_proceso,
                              tmp.nss_infonavit,
                              sol.aivs92,
                              sol.aivs97,
                              ug.estado,
                              ug.diagnostico
                         FROM safre_tmp:tmp_rech_sdo_det_agr tmp,
                              safre_tmp:tmp_agr_solic_sdo sol,
                              cre_uso_garantia ug,
                              cre_ctr_archivo arh
                        WHERE tmp.nss_infonavit      = sol.nss
                          AND sol.id_derechohabiente = ug.id_derechohabiente
                          AND ug.id_cre_ctr_archivo  = ",v_d_id_cre_ctr_arch,
                        " AND ug.id_cre_ctr_archivo  = arh.id_cre_ctr_archivo
                          AND ug.edo_procesar        = 90
                          AND sol.modulo_cod         = 'UA';"

   PREPARE prp_arh_ua FROM v_qry_salida
   DECLARE crs_arh_ua CURSOR FOR prp_arh_ua

   FOREACH crs_arh_ua INTO r_arh_op01.f_proceso,
                            r_arh_op01.nss,
                            r_arh_op01.aivs92,
                            r_arh_op01.aivs97,
                            r_arh_op01.estado,
                            r_arh_op01.causal_rch

      IF(r_arh_op01.aivs92 IS NULL) THEN
         LET r_arh_op01.aivs92 = 0
      END IF 

      IF(r_arh_op01.aivs97 IS NULL) THEN
         LET r_arh_op01.aivs97 = 0
      END IF 
      
      CASE 
         WHEN  r_arh_op01.estado = 20
            LET r_arh_op01.tipo  = "UG"
         WHEN  r_arh_op01.estado = 25 OR r_arh_op01.estado = 145
            LET r_arh_op01.tipo  = "SR"
         WHEN  r_arh_op01.estado = 140
            LET r_arh_op01.tipo  = "CA"
         WHEN  r_arh_op01.estado = 142
            LET r_arh_op01.tipo  = "CE"
      END CASE 

      LET v_detalle = r_arh_op01.f_proceso USING "yyyymmdd",
                      r_arh_op01.nss,
                      r_arh_op01.aivs92,
                      r_arh_op01.aivs97,
                      r_arh_op01.tipo,
                      r_arh_op01.num_intento,
                      r_arh_op01.causal_rch USING "&&&"

      --Escribe en archivo de salida
      CALL archivo.writeLine(v_detalle)
      
   END FOREACH

   CALL archivo.close()
   
END FUNCTION 

##OBJETIVO: Obtiene la descripcion para el diagnostico
FUNCTION fn_obt_desc_diagnostico(p_diagnostico, p_edo_procesar)

   DEFINE p_diagnostico        LIKE cre_his_acreditado.diagnostico
   DEFINE p_edo_procesar       LIKE cre_his_acreditado.edo_procesar
   DEFINE v_desc_diagnostico   LIKE cat_rechazo.desc_rechazo
   DEFINE v_tpo_rechazo        LIKE cat_rechazo.tpo_rechazo

   CASE 
      WHEN (p_edo_procesar = 40 OR p_edo_procesar = 50  OR p_edo_procesar = 90 OR 
            p_edo_procesar =110 OR p_edo_procesar = 190 OR p_edo_procesar = 200)
         LET v_tpo_rechazo = "RCH"
      WHEN p_edo_procesar = 100
         LET v_tpo_rechazo = "DEV"
      WHEN p_edo_procesar = 150
         LET v_tpo_rechazo = "SIS"
      OTHERWISE 
         LET v_tpo_rechazo = NULL
   END CASE

   IF p_diagnostico = 0 THEN 
      LET v_desc_diagnostico = "N/A"
   ELSE
      IF v_tpo_rechazo IS NOT NULL THEN
         SELECT desc_rechazo
           INTO v_desc_diagnostico
           FROM cat_rechazo
          WHERE tpo_rechazo = v_tpo_rechazo
            AND cod_rechazo = p_diagnostico
      END IF
   END IF

   IF v_desc_diagnostico IS NULL THEN
      LET v_desc_diagnostico = "N/A"
   END IF 

   RETURN v_desc_diagnostico

END FUNCTION   

FUNCTION fn_cre_ctr_arch()

   DEFINE v_i_proceso_cod       LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE v_i_opera_cod         LIKE cat_operacion.opera_cod -- codigo de la operacion
   DEFINE v_si_id_proceso       LIKE cre_ctr_archivo.id_proceso -- identificador del proceso
   DEFINE v_d_cre_ctr_archivo   LIKE cre_ctr_archivo.id_cre_ctr_archivo -- identificador del archivo
   DEFINE v_extencion           LIKE cat_operacion.extension
   DEFINE v_s_qryTxt            STRING -- guarda una sentencia SQL a ejecutar

   -- se asigna el código de proceso y operacion de la validación del archivo
   LET v_i_proceso_cod = g_proc_cod_agr_rech_saldos -- recepción rechazo de saldos agr
   LET v_i_opera_cod = 1
   LET v_si_id_proceso = g_id_proceso_agr -- Anualidades Garantizadas

   LET v_s_qryTxt = " SELECT extension\n",
                    "   FROM cat_operacion\n",
                    "  WHERE proceso_cod = ",v_i_proceso_cod,"\n",
                    "    AND opera_cod = ",v_i_opera_cod

   PREPARE prp_obt_extension FROM v_s_qryTxt
   EXECUTE prp_obt_extension INTO v_extencion

   LET v_s_qryTxt = " SELECT MAX(id_cre_ctr_archivo)",
                    "   FROM cre_ctr_archivo",
                    "  WHERE id_proceso = ",v_si_id_proceso,
                    "    AND operacion = 01",--rechazos
                    "    AND estado = 20",
                    "    AND nom_archivo LIKE '%.",v_extencion CLIPPED,"'"

   PREPARE prp_cons FROM v_s_qryTxt
   EXECUTE prp_cons INTO v_d_cre_ctr_archivo

   IF v_d_cre_ctr_archivo IS NULL THEN
      LET v_d_cre_ctr_archivo = 0
   END IF

   RETURN v_d_cre_ctr_archivo

END FUNCTION
