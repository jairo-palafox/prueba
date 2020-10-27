--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

#########################################################################
#Modulo            =>GRT                                                #
#Programa          =>GRTP11                                             #
#Objetivo          =>Programa que realiza la conciliación de la infor-  #
#                    mación de grt deudor vs tmp deudor rechazo saldos  #
#                    para el módulo de Uso de Garantía 43 bis           #
#Autor             =>Daniel Buendia, EFP                                #
#Fecha inicio      =>26 Abril 2012                                      #
#Autor modifica    =>Emilio Abarca, EFP                                 #
#Fecha modifica    =>06 Febrero 2018                                    #
#Desc. modifica    =>Adecuación al reporte PDF                          #
#########################################################################

DATABASE safre_viv

GLOBALS "GRTG01.4gl"

DEFINE p_v_usuario            LIKE seg_usuario.usuario,         -- nombre del usuario
       p_d_pid                 LIKE bat_ctr_proceso.pid,         -- pid
       p_i_proceso_cod         LIKE cat_proceso.proceso_cod,     -- codigo del proceso
       p_i_opera_cod           LIKE cat_operacion.opera_cod,     -- codigo de la operación
       p_v_arch_proceso        LIKE cre_ctr_archivo.nom_archivo, -- nombre del archivo a integrar
       v_d_id_cre_ctr_arch     DECIMAL(9,0)
   DEFINE p_d_folio           VARCHAR(10)                       -- numero de folio
   --variables para inf. reporte
   DEFINE v_r_cre_ctr_arch    RECORD LIKE cre_ctr_archivo.*
   DEFINE v_r_bat_ctr_opera   RECORD LIKE bat_ctr_operacion.*
   DEFINE v_ruta_bin          LIKE seg_modulo.ruta_bin
   DEFINE v_ruta_listados     LIKE seg_modulo.ruta_listados
   DEFINE v_ruta_envio        CHAR(40)
   DEFINE r_b_valida          SMALLINT
   DEFINE v_s_titulo_correo   STRING -- contiene el titulo del correo
   DEFINE v_s_archivo_correo  STRING -- ruta y nombre del archivo adjunto en el correo
   DEFINE v_ruta_rpt          STRING -- Ruta y nombre del reporte PDF a adjuntar
   DEFINE v_s_mens_correo     STRING -- contiene el cuerpo del correo
   DEFINE v_precio_fondo      DECIMAL(19,14)
   DEFINE v_arh_salida        STRING
   DEFINE v_f_movimiento      DATE  
   
   DEFINE r_total_global    RECORD
      t_registros     INTEGER,
      aivs97          DECIMAL(16,6),
      pesos           DECIMAL(12,2),
      porc_concatena  CHAR(12)
   END RECORD
   DEFINE r_total_sol_nueva RECORD
      t_registros     INTEGER,
      aivs97          DECIMAL(16,6),
      pesos           DECIMAL(12,2),
      porc_concatena  CHAR(12)
   END RECORD 
   DEFINE r_total_adelantos RECORD
      t_registros     INTEGER,
      aivs97          DECIMAL(16,6),
      pesos           DECIMAL(12,2),
      porc_concatena  CHAR(12)
   END RECORD
   DEFINE r_total_especial RECORD
      t_registros     INTEGER,
      aivs97          DECIMAL(16,6),
      pesos           DECIMAL(12,2),
      porc_concatena  CHAR(12)
   END RECORD
   --arreglos para detalle rechazos
   DEFINE arr_rch_sol_nueva DYNAMIC ARRAY OF RECORD
      t_registros     INTEGER,
      aivs97          DECIMAL(16,6),
      pesos           DECIMAL(12,2),
      diagnostico     CHAR(3),
      desc_rechazo    CHAR(150)
   END RECORD
   DEFINE arr_rch_adelantos DYNAMIC ARRAY OF RECORD
      t_registros     INTEGER,
      aivs97          DECIMAL(16,6),
      pesos           DECIMAL(12,2),
      diagnostico     CHAR(3),
      desc_rechazo    CHAR(150)
   END RECORD
   DEFINE arr_rch_especial DYNAMIC ARRAY OF RECORD
      t_registros     INTEGER,
      aivs97          DECIMAL(16,6),
      pesos           DECIMAL(12,2),
      diagnostico     CHAR(3),
      desc_rechazo    CHAR(150)
   END RECORD

   DEFINE v_s_qryTxt     STRING
   DEFINE v_d_pid_arch   LIKE bat_ctr_operacion.pid
   --variables conf. del reporte
   DEFINE v_reporte_bin   STRING
   DEFINE v_manejador_rpt OM.SaxDocumentHandler -- Contenedor de Documentos para el reporte


# Objetivo: Conciliar la información de Rechazo de Saldos de Uso de Garantía 43 bis
MAIN

   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario         = ARG_VAL(1)
   LET p_d_pid             = ARG_VAL(2)
   LET p_i_proceso_cod     = ARG_VAL(3)
   LET p_i_opera_cod       = ARG_VAL(4)
   LET p_d_folio           = ARG_VAL(5)
   LET p_v_arch_proceso    = ARG_VAL(6)

   CALL fn_rutas("grt") RETURNING v_ruta_bin, v_ruta_listados

   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'grt';
    
   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".GRTP11.log")

   DISPLAY " "
   DISPLAY "=INICIA GRTP11="
   DISPLAY "= CONCILIACIÓN DE RECHAZO DE SALDOS GRT ="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"
   DISPLAY " ARCHIVO       : ",p_v_arch_proceso

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

   LET v_d_pid_arch = fn_max_pid(g_proc_cod_grt_uso_rech_saldos, 2)

   DISPLAY " "
   CALL obtiene_info_rpt()
   DISPLAY " > OBTIENE INFORMACIÓN PARA EL REPORTE PDF ...COMPLETADO"
   DISPLAY ""
   CALL genera_archivo_salida()
   DISPLAY " > GENERA ARCHIVO DE SALDA ..COMPLETADO"
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
                    "    AND proceso_cod = ",g_proc_cod_grt_uso_rech_saldos,"\n",
                    "    AND opera_cod = 2"

   PREPARE prp_bat_ctr_opera FROM v_s_qryTxt
   EXECUTE prp_bat_ctr_opera INTO v_r_bat_ctr_opera.*

   #################################################
   #   CONFIGURACIÓN PARA SALIDA DEL REPORTE PDF   #
   #################################################   

   LET v_reporte_bin = v_ruta_bin CLIPPED,"/GRTP111.4rp"
   LET v_ruta_rpt    = v_ruta_listados CLIPPED,"/",
                       p_v_usuario CLIPPED,"-GRTL18-",
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
   LET v_s_titulo_correo = "Proceso: CONCILIACIÓN DE RECHAZO DE SALDOS USO DE GTÍA 43 BIS"
   -- se asigna el archivo a adjuntar
   LET v_s_archivo_correo = v_ruta_rpt
   -- se asigna el cuerpo del correo
   LET v_s_mens_correo =  "ID Proceso   : ",p_d_pid,"\n",
                          "Proceso      : CONCILIACIÓN\n",
                          "Operacion    : RECHAZO DE SALDOS USO GRT\n",
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
   
   DEFINE r_op01_rch      RECORD
      id_cre_uso_garantia        DECIMAL(9,0),
      id_derechohabiente         DECIMAL(9,0),
      importe_v97                DECIMAL(12,2),
      estado                     SMALLINT
   END RECORD

   ##################################
   #   INFORMACIÓN DETALLE GLOBAL   #
   ##################################
   
   #Nota: La información se obtiene de cre_uso_garantia, ya que en la integración se crea un nuevo registro en esa tabla de acuerdo al
   #      id_cre_ctr_archivo cargado
   INITIALIZE r_op01_rch.* TO NULL 

   DECLARE crs_op01_rch CURSOR FOR
   SELECT id_cre_uso_garantia,
           id_derechohabiente,
           importe_v97,
           estado
     FROM cre_uso_garantia 
    WHERE edo_procesar = 90 --SALDO RECHAZADO
      AND id_cre_ctr_archivo = v_d_id_cre_ctr_arch

   LET cont = 1

   --Total global registros procesados
   LET r_total_global.t_registros       = 0
   LET r_total_global.aivs97            = 0
   LET r_total_global.pesos             = 0
   LET r_total_global.porc_concatena    = 0
   --Total solicitudes nuevas
   LET r_total_sol_nueva.t_registros    = 0
   LET r_total_sol_nueva.aivs97         = 0
   LET r_total_sol_nueva.pesos          = 0
   LET r_total_sol_nueva.porc_concatena = 0
   --Total adelantos
   LET r_total_adelantos.t_registros    = 0
   LET r_total_adelantos.aivs97         = 0
   LET r_total_adelantos.pesos          = 0
   LET r_total_adelantos.porc_concatena = 0
   --Total especial
   LET r_total_especial.t_registros     = 0
   LET r_total_especial.aivs97          = 0
   LET r_total_especial.pesos           = 0
   LET r_total_especial.porc_concatena  = 0
   
   FOREACH crs_op01_rch INTO r_op01_rch.id_cre_uso_garantia,
                              r_op01_rch.id_derechohabiente,
                              r_op01_rch.importe_v97,
                              r_op01_rch.estado

      IF(r_op01_rch.importe_v97 IS NULL) THEN
         LET r_op01_rch.importe_v97 = 0
      END IF 
      
      --contabiliza todos los registros para totales globales
      LET r_total_global.t_registros       = r_total_global.t_registros  + 1
      LET r_total_global.pesos             = r_total_global.pesos + r_op01_rch.importe_v97
    
      --Evalua que tipo de solicitud es
      CASE
         --Solicitudes nuevas
         WHEN r_op01_rch.estado = 20
            LET r_total_sol_nueva.t_registros = r_total_sol_nueva.t_registros + 1
            LET r_total_sol_nueva.pesos       = r_total_sol_nueva.pesos + r_op01_rch.importe_v97
         --Adelantos
         WHEN r_op01_rch.estado = 140
            LET r_total_adelantos.t_registros = r_total_adelantos.t_registros + 1
            LET r_total_adelantos.pesos       = r_total_adelantos.pesos + r_op01_rch.importe_v97
         --Especial
         WHEN r_op01_rch.estado = 142
            LET r_total_especial.t_registros  = r_total_especial.t_registros + 1
            LET r_total_especial.pesos        = r_total_especial.pesos + r_op01_rch.importe_v97

      END CASE 
      
      LET cont = cont + 1
      
   END FOREACH 

   LET v_f_movimiento = NULL 
   
   --Obtiene la f_movimiento para calcular el valor de fondo
   SELECT MAX(f_movimiento)
     INTO v_f_movimiento
     FROM cre_uso_garantia
    WHERE edo_procesar = 90
      AND id_cre_ctr_archivo = v_d_id_cre_ctr_arch

   IF(v_f_movimiento IS NOT NULL) THEN
      SELECT precio_fondo
        INTO v_precio_fondo
        FROM glo_valor_fondo
       WHERE f_valuacion = v_f_movimiento
         AND fondo = 11;

      --Calcula AIVS97 total global
      LET r_total_global.aivs97 =  (r_total_global.pesos / v_precio_fondo)
      --Calcula AIVS solicitudes nuevas
      LET r_total_sol_nueva.aivs97 = (r_total_sol_nueva.pesos / v_precio_fondo)
      --Calcula IVS Adelantos
      LET r_total_adelantos.aivs97 = (r_total_adelantos.pesos / v_precio_fondo)
      --Calcula AIVS especial
      LET r_total_especial.aivs97  = (r_total_especial.pesos / v_precio_fondo)
   END IF

   --Calcula procentajes
   LET v_aux_porcentaje = 0

   --prc total global
   LET v_aux_porcentaje = (r_total_global.t_registros / r_total_global.t_registros) * 100
   LET r_total_global.porc_concatena = v_aux_porcentaje CLIPPED,"%"

   --prc solicitudes nuevas
   LET v_aux_porcentaje = (r_total_sol_nueva.t_registros / r_total_global.t_registros) * 100
   LET r_total_sol_nueva.porc_concatena = v_aux_porcentaje CLIPPED,"%"

   --prc adelantos
   LET v_aux_porcentaje = (r_total_adelantos.t_registros / r_total_global.t_registros) * 100
   LET r_total_adelantos.porc_concatena = v_aux_porcentaje CLIPPED,"%"

   --prc especiales
   LET v_aux_porcentaje = (r_total_especial.t_registros / r_total_global.t_registros) * 100
   LET r_total_especial.porc_concatena= v_aux_porcentaje CLIPPED,"%"

   ###########################
   #   DETALLE DE RECHAZOS   #
   ###########################

   --Inicializa valores en caso de no encontrar registros
   LET arr_rch_sol_nueva[1].t_registros  = 0
   LET arr_rch_sol_nueva[1].aivs97       = 0
   LET arr_rch_sol_nueva[1].pesos        = 0
   LET arr_rch_sol_nueva[1].diagnostico  = "s/c" 
   LET arr_rch_sol_nueva[1].desc_rechazo = NULL

   LET arr_rch_adelantos[1].t_registros  = 0
   LET arr_rch_adelantos[1].aivs97       = 0
   LET arr_rch_adelantos[1].pesos        = 0
   LET arr_rch_adelantos[1].diagnostico  = "s/c" 
   LET arr_rch_adelantos[1].desc_rechazo = NULL 

   LET arr_rch_especial[1].t_registros  = 0
   LET arr_rch_especial[1].aivs97       = 0
   LET arr_rch_especial[1].pesos        = 0
   LET arr_rch_especial[1].diagnostico  = "s/c"
   LET arr_rch_especial[1].desc_rechazo = NULL

   --Solicitudes nuevas
   DECLARE crs_rch_sol_nuevas CURSOR FOR 
   SELECT u.diagnostico,
          r.desc_rechazo,
          SUM(u.importe_v97),
          COUNT(*)
     FROM cre_uso_garantia u,
          cat_rechazo r
    WHERE u.estado = 20 
      AND u.edo_procesar = 90
      AND u.id_cre_ctr_archivo = v_d_id_cre_ctr_arch
      AND r.tpo_rechazo = "RCH"
      AND u.diagnostico = r.cod_rechazo
    GROUP BY 1,2

   LET k = 1
   
   FOREACH crs_rch_sol_nuevas INTO arr_rch_sol_nueva[k].diagnostico,
                                    arr_rch_sol_nueva[k].desc_rechazo,
                                    arr_rch_sol_nueva[k].pesos,
                                    arr_rch_sol_nueva[k].t_registros

      IF(arr_rch_sol_nueva[k].pesos IS NULL) THEN
         LET arr_rch_sol_nueva[k].pesos = 0 
      END IF
      
      --calcula AIVS97
      LET arr_rch_sol_nueva[k].aivs97 = (arr_rch_sol_nueva[k].pesos / v_precio_fondo) 
      
      LET k = k + 1

   END FOREACH 

   --Elimina última fila en blanco
   IF(arr_rch_sol_nueva[arr_rch_sol_nueva.getLength()].diagnostico IS NULL) THEN
      CALL arr_rch_sol_nueva.deleteElement(arr_rch_sol_nueva.getLength())
   END IF 

   --Adelantos
   DECLARE crs_rch_adelantos CURSOR FOR 
   SELECT u.diagnostico,
          r.desc_rechazo,
          SUM(u.importe_v97),
          COUNT(*)
     FROM cre_uso_garantia u,
          cat_rechazo r
    WHERE u.estado = 140 
      AND u.edo_procesar = 90
      AND u.id_cre_ctr_archivo = v_d_id_cre_ctr_arch
      AND r.tpo_rechazo = "RCH"
      AND u.diagnostico = r.cod_rechazo
    GROUP BY 1,2

   LET k = 1
   
   FOREACH crs_rch_adelantos INTO arr_rch_adelantos[k].diagnostico,
                                   arr_rch_adelantos[k].desc_rechazo,
                                   arr_rch_adelantos[k].pesos,
                                   arr_rch_adelantos[k].t_registros

      IF(arr_rch_adelantos[k].pesos IS NULL) THEN
         LET arr_rch_adelantos[k].pesos = 0
      END IF 
      
      --calcula AIVS97
      LET arr_rch_adelantos[k].aivs97 = (arr_rch_adelantos[k].pesos / v_precio_fondo) 
      
      LET k = k + 1

   END FOREACH 

   --Elimina última fila en blanco
   IF(arr_rch_adelantos[arr_rch_adelantos.getLength()].diagnostico IS NULL) THEN
      CALL arr_rch_adelantos.deleteElement(arr_rch_adelantos.getLength())
   END IF 
   
   --Especial
   DECLARE crs_rch_especial CURSOR FOR 
   SELECT u.diagnostico,
          r.desc_rechazo,
          SUM(u.importe_v97),
          COUNT(*)
     FROM cre_uso_garantia u,
          cat_rechazo r
    WHERE u.estado = 142
      AND u.edo_procesar = 90
      AND u.id_cre_ctr_archivo = v_d_id_cre_ctr_arch
      AND r.tpo_rechazo = "RCH"
      AND u.diagnostico = r.cod_rechazo
    GROUP BY 1,2

   LET k = 1
   
   FOREACH crs_rch_especial INTO arr_rch_especial[k].diagnostico,
                                  arr_rch_especial[k].desc_rechazo,
                                  arr_rch_especial[k].pesos,
                                  arr_rch_especial[k].t_registros

      IF(arr_rch_especial[k].pesos IS NULL) THEN
         LET arr_rch_especial[k].pesos = 0
      END IF

      --calcula AIVS97
      LET arr_rch_especial[k].aivs97 = (arr_rch_especial[k].pesos / v_precio_fondo) 
      
      LET k = k + 1

   END FOREACH
   
   --Elimina última fila en blanco
   IF(arr_rch_especial[arr_rch_especial.getLength()].diagnostico IS NULL) THEN
      CALL arr_rch_especial.deleteElement(arr_rch_especial.getLength())
   END IF

END FUNCTION 

REPORT genera_PDF()

   DEFINE v_f_presenta     DATE
   DEFINE v_desc_operacion CHAR(60)
   DEFINE f                INTEGER 

   FORMAT 
   FIRST PAGE HEADER
      LET v_f_presenta = TODAY
      LET v_desc_operacion = "Recepción rechazo solicitud sdo uso 43Bis"  

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
      PRINTX r_total_global.pesos            
      PRINTX r_total_global.porc_concatena   
      --Total solicitudes nuevas
      PRINTX r_total_sol_nueva.t_registros    
      PRINTX r_total_sol_nueva.aivs97         
      PRINTX r_total_sol_nueva.pesos          
      PRINTX r_total_sol_nueva.porc_concatena 
      --Total adelantos
      PRINTX r_total_adelantos.t_registros    
      PRINTX r_total_adelantos.aivs97         
      PRINTX r_total_adelantos.pesos          
      PRINTX r_total_adelantos.porc_concatena 
      --Total especial
      PRINTX r_total_especial.t_registros     
      PRINTX r_total_especial.aivs97          
      PRINTX r_total_especial.pesos           
      PRINTX r_total_especial.porc_concatena
   
   ON EVERY ROW
      #Rechazos

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
      
END REPORT  

FUNCTION genera_archivo_salida()

   DEFINE v_qry_salida    STRING
   DEFINE v_detalle       STRING
   DEFINE v_ruta_archivo  STRING 
   DEFINE archivo         base.Channel
   
   DEFINE r_arh_op01      RECORD
      f_envio     DATE, 
      nss         CHAR(11),
      aivs97      DECIMAL(13,2),
      pesos97     DECIMAL(13,2),
      p_pago      CHAR(6),
      estado      SMALLINT,
      tipo        CHAR(2),
      num_intento CHAR(4),
      causal_rch  CHAR(3)
   END RECORD  

   LET v_arh_salida   = "Detalle_43Bis01_",TODAY USING "yyyymmdd",".cgt" CLIPPED 
   LET v_ruta_archivo = v_ruta_envio CLIPPED,"/",v_arh_salida

   LET v_qry_salida = "SELECT h.f_proceso,
                              a.nss,
                              u.importe_v97,
                              u.periodo_pago,
                              u.estado,
                              u.diagnostico
                         FROM cre_uso_garantia u,
                              afi_derechohabiente a,
                              cre_ctr_archivo h
                        WHERE u.edo_procesar = 90
                          AND u.id_derechohabiente = a.id_derechohabiente
                          AND u.id_cre_ctr_archivo = h.id_cre_ctr_archivo 
                          AND u.id_cre_ctr_archivo = ",v_d_id_cre_ctr_arch

   PREPARE prp_arh_salida FROM v_qry_salida
   DECLARE crs_arh_salida CURSOR FOR prp_arh_Salida

   LET archivo = base.Channel.create()
   CALL archivo.openFile(v_ruta_archivo,"w")

   LET r_arh_op01.f_envio     = NULL 
   LET r_arh_op01.nss         = NULL 
   LET r_arh_op01.aivs97      = 0
   LET r_arh_op01.pesos97     = 0
   LET r_arh_op01.p_pago      = NULL
   LET r_arh_op01.estado      = NULL
   LET r_arh_op01.tipo        = NULL
   LET r_arh_op01.num_intento = "   1" 
   LET r_arh_op01.causal_rch  = NULL
   LET v_detalle              = NULL 
   
   FOREACH crs_arh_salida INTO r_arh_op01.f_envio,
                                r_arh_op01.nss,
                                r_arh_op01.pesos97,
                                r_arh_op01.p_pago,
                                r_arh_op01.estado,
                                r_arh_op01.causal_rch

      IF(r_arh_op01.pesos97 IS NULL) THEN
         LET r_arh_op01.pesos97 = 0
      END IF 
      
      --calcula aivs
      LET r_arh_op01.aivs97 = (r_arh_op01.pesos97/v_precio_fondo)

      CASE 
         WHEN  r_arh_op01.estado = 20
            LET r_arh_op01.tipo  = "UG"
         WHEN  r_arh_op01.estado = 140
            LET r_arh_op01.tipo  = "CA"
         WHEN  r_arh_op01.estado = 142
            LET r_arh_op01.tipo  = "CE"
      END CASE 

      LET v_detalle = r_arh_op01.f_envio USING "yyyymmdd",
                      r_arh_op01.nss,
                      r_arh_op01.aivs97,
                      r_arh_op01.pesos97,
                      r_arh_op01.p_pago,
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

   DEFINE p_diagnostico        LIKE cre_uso_garantia.diagnostico,
          p_edo_procesar       LIKE cre_uso_garantia.edo_procesar,
          v_desc_diagnostico   LIKE cat_rechazo.desc_rechazo,
          v_tpo_rechazo        LIKE cat_rechazo.tpo_rechazo

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
   DEFINE v_d_id_cre_ctr_arch   LIKE cre_ctr_archivo.id_cre_ctr_archivo, -- identificador del archivo
          v_extencion           LIKE cat_operacion.extension,
          v_s_qryTxt            STRING -- guarda una sentencia SQL a ejecutar

   -- se obtiene la extensión del proceso y operación
   LET v_s_qryTxt = " SELECT extension\n",
                    "   FROM cat_operacion\n",
                    "  WHERE proceso_cod = ",p_i_proceso_cod,"\n",
                    "    AND opera_cod = ",p_i_opera_cod

   PREPARE prp_obt_extension FROM v_s_qryTxt
   EXECUTE prp_obt_extension INTO v_extencion

   -- se obtiene el maximo pid para el proceso, operación y extensión del archivo
   LET v_s_qryTxt = " SELECT MAX(id_cre_ctr_archivo)",
                    "   FROM cre_ctr_archivo",
                    "  WHERE id_proceso = ",g_id_proceso_grt_uso,
                    "    AND operacion = 01",--rechazos
                    "    AND estado = 20",
                    "    AND nom_archivo LIKE '%.",v_extencion CLIPPED,"'"

   PREPARE prp_cons FROM v_s_qryTxt
   EXECUTE prp_cons INTO v_d_id_cre_ctr_arch

   -- si el identificador es nulo se asigna cero
   IF v_d_id_cre_ctr_arch IS NULL THEN 
      LET v_d_id_cre_ctr_arch = 0
   END IF

   RETURN v_d_id_cre_ctr_arch
END FUNCTION 
