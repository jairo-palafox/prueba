--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

#########################################################################
#Modulo            =>GRT                                                #
#Programa          =>GRTP13                                             #
#Objetivo          =Programa que realiza la conciliación de la informa- #
#                   ción de deudor vs tmp deudor saldo transferidos     #
#                   para el módulo de Uso de Garantia 43 bis            #
#Autor             =>Daniel Buendia, EFP                                #
#Fecha inicio      =>26 Abril 2012                                      #
#Autor modifica    =>Emilio Abarca, EFP                                 #
#Fecha modifica    =>05 Marzo 2018                                      #
#Desc. modifica    =>Adecuación al reporte PDF y generación de archivo  #
#                    de salida para los saldos transferidos.            #
#########################################################################

DATABASE safre_viv
GLOBALS "GRTG01.4gl"

DEFINE p_v_usuario           LIKE seg_usuario.usuario,                # Nombre del usuario
        p_d_pid               LIKE bat_ctr_proceso.pid,                # pid
        p_i_proceso_cod       LIKE cat_proceso.proceso_cod,            # Código del proceso
        p_i_opera_cod         LIKE cat_operacion.opera_cod,            # Código de la operacion
        p_d_folio             LIKE glo_ctr_archivo.folio,              # Número de folio
        p_v_arch_proceso      VARCHAR(100),                           # Nombre del archivo a integrar
        v_c_programa_cod      LIKE cat_operacion.programa_cod,         -- nombrel del programa
        v_d_id_cre_ctr_arch   LIKE cre_ctr_archivo.id_cre_ctr_archivo, -- id del archiovo
        v_r_cre_ctr_arch      RECORD LIKE cre_ctr_archivo.*,          -- registro de cre ctr archivo
        v_r_bat_ctr_opera     RECORD LIKE bat_ctr_operacion.*,        -- registro de bat ctr operación
        v_ruta_bin            LIKE seg_modulo.ruta_bin,
        v_si_operacion        LIKE cre_ctr_archivo.operacion,         -- operacion del proceso
        v_s_mens_correo       STRING,                                 -- contiene el cuerpo del correo
        v_s_titulo_correo     STRING,                                 -- contiene el titulo del correo
        v_s_archivo_correo    STRING,                                 -- ruta y nombre del archivo adjunto en el correo 
        r_b_valida            SMALLINT
   DEFINE v_precio_fondo     DECIMAL(19,14)
   DEFINE v_f_movimiento     DATE
   DEFINE v_ruta_rpt         STRING                         #Ruta y nombre del reporte PDF a adjuntar
   DEFINE v_ruta_listados    LIKE seg_modulo.ruta_listados
   DEFINE v_arh_salida       STRING 
   DEFINE v_ruta_envio       CHAR(40)
   
   --Record y arreglos para el reporte
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

   DEFINE v_s_qryTxt      STRING
   DEFINE v_d_pid_arch    LIKE bat_ctr_operacion.pid
   DEFINE v_reporte_bin   STRING
   DEFINE v_manejador_rpt OM.SaxDocumentHandler -- Contenedor de Documentos para el reporte

# Objetivo: Conciliar la información de Saldos Transferidos de Uso de Garantía 43 bis
MAIN

   # Se recuperan los parámetros que envia el programa lanzador
   LET p_v_usuario       = ARG_VAL(1)
   LET p_d_pid           = ARG_VAL(2)
   LET p_i_proceso_cod   = ARG_VAL(3)
   LET p_i_opera_cod     = ARG_VAL(4)
   LET p_d_folio         = ARG_VAL(5)
   LET p_v_arch_proceso  = ARG_VAL(6)

   # se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".GRTP13.log")

   DISPLAY " "
   DISPLAY "=INICIA GRTP13="
   DISPLAY "= CONCILIACIÓN DE SALDOS TRANSFERIDOS GRT ="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"
   DISPLAY " ARCHIVO       : ",p_v_arch_proceso
   DISPLAY " "

   -- se inicializan variables
   LET v_si_operacion = 9 -- Saldos Transferidos

   -- recupera la ruta de listados en el que se enviara el archivo
   CALL fn_rutas("grt") RETURNING v_ruta_bin, v_ruta_listados

   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'grt';

     -- se consulta el folio del archivo
   LET v_s_qryTxt = " SELECT MAX(id_cre_ctr_archivo)\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE id_proceso = ",g_id_proceso_grt_uso,"\n",
                    "    AND operacion = ",v_si_operacion

   PREPARE prp_folio_archivo FROM v_s_qryTxt
   EXECUTE prp_folio_archivo INTO v_d_id_cre_ctr_arch

   DISPLAY " Identificador Archivo: ",v_d_id_cre_ctr_arch
   DISPLAY ""

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

   LET v_d_pid_arch = fn_max_pid(g_proc_cod_grt_uso_sdos_transf, 2)

   CALL obtiene_info_rpt()
   DISPLAY " > OBTIENE INFORMACIÓN PARA EL REPORTE PDF ...COMPLETADO"
   DISPLAY ""
   CALL genera_archivo_salida()
   DISPLAY " > GENERA ARCHIVO DE SALIDA ...COMPLETADO"
   DISPLAY " El archivo de salida se ha generado en /safreviv_int/grt/envio"
   DISPLAY " con nombre: ",v_arh_salida
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
                    "    AND proceso_cod = ",g_proc_cod_grt_uso_sdos_transf,"\n",
                    "    AND opera_cod = 2"

   PREPARE prp_bat_ctr_opera FROM v_s_qryTxt
   EXECUTE prp_bat_ctr_opera INTO v_r_bat_ctr_opera.*

   #################################################
   #   CONFIGURACIÓN PARA SALIDA DEL REPORTE PDF   #
   #################################################   

   LET v_reporte_bin = v_ruta_bin CLIPPED,"/GRTP131.4rp"
   LET v_ruta_rpt    = v_ruta_listados CLIPPED,"/",
                       p_v_usuario CLIPPED,"-GRTL20-",
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

   DISPLAY " ENVIA CORREO DEL REPORTE"
   -- se asigna el titulo del correo
   LET v_s_titulo_correo = "Proceso: CONCILIACIÓN DE SALDOS TRANSFERIDOS DE USO DE GTÍA 43 BIS"
   -- se asigna el archivo a adjuntar
   LET v_s_archivo_correo = v_ruta_rpt
   -- se asigna el cuerpo del correo
   LET v_s_mens_correo =  "ID Proceso   : ",p_d_pid,"\n",
                          "Proceso      : CONCILIACIÓN\n",
                          "Operacion    : SALDOS TRANSFERIDOS USO GRT\n",
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

{
   -- se asigna el nombre del programa segun el lanzador del proceso para crear el
   -- nombre del reporte y que este se pueda mostrar en el monitor de procesos
   IF p_i_proceso_cod = g_proc_cod_grt_uso_cocilia THEN
      LET v_c_programa_cod = "GRTL29" -- Conciliación
   ELSE
      LET v_c_programa_cod = "GRTL20" -- Integración Saldos
   END IF 
}
  
END MAIN

FUNCTION obtiene_info_rpt()

   DEFINE cont                   INTEGER 
   DEFINE k                      INTEGER  
   DEFINE v_aux_porcentaje       DECIMAL(6,2)
   
   DEFINE r_op09      RECORD
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
   INITIALIZE r_op09.* TO NULL 

   DECLARE crs_op09 CURSOR FOR
   SELECT id_cre_uso_garantia,
           id_derechohabiente,
           importe_v97,
           estado
     FROM cre_uso_garantia 
    WHERE edo_procesar = 115 --DEVUELTA
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
   
   FOREACH crs_op09 INTO r_op09.id_cre_uso_garantia,
                          r_op09.id_derechohabiente,
                          r_op09.importe_v97,
                          r_op09.estado

      IF(r_op09.importe_v97 IS NULL) THEN
         LET r_op09.importe_v97 = 0
      END IF 
      
      --contabiliza todos los registros para totales globales
      LET r_total_global.t_registros       = r_total_global.t_registros  + 1
      LET r_total_global.pesos             = r_total_global.pesos + r_op09.importe_v97
    
      --Evalua que tipo de solicitud es
      CASE
         --Solicitudes nuevas
         WHEN r_op09.estado = 20
            LET r_total_sol_nueva.t_registros = r_total_sol_nueva.t_registros + 1
            LET r_total_sol_nueva.pesos       = r_total_sol_nueva.pesos + r_op09.importe_v97
         --Adelantos
         WHEN r_op09.estado = 140
            LET r_total_adelantos.t_registros = r_total_adelantos.t_registros + 1
            LET r_total_adelantos.pesos       = r_total_adelantos.pesos + r_op09.importe_v97
         --Especial
         WHEN r_op09.estado = 142
            LET r_total_especial.t_registros  = r_total_especial.t_registros + 1
            LET r_total_especial.pesos        = r_total_especial.pesos + r_op09.importe_v97

      END CASE 
      
      LET cont = cont + 1
      
   END FOREACH 

   --En este caso la f_movimiento para devoluciones no es reportada por PROCESAR
   --por lo que se asigna la del día de ejecución del proceso.
   {LET v_f_movimiento = TODAY}
   
   --Obtiene la f_movimiento para calcular el valor de fondo
   SELECT MAX(f_movimiento)
     INTO v_f_movimiento
     FROM cre_uso_garantia
    WHERE edo_procesar = 115
      AND id_cre_ctr_archivo = v_d_id_cre_ctr_arch

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
   
END FUNCTION 

REPORT genera_PDF()

   DEFINE v_f_presenta     DATE
   DEFINE v_desc_operacion CHAR(60)

   FORMAT 
   FIRST PAGE HEADER
      LET v_f_presenta = TODAY
      LET v_desc_operacion = "Recepción Saldos Transferidos Uso 43BIS"  

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
      
END REPORT  


FUNCTION genera_archivo_salida()

   DEFINE v_qry_salida    STRING
   DEFINE v_detalle       STRING
   DEFINE v_ruta_archivo  STRING 
   DEFINE archivo         base.Channel
   
   DEFINE r_arh_op09      RECORD
      f_envio     DATE, 
      nss         CHAR(11),
      aivs97      DECIMAL(13,2),
      pesos97     DECIMAL(13,2),
      p_pago      CHAR(6),
      estado      SMALLINT,
      tipo        CHAR(2)
   END RECORD  

   LET v_arh_salida   = "Detalle_43Bis09_",TODAY USING "yyyymmdd",".cgt" CLIPPED 
   LET v_ruta_archivo = v_ruta_envio CLIPPED,"/",v_arh_salida

   LET v_qry_salida = "SELECT h.f_proceso,
                              a.nss,
                              u.importe_v97,
                              u.periodo_pago,
                              u.estado
                         FROM cre_uso_garantia u,
                              afi_derechohabiente a,
                              cre_ctr_archivo h
                        WHERE u.id_derechohabiente = a.id_derechohabiente
                          AND u.edo_procesar = 115
                          AND u.id_cre_ctr_archivo = h.id_cre_ctr_archivo 
                          AND u.id_cre_ctr_archivo = ",v_d_id_cre_ctr_arch

   PREPARE prp_arh_salida FROM v_qry_salida
   DECLARE crs_arh_salida CURSOR FOR prp_arh_Salida

   LET archivo = base.Channel.create()
   CALL archivo.openFile(v_ruta_archivo,"w")

   LET r_arh_op09.f_envio     = NULL 
   LET r_arh_op09.nss         = NULL 
   LET r_arh_op09.aivs97      = 0
   LET r_arh_op09.pesos97     = 0
   LET r_arh_op09.p_pago      = NULL
   LET r_arh_op09.estado      = NULL
   LET r_arh_op09.tipo        = NULL
   LET v_detalle              = NULL 
   
   FOREACH crs_arh_salida INTO r_arh_op09.f_envio,
                                r_arh_op09.nss,
                                r_arh_op09.pesos97,
                                r_arh_op09.p_pago,
                                r_arh_op09.estado

      IF(r_arh_op09.pesos97 IS NULL) THEN
         LET r_arh_op09.pesos97 = 0
      END IF 

      --calcula aivs
      LET r_arh_op09.aivs97 = (r_arh_op09.pesos97 / v_precio_fondo)

      CASE 
         WHEN  r_arh_op09.estado = 20
            LET r_arh_op09.tipo  = "UG"
         WHEN  r_arh_op09.estado = 140
            LET r_arh_op09.tipo  = "CA"
         WHEN  r_arh_op09.estado = 142
            LET r_arh_op09.tipo  = "CE"
      END CASE 

      LET v_detalle = r_arh_op09.f_envio USING "yyyymmdd",
                      r_arh_op09.nss,
                      r_arh_op09.aivs97,
                      r_arh_op09.pesos97,
                      r_arh_op09.p_pago,
                      r_arh_op09.tipo

      --Escribe en archivo de salida
      CALL archivo.writeLine(v_detalle)

   END FOREACH 

   CALL archivo.close()

END FUNCTION

{
# Objetivo: Conciliar la información de Saldos Transferidos
MAIN
   DEFINE p_v_usuario           LIKE seg_usuario.usuario,     # Nombre del usuario
          p_d_pid               LIKE bat_ctr_proceso.pid,     # pid
          p_i_proceso_cod       LIKE cat_proceso.proceso_cod, # Código del proceso
          p_i_opera_cod         LIKE cat_operacion.opera_cod, # Código de la operacion
          p_d_folio             LIKE glo_ctr_archivo.folio,   # Número de folio
          p_v_arch_proceso      VARCHAR(100),                 # Nombre del archivo a integrar
          v_c_programa_cod     LIKE bat_ctr_operacion.programa_cod, -- nombrel del programa
          v_r_registro          RECORD
             movimiento         LIKE cre_saldo_deudor.movimiento, # Movimiento
             mvto_desc          LIKE cat_movimiento.movimiento_desc, # Descripcion de movimiento
             sum_monto_aivs     LIKE cre_saldo_deudor.monto_aivs, # Suma de monto aivs
             sum_monto_pesos    LIKE cre_saldo_deudor.monto_pesos # Suma de monto pesos
          END RECORD,
          --v_ide_derechohabiente LIKE cre_deudor.id_derechohabiente,
          v_c_subcuenta_desc    LIKE cat_subcuenta.subcuenta_desc, -- descripción de la subcuenta
          v_manejador_rpt       OM.SaxDocumentHandler, # Contenedor de Documentos para el reporte
          v_s_qryTxt            STRING, # guarda una sentencia SQL a ejecutar
          v_s_mens_correo       STRING, -- contiene el cuerpo del correo
          v_s_titulo_correo     STRING, -- contiene el titulo del correo
          v_s_archivo_correo    STRING, -- ruta y nombre del archivo adjunto en el correo 
          v_edit_folio          INTEGER,
          v_nom_reporte         VARCHAR(80), -- nombre del reporte
          r_ruta_bin            LIKE seg_modulo.ruta_bin,
          r_ruta_listados       LIKE seg_modulo.ruta_listados,
          r_b_valida            SMALLINT

   # Se recuperan los parámetros que envia el programa lanzador
   LET p_v_usuario       = ARG_VAL(1)
   LET p_d_pid           = ARG_VAL(2)
   LET p_i_proceso_cod   = ARG_VAL(3)
   LET p_i_opera_cod     = ARG_VAL(4)
   LET p_d_folio         = ARG_VAL(5)
   LET p_v_arch_proceso  = ARG_VAL(6)

   # Se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".GRTP13.log")

   DISPLAY "=INICIA GRTP13="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"
   DISPLAY " ARCHIVO       : ",p_v_arch_proceso

   # Recupera la ruta de listados en el que se enviara el archivo
   CALL fn_rutas("grt") RETURNING r_ruta_bin, r_ruta_listados

   -- se indentifica el programa lanzador para poder nombrar el reporte
   IF p_i_proceso_cod = g_proc_cod_grt_uso_cocilia THEN
      LET v_c_programa_cod = "GRTL29" -- Conciliación
   ELSE
      LET v_c_programa_cod = "GRTL20" -- Integración Saldos
   END IF

   DISPLAY " CREA REPORTE CONCILIACION "
   # Se indica que el reporte usara la plantilla creada
   IF fgl_report_loadCurrentSettings("GRTP131.4rp") THEN  -- if  the file loaded OK
      -- se crea el nombre del reporte
      LET v_nom_reporte = p_v_usuario CLIPPED || "-",v_c_programa_cod CLIPPED,"-", p_d_pid USING "&&&&&", "-", p_i_proceso_cod USING "&&&&&", "-", p_i_opera_cod USING "&&&&&"
      CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_nom_reporte)

      -- se indica que no es necesaria la vista previa
      CALL fgl_report_selectPreview(0)

      -- se guarda la configuración en el manejador del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()      -- commit the file settings
   ELSE
      DISPLAY "NO FUE POSIBLE GENERAR EL REPORTE"
      CALL fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod) 
                         RETURNING r_b_valida
                                     
      IF(r_b_valida <> 0)THEN
         # En caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF
      EXIT PROGRAM
   END IF
   
   -- se asigna el folio con formato a la variable 
   LET v_edit_folio =  p_d_folio

   -- se lee la descripción de la subcuenta 4- vivienda 97
   LET v_s_qryTxt = " SELECT subcuenta_desc\n",
                    " FROM cat_subcuenta\n",
                    " WHERE subcuenta = 4"

   PREPARE prp_subcuenta_desc FROM v_s_qryTxt
   EXECUTE prp_subcuenta_desc INTO v_c_subcuenta_desc

   # Inicia el reporte de registros con rechazo
   START REPORT reporte_saldos_transf TO XML HANDLER v_manejador_rpt

   --# Se obtienen todos los registros de deudor con movimiento (41, 42, 52 y 252)
   --LET v_s_qryTxt = " SELECT a.movimiento, b.movimiento_desc, SUM(a.monto_aivs), SUM(a.monto_pesos)\n",
   --                 "   FROM cre_saldo_deudor a, cat_movimiento b,safre_tmp:tmp_deudor_saldo_grt t\n",                    
   --                 "  WHERE a.id_cre_acreditado = t.id_cre_acreditado\n",
   --                 "    AND a.movimiento = b.movimiento ",
   --                 "  GROUP BY 1, 2"
                    
   # Se obtienen todos los registros de deudor con movimiento (41, 42, 52 y 252)
   LET v_s_qryTxt = " SELECT a.movimiento, b.movimiento_desc, SUM(a.monto_aivs), SUM(a.monto_pesos)\n",
                    "  FROM cre_saldo_deudor a, cat_movimiento b\n",
                    " WHERE a.id_cre_acreditado IN (\n",
                    "        SELECT id_cre_uso_garantia\n",
                    "         FROM cre_uso_garantia\n",
                    "        WHERE id_derechohabiente IN (\n",
                    "              SELECT id_derechohabiente\n",
                    "                FROM safre_tmp:tmp_deudor_saldo_grt))\n",
                    "   AND a.movimiento = b.movimiento\n",
                    " GROUP BY 1, 2"

   PREPARE prp_slc_mov_sumAivs_sumPesos FROM v_s_qryTxt
   DECLARE cur_slc_mov_sumAivs_sumPesos CURSOR FOR prp_slc_mov_sumAivs_sumPesos

   FOREACH cur_slc_mov_sumAivs_sumPesos INTO v_r_registro.* 
                                             --v_ide_derechohabiente
      # salida del reporte
      OUTPUT TO REPORT reporte_saldos_transf(v_r_registro.*, p_v_usuario, v_edit_folio, v_c_subcuenta_desc)
   END FOREACH
   
   # Si ocurrió algun error en la consulta, se cambia el estado de la operacion en erronea
   IF(SQLCA.SQLCODE <> 0)THEN
      DISPLAY "ERROR EN CONSULTA (CODIGO):"||SQLCA.SQLCODE
      DISPLAY "MENSAJE SQL:"||SQLCA.SQLERRM
      CALL fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod) 
                         RETURNING r_b_valida
                                     
      IF(r_b_valida <> 0)THEN
         # En caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF
   END IF

   #Finaliza el reporte
   FINISH REPORT reporte_saldos_transf

   # Actualiza la operacion a finalizada
   CALL fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, p_i_opera_cod) 
                              RETURNING r_b_valida
   IF(r_b_valida <> 0)THEN
      # En caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)
      CALL fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod) 
                         RETURNING r_b_valida
                                     
      IF(r_b_valida <> 0)THEN
         # En caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF
   END IF

   DISPLAY " ENVIA CORREO DEL REPORTE"
   -- se asigna el titulo del correo
   LET v_s_titulo_correo = "Proceso: CONCILIACIÓN DE SALDOS USO DE ANUALIDAD 43 BIS"

   -- se asigna el archivo a adjuntar
   LET v_s_archivo_correo = r_ruta_listados CLIPPED||"/"||v_nom_reporte CLIPPED||".pdf"

   -- se asigna el cuerpo del correo
   LET v_s_mens_correo =  "ID Proceso   : ",p_d_pid,"\n",
                          "Proceso      : CONCILIACIÓN\n",
                          "Operacion    : SALDOS TRANSFERIDOS USO DE ANUALIDAD 43 BIS\n",
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
}
{
#OBJETIVO: Genera el reporte de la conciliación de Saldos Transferidos
REPORT reporte_saldos_transf(p_r_registro, p_v_usuario, p_edit_folio, p_c_subcuenta_desc)
DEFINE p_r_registro       RECORD
          movimiento      LIKE cre_saldo_deudor.movimiento,       # Movimiento
          mvto_desc       LIKE cat_movimiento.movimiento_desc, # Descripcion de movimiento
          sum_monto_aivs  LIKE cre_saldo_deudor.monto_aivs,       # Suma de monto aivs
          sum_monto_pesos LIKE cre_saldo_deudor.monto_pesos       # Suma de monto pesos
       END RECORD,
       p_v_usuario        LIKE seg_usuario.usuario_cod, 
       p_edit_folio       INTEGER,
       p_c_subcuenta_desc LIKE cat_subcuenta.subcuenta_desc, -- descripción de la subcuenta
       v_fecha_present    LIKE dis_sum_avance_pago.f_presentacion,
       v_num_registros    INTEGER,
       v_sum_aivs         DECIMAL(22,6),
       v_sum_pesos        DECIMAL(22,6),
       v_fecha_reporte    DATE
       --p_titulo           STRING--CHAR (100)

   FORMAT

   FIRST PAGE HEADER
      LET v_sum_aivs = 0
      LET v_sum_pesos = 0
      LET v_num_registros = 0
      LET v_fecha_reporte = TODAY
      LET v_fecha_present = v_fecha_reporte
      PRINTX v_fecha_reporte USING "dd-mm-yyyy" 
      PRINTX p_edit_folio
      PRINTX v_fecha_present USING "dd-mm-yyyy"
      PRINTX p_v_usuario

   ON EVERY ROW
      LET v_sum_pesos = v_sum_pesos + p_r_registro.sum_monto_pesos
      LET v_sum_aivs = v_sum_aivs + p_r_registro.sum_monto_aivs
      LET v_num_registros = v_num_registros + 1 
      PRINTX p_r_registro.*

   ON LAST ROW
      PRINTX p_c_subcuenta_desc
      PRINTX v_num_registros 
      PRINTX v_sum_pesos
      PRINTX v_sum_aivs

END REPORT
}