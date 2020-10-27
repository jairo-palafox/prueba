--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

#########################################################################
#Modulo:           =>AGR                                                #
#Programa:         =>AGRP13                                             #
#Objetivo:         =>Programa para generar las marcas de los registros  #
#                    aceptados en el paso de integración de Uso         #
#                    Anualidad del módulo Anualidades Garantizadas      #
#Autor:            =>Daniel Buendia, EFP                                #
#Fecha inicio:     =>24 Mayo 2012                                       #
#Autor modifica    =>Emilio Abarca, EFP                                 #
#Fecha modifica    =>29 Agosto 2017                                     #
#########################################################################

DATABASE safre_viv

GLOBALS 

   DEFINE p_v_usuario         LIKE seg_usuario.usuario     -- nombre del usuario
   DEFINE p_d_pid             LIKE bat_ctr_proceso.pid     -- pid
   DEFINE p_i_proceso_cod     LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE p_i_opera_cod       LIKE cat_operacion.opera_cod -- codigo de la operacion
   DEFINE p_d_folio           LIKE cre_ctr_archivo.folio_archivo -- numero de folio
   DEFINE p_v_arch_proceso    VARCHAR(100)                      -- nombre del archivo a integrar
   DEFINE p_d_id_cre_ctr_arch LIKE cre_ctr_archivo.id_cre_ctr_archivo -- identificador de la tabla de control
   DEFINE v_c_programa_cod    LIKE bat_ctr_operacion.programa_cod     -- nombre del programa
   DEFINE v_i_tot_reg_marca   INTEGER  -- total de registros marcados
   DEFINE v_i_edo_marcaje     SMALLINT -- estado de marcaje
   DEFINE v_s_qryTxt          STRING   -- guarda una sentencia SQL a ejecutar
   DEFINE r_b_valida          SMALLINT -- booleana que indica si el proceso se puede ejecutar o no
         
   -- Variables para el reporte
   DEFINE v_v_nom_reporte     STRING  -- nombre del reporte 
   DEFINE v_s_titulo_correo   STRING 
   DEFINE v_s_archivo_correo  STRING 
   DEFINE g_c_ruta_bin        LIKE seg_modulo.ruta_bin      -- ruta del bin del módulo
   DEFINE g_c_ruta_listados   LIKE seg_modulo.ruta_listados -- ruta de listados del módulo
   DEFINE v_s_mens_correo     STRING
   DEFINE v_s_comando         STRING 
   DEFINE v_c_ruta_list_bat   CHAR(40)

     -- Arreglo para causales de rechazo
   DEFINE arr_rch_agr DYNAMIC ARRAY OF RECORD
      estado          SMALLINT,
      t_registros     INTEGER,
      monto_pesos     DECIMAL(14,2),
      cve_causal      SMALLINT,
      desc_causal     CHAR(40)
   END RECORD 
   
END GLOBALS 

MAIN
  
   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario         = ARG_VAL(1)
   LET p_d_pid             = ARG_VAL(2)
   LET p_i_proceso_cod     = ARG_VAL(3)
   LET p_i_opera_cod       = ARG_VAL(4)
   LET p_d_folio           = ARG_VAL(5)
   LET p_v_arch_proceso    = ARG_VAL(6)
   LET p_d_id_cre_ctr_arch = ARG_VAL(7)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".AGRP13.log")

   DISPLAY "=INICIA AGRP13="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"
   DISPLAY " ARCHIVO:      : ",p_v_arch_proceso
   DISPLAY " ID CTR ARCHIVO: ",p_d_id_cre_ctr_arch
   DISPLAY ""

   -- recupera la ruta de listados en el que se enviara el archivo
   CALL fn_rutas("agr") RETURNING g_c_ruta_bin, g_c_ruta_listados

   -- Recupera directorio BAT
   SELECT ruta_listados
     INTO v_c_ruta_list_bat
     FROM seg_modulo
    WHERE modulo_cod = 'bat'
   
   -- se inicializan variables
   LET v_c_programa_cod = "AGRP13"

   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(p_d_pid,p_i_proceso_cod,p_i_opera_cod) RETURNING r_b_valida

   -- se verifica si la operación fue o no valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se invoca la función que deja la operación en estado Procesando
   LET r_b_valida = fn_actualiza_opera_ini(p_d_pid,p_i_proceso_cod,p_i_opera_cod,
                                           p_d_folio, v_c_programa_cod,
                                           p_v_arch_proceso, p_v_usuario)

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   DISPLAY ""
   DISPLAY "=> SE PROCESA LA MARCA"
   -- se crea sentencia que ejecuta PROCEDURE que inserta los registros integrados en cta crédito
   LET v_s_qryTxt = "EXECUTE FUNCTION safre_viv:fn_uso_procesa_marca_cuenta(?,?,?,?)"

   PREPARE prp_procesa_marca_cuenta FROM v_s_qryTxt
   EXECUTE prp_procesa_marca_cuenta USING p_v_usuario,
                                          p_d_folio,
                                          p_d_id_cre_ctr_arch,
                                          p_i_proceso_cod
                                     INTO v_i_edo_marcaje

   -- verifica si ocurrió un error durante el proceos de marcaje
   IF v_i_edo_marcaje <> 0 THEN
      -- Ocurrió un error, se muestra el error
      DISPLAY "OCURRIÓ UN ERROR EN EL PROCESO DE MARCAJE: ",v_i_edo_marcaje

      -- ocurrió un error y se marca como rechazado la operación
      LET r_b_valida = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

      EXIT PROGRAM
   END IF

   -- se realiza el conteo de los registros marcados
   SELECT COUNT(*)
     INTO v_i_tot_reg_marca
     FROM sfr_marca_activa
    WHERE id_derechohabiente IN (
          SELECT UNIQUE id_derechohabiente
            FROM cre_uso_garantia
           WHERE id_cre_ctr_archivo = p_d_id_cre_ctr_arch)

   DISPLAY "TOTAL DE REGISTROS MARCADOS: ",v_i_tot_reg_marca
   DISPLAY ""

   -- se invoca la función que deja la operación en estado Finalizado
   LET r_b_valida = fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      -- ocurrió un error y se marca como rechazado la operación
      LET r_b_valida = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

      --EXIT PROGRAM
   ELSE 
      -- Genera PDF al finalizar la operación para poder obtener la fecha de término
      DISPLAY "=> GENERA REPORTE PDF"
      DISPLAY ""
   
      -- se asigna el nombre del reporte
      LET v_v_nom_reporte = g_c_ruta_listados CLIPPED ,"/",p_v_usuario CLIPPED, "-", v_c_programa_cod CLIPPED,"-", p_d_pid USING "&&&&&", "-", p_i_proceso_cod USING "&&&&&", "-", p_i_opera_cod USING "&&&&&",".pdf"

      -- se invoca la funcion que genera el reporte
      CALL f_genera_rpt_IntegRecurr()

      DISPLAY ""
      DISPLAY "=> ENVIA CORREO DEL REPORTE"
      DISPLAY ""

      -- se asigna el titulo del correo
      LET v_s_titulo_correo = "Proceso: RECEPCIÓN DE USO DE ANUALIDAD"

      -- se asigna el archivo a adjuntar
      LET v_s_archivo_correo = v_v_nom_reporte

      -- se asigna el cuerpo del correo
      LET v_s_mens_correo =  "ID Proceso   : ",p_d_pid,"\n",
                             "Proceso      : RECEPCIÓN  USO ANUALIDADES GARANTIZADAS\n",
                             "Fecha Inicio : ",TODAY,"\n",
                             "Fecha Fin    : ",TODAY

      -- se invoca la función que envía por correo el elemento generado
      CALL fn_correo_proceso(p_d_pid,
                             p_i_proceso_cod,
                             p_i_opera_cod,
                             v_s_archivo_correo,
                             v_s_titulo_correo,
                             v_s_mens_correo)      
   END IF

   DISPLAY ""
   DISPLAY "=> EJECUTA OPERACIÓN GENERA ARCHIVO DE RECHAZOS"

   LET p_i_opera_cod = 4 -- Resetea operación, para lanzar la última

   LET v_s_comando = " nohup time fglrun ",g_c_ruta_bin CLIPPED,"/AGRS13 ",p_v_usuario," ",p_d_pid," ",
                                           p_d_id_cre_ctr_arch," 1>  ",v_c_ruta_list_bat CLIPPED,
                                           "/nohup:",p_d_pid USING "&&&&&",":",p_i_proceso_cod USING "&&&&&",":",
                                           p_i_opera_cod USING "&&&&&"," 2>&1 &"

   RUN v_S_comando
   DISPLAY ""
   
   DISPLAY "=FIN="
   
END MAIN

#Objetivo: Función que genera el reporte de Integración Uso Anualidad
FUNCTION f_genera_rpt_IntegRecurr()

   DEFINE v_reporte         STRING
   DEFINE cont              INTEGER     -- contador para causal de rechazos
   
   DEFINE r_inf_report  RECORD 
      nom_archivo    CHAR(40),      -- nombre del archivo
      id_operacion   SMALLINT,      -- operacion
      fecha_hr_ini   CHAR(25),      -- fecha inicial del proceso
      fecha_hr_fin   CHAR(25),      -- fecha final de la operacion
      desc_operacion CHAR(40),      -- descripción de la operación
      tot_registros  INTEGER ,      -- Total de registros cargados
      monto_total    DECIMAL(14,2), -- Suma monto de todos los registros cargados
      porc_total     CHAR(6),       -- Porcentaje total registros cargados
      tot_aceptados  INTEGER ,      -- numero total de regs aceptados
      monto_acept    DECIMAL(14,2), -- Suma pesos  regs. aceptados
      prc_acept      CHAR(6),       -- Porcentaje total regs. aceptados
      tot_rechazados INTEGER ,      -- numero total de regs. rechazados
      monto_rch      DECIMAL(14,2), -- Suma pesos regs. rechazados
      prc_rch        CHAR(6)        -- Porcentaje regs. rechazados
   END RECORD
   
   DEFINE v_manejador_rpt   om.SaxDocumentHandler 
   DEFINE v_i_folio_format  INTEGER  -- numero de folio con formato
   DEFINE v_s_qryTxt        STRING       
   DEFINE v_porcentaje      DECIMAL(3,0)

   LET v_reporte    = g_c_ruta_bin CLIPPED,"/AGRP131.4rp"
   LET v_porcentaje = 0
   
    -- se asignan los valores del registro del reporte
   LET v_i_folio_format  = p_d_folio --USING "&&&&&&&&&&"
   
   -- se crea la sentencia sql que busca la información del archivo cargado
   LET v_s_qryTxt = " SELECT nom_archivo,operacion,tot_registros\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE id_cre_ctr_archivo = ",p_d_id_cre_ctr_arch

   PREPARE prp_cre_ctr_arch FROM v_s_qryTxt
   EXECUTE prp_cre_ctr_arch INTO r_inf_report.nom_archivo,
                                  r_inf_report.id_operacion,
                                  r_inf_report.tot_registros

   -- se crea la sentencia sql que busca la información de la operación
   LET v_s_qryTxt = " SELECT fecha_ini,fecha_fin\n",
                    "   FROM bat_ctr_operacion\n",
                    "  WHERE proceso_cod = ",p_i_proceso_cod,"\n",
                    "    AND opera_cod = ",p_i_opera_cod,"\n",
                    "    AND folio = ",p_d_folio

   PREPARE prp_bat_ctr_opera FROM v_s_qryTxt
   EXECUTE prp_bat_ctr_opera INTO r_inf_report.fecha_hr_ini,
                                   r_inf_report.fecha_hr_fin

   -- Obtiene descripción de la operación
   LET r_inf_report.desc_operacion = fn_obt_desc_operacion(r_inf_report.id_operacion)
  
   -- Obtiene monto(pesos) total de todos los registros cargados
   SELECT SUM(importe_v97)
     INTO r_inf_report.monto_total
     FROM cre_uso_garantia
    WHERE id_cre_ctr_archivo = p_d_id_cre_ctr_arch

   IF(r_inf_report.monto_total IS NULL) THEN
      LET r_inf_report.monto_total = 0 
   END IF 

   -- Calcula porcentaje del total de registros cargados
   LET v_porcentaje = (r_inf_report.tot_registros / r_inf_report.tot_registros) * 100 
   LET r_inf_report.porc_total = v_porcentaje CLIPPED,"%"
    
   -- Total registros aceptados con marca y suma monto pesos solicitado
   SELECT SUM (importe_v97),COUNT(*)
     INTO r_inf_report.monto_acept,
          r_inf_report.tot_aceptados
     FROM cre_uso_garantia
    WHERE id_cre_ctr_archivo = p_d_id_cre_ctr_arch
      AND estado = 20;

   IF(r_inf_report.monto_acept IS NULL) THEN
      LET r_inf_report.monto_acept = 0 
   END IF 

   -- Calcula porcentaje aceptados
   LET v_porcentaje  = 0
   LET v_porcentaje  = (r_inf_report.tot_aceptados / r_inf_report.tot_registros) * 100
   LET r_inf_report.prc_acept = v_porcentaje CLIPPED,"%"

   -- Recupera total registros rechazados
   SELECT SUM (importe_v97),COUNT(*)
     INTO r_inf_report.monto_rch,
          r_inf_report.tot_rechazados
     FROM cre_uso_garantia
    WHERE id_cre_ctr_archivo = p_d_id_cre_ctr_arch
      AND estado <> 20;

   IF(r_inf_report.monto_rch IS NULL) THEN
      LET r_inf_report.monto_rch = 0 
   END IF 
      
   -- Calcula porcentaje rechazados
   LET v_porcentaje  = 0
   LET v_porcentaje  = (r_inf_report.tot_rechazados / r_inf_report.tot_registros) * 100
   LET r_inf_report.prc_rch = v_porcentaje CLIPPED,"%"

   -- Inicializa arreglo en caso de no encontrar rechazos
   LET arr_rch_agr[1].estado      = NULL 
   LET arr_rch_agr[1].t_registros = 0
   LET arr_rch_agr[1].monto_pesos = 0
   LET arr_rch_agr[1].cve_causal  = NULL 
   LET arr_rch_agr[1].desc_causal = NULL 

  
   -- Recupera rregistros por el rechazo 240
   DECLARE crs_rch_agr_240 CURSOR FOR 
      SELECT u.estado,
              u.diagnostico,
              r.desc_estado,
         SUM(importe_v97),
              COUNT(*)
         FROM cre_uso_garantia u,
              cat_rch_acreditado r
       WHERE u.id_cre_ctr_archivo = p_d_id_cre_ctr_arch
         AND u.estado = 240 
         AND u.diagnostico = r.estado  
         GROUP BY 1,2,3;

   LET cont = 1
  
   FOREACH crs_rch_agr_240 INTO arr_rch_agr[cont].estado,
                                 arr_rch_agr[cont].cve_causal,
                                 arr_rch_agr[cont].desc_causal,
                                 arr_rch_agr[cont].monto_pesos,
                                 arr_rch_agr[cont].t_registros
          
      LET cont = cont + 1
      
   END FOREACH

   -- Recupera registros por el rechazo 150
   DECLARE crs_rch_agr_150 CURSOR FOR 
      SELECT u.estado,
              q.estado_desc,
         SUM(importe_v97),
              COUNT(*)
        FROM cre_uso_garantia u,
             cat_maq_credito q 
      WHERE u.id_cre_ctr_archivo = p_d_id_cre_ctr_arch
        AND u.estado = 150
        AND u.estado = q.estado
        GROUP BY 1,2;  

   FOREACH crs_rch_agr_150 INTO arr_rch_agr[cont].estado,
                                 arr_rch_agr[cont].desc_causal,
                                 arr_rch_agr[cont].monto_pesos,
                                 arr_rch_agr[cont].t_registros

      -- Para la causal de rechazo se deja el estado
      LET arr_rch_agr[cont].cve_causal = arr_rch_agr[cont].estado
      
      LET cont = cont + 1
      
   END FOREACH 

   -- Elimina fila en blanco
   IF(arr_rch_agr[arr_rch_agr.getLength()].estado IS NULL) THEN
      CALL arr_rch_agr.deleteElement(arr_rch_agr.getLength()) 
   END IF 

   #CONFIGURACIÓN DEL REPORTE
   IF (fgl_report_loadCurrentSettings(v_reporte)) THEN
      CALL fgl_report_selectDevice ("PDF")
      CALL fgl_report_selectPreview(0)
      CALL fgl_report_setOutputFileName(v_v_nom_reporte)
      
       -- se asigna la configuración en el manejo del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()

      IF (v_manejador_rpt IS NOT NULL) THEN
         -- inicia el reporte de registros con rechazo
         START REPORT reporte_integ_recurr TO XML HANDLER v_manejador_rpt

         OUTPUT TO REPORT reporte_integ_recurr(r_inf_report.*, v_i_folio_format)

         -- finaliza el reporte
        FINISH REPORT reporte_integ_recurr
      END IF
   ELSE
      DISPLAY "No fue posible generar el reporte"
      EXIT PROGRAM
   END IF 
   
END FUNCTION

#Objetivo: Genera el reporte de Integración de Uso Anualidad
REPORT reporte_integ_recurr(p_rec_inf_report, p_i_folio)

  DEFINE p_rec_inf_report  RECORD 
      nom_archivo    CHAR(40),      -- nombre del archivo
      id_operacion   SMALLINT,      -- operacion
      fecha_hr_ini   CHAR(25),      -- fecha inicial del proceso
      fecha_hr_fin   CHAR(25),      -- fecha final de la operacion
      desc_operacion CHAR(40),      -- descripción de la operación
      tot_registros  INTEGER ,      -- Total de registros cargados
      monto_total    DECIMAL(14,2), -- Suma monto de todos los registros cargados
      porc_total     CHAR(6),       -- Porcentaje total registros cargados
      tot_aceptados  INTEGER ,      -- numero total de regs aceptados
      monto_acept    DECIMAL(14,2), -- Suma pesos  regs. aceptados
      prc_acept      CHAR(6),       -- Porcentaje total regs. aceptados
      tot_rechazados INTEGER ,      -- numero total de regs. rechazados
      monto_rch      DECIMAL(14,2), -- Suma pesos regs. rechazados
      prc_rch        CHAR(6)        -- Porcentaje regs. rechazados
   END RECORD
   DEFINE p_i_folio       VARCHAR(10) -- numero de folio con formato
   DEFINE v_fecha_reporte DATE
   DEFINE z               INTEGER     

   FORMAT

   FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY

      #ENCABEZADO
      PRINTX p_v_usuario
      PRINTX p_i_folio
      PRINTX v_fecha_reporte USING "DD-MM-YYYY"
      #DETALLE
      PRINTX p_rec_inf_report.nom_archivo
      PRINTX p_rec_inf_report.fecha_hr_ini
      PRINTX p_rec_inf_report.fecha_hr_fin
      PRINTX p_rec_inf_report.desc_operacion
      PRINTX p_rec_inf_report.tot_registros
      PRINTX p_rec_inf_report.monto_total
      PRINTX p_rec_inf_report.porc_total
      PRINTX p_rec_inf_report.tot_aceptados
      PRINTX p_rec_inf_report.monto_acept
      PRINTX p_rec_inf_report.prc_acept
      PRINTX p_rec_inf_report.tot_rechazados
      PRINTX p_rec_inf_report.monto_rch
      PRINTX p_rec_inf_report.prc_rch
      
   ON EVERY ROW
   
      FOR z= 1 TO arr_rch_agr.getLength()
         -- Imprime valores del arreglo
         PRINTX arr_rch_agr[z].t_registros
         PRINTX arr_rch_agr[z].monto_pesos
         PRINTX arr_rch_agr[z].cve_causal
         PRINTX arr_rch_agr[z].desc_causal
      END FOR 
      
END REPORT

#Objetivo: Busca la descripción de la operación
FUNCTION fn_obt_desc_operacion(p_c_operacion)
   DEFINE p_c_operacion  LIKE cat_operacion_prc.operacion, -- operación
          v_c_desc_opera LIKE cat_operacion_prc.desc_operacion, -- descripción de la operación
          v_s_qryTxt      STRING -- se asigna consulta sql a ejecutar

   -- se consulta la descripción del estado
   LET v_s_qryTxt = " SELECT desc_operacion\n",
                    "   FROM cat_operacion_prc\n",
                    "  WHERE operacion = '",p_c_operacion,"'"

   PREPARE prp_desc_operacion FROM v_s_qryTxt
   EXECUTE prp_desc_operacion INTO v_c_desc_opera

   -- se verifica si se encontró descripción
   IF v_c_desc_opera IS NULL THEN
      LET v_c_desc_opera = "DESCRIPCIÓN NO ENCONTRADA"
   END IF

   RETURN v_c_desc_opera
END FUNCTION