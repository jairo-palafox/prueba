--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

####################################################################
#Modulo            =>AGR                                           #
#Programa          =>AGRE10                                        #
#Objetivo          =>Programa para integrar el archivo de          #
#                    Confirmación DSE para el modulo de            #
#                    Anualidades Garantizadas                      #
#Autor             =>Daniel Buendia, EFP                           #
#Fecha inicio      =>01 Febrero 2012                               #
####################################################################

DATABASE safre_viv

DEFINE p_v_usuario       LIKE seg_usuario.usuario, -- nombre del usuario
       p_d_pid           LIKE bat_ctr_proceso.pid, -- pid
       p_i_proceso_cod   LIKE cat_proceso.proceso_cod, -- codigo del proceso
       p_i_opera_cod     LIKE cat_operacion.opera_cod, -- codigo de la operacion
       p_d_folio         LIKE glo_ctr_archivo.folio, -- numero de folio
       p_v_arch_proceso  VARCHAR(100) -- nombre del archivo a integrar

#Objetivo: Funcion que realiza la integracion del archivo de rechazo devolucion saldos
MAIN
   DEFINE v_s_qryTxt         STRING, -- guarda una sentencia SQL a ejecutar
          v_v_nom_reporte    VARCHAR(80), -- nombre del reporte
          v_s_mens_correo    STRING, -- contiene el cuerpo del correo
          v_s_titulo_correo  STRING, -- contiene el titulo del correo
          v_s_archivo_correo STRING, -- ruta y nombre del archivo adjunto en el correo
          v_r_dse_ctr_arch   RECORD
             tot_registros   LIKE dse_ctr_archivo.tot_registros, -- numero total de registros
             tot_aceptados   LIKE dse_ctr_archivo.tot_aceptados, -- numero de registros aceptados
             tot_rechazados  LIKE dse_ctr_archivo.tot_rechazados -- numero de registros rechazados
          END RECORD,
          v_s_comando        STRING, -- contiene al comando a correr
          v_c_ruta_list_bat  LIKE seg_modulo.ruta_listados, -- ruta listados de bat
          r_c_ruta_bin       LIKE seg_modulo.ruta_bin, -- ruta del bin de cta
          r_c_ruta_listados  LIKE seg_modulo.ruta_listados, -- ruta listados cta
          r_b_existe_err     BOOLEAN, -- indica si ocurrio un error durante un subproceso
          r_b_valida         SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario      = ARG_VAL(1)
   LET p_d_pid          = ARG_VAL(2)
   LET p_i_proceso_cod  = ARG_VAL(3)
   LET p_i_opera_cod    = ARG_VAL(4)
   LET p_d_folio        = ARG_VAL(5)
   LET p_v_arch_proceso = ARG_VAL(6)   

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".AGRE10.log")

   DISPLAY "=INICIA AGRE10="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " ARCHIVO:      : ",p_v_arch_proceso

   -- se genera el folio
   LET p_d_folio = fn_genera_folio(p_i_proceso_cod, p_i_opera_cod, p_v_usuario)
   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"

   -- se invoca la funcion que crea la tabla temporal a insertar los registros del proceso
   CALL fn_crea_tmp_conf_devol_saldo()

   -- se inicializan variables
   LET r_b_existe_err = FALSE -- se asume que no ocurrirá error en el subproceso

   -- se obtiene la ruta bin y de listados del módulo
   CALL fn_rutas("agr") RETURNING r_c_ruta_bin, r_c_ruta_listados

   DISPLAY " INTEGRA CONF DEV SDOS EXC"
   -- se crea la sentencia que ejecuta el procedure que realiza la integracion de rechazo de saldos
   LET v_s_qryTxt = "EXECUTE PROCEDURE safre_viv:fn_dse_integra_confdse_agr(?,?,?,?)"

   PREPARE prp_integra_rech_saldo FROM v_s_qryTxt
   EXECUTE prp_integra_rech_saldo USING p_d_pid, p_v_usuario, p_v_arch_proceso, p_d_folio
                                   INTO r_b_valida

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      DISPLAY " Ocurrió un ERROR durante la integración: ",r_b_valida
      -- se marca el proceso como erroneo
      LET r_b_valida = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

      -- vericia el regreso de la funcion
      IF r_b_valida <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF

   -- se muestran las cifras de control para información del archivo
   LET v_s_qryTxt = " SELECT tot_registros, tot_aceptados, tot_rechazados\n",
                    "   FROM dse_ctr_archivo\n",
                    "  WHERE folio = ",p_d_folio

   PREPARE prp_ccontrol_dse FROM v_s_qryTxt
   EXECUTE prp_ccontrol_dse INTO v_r_dse_ctr_arch.*

   DISPLAY "TOTAL REGISTROS : ",v_r_dse_ctr_arch.tot_registros
   DISPLAY "TOTAL ACEPTADOS : ",v_r_dse_ctr_arch.tot_aceptados
   DISPLAY "TOTAL RECHAZADOS: ",v_r_dse_ctr_arch.tot_rechazados
{
   -- se invoca la función que deja la operación en estado Finalizado
   LET r_b_valida = fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF
}
   -- se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat1 FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat1 INTO v_c_ruta_list_bat

   DISPLAY " Se ejecuta el proceso de conciliación"
   -- se crea el comando que ejecuta la conciliación de Confirmación de devolución
   LET v_s_comando = " nohup time fglrun ",r_c_ruta_bin CLIPPED,"/AGRP23 ",
                                           p_v_usuario, " ",
                                           p_d_pid, " ",
                                           p_i_proceso_cod, " ",
                                           p_i_opera_cod, " ",
                                           p_d_folio, " ",
                                           p_v_arch_proceso,
                                           " 1>> ",v_c_ruta_list_bat CLIPPED,
                                           "/nohup:",p_d_pid USING "&&&&&",":",
                                           p_i_proceso_cod USING "&&&&&",":",
                                           p_i_opera_cod USING "&&&&&",
                                           " 2>&1 &"

   --DISPLAY v_s_comando
   RUN v_s_comando
{
   DISPLAY " GENERA REPORTE CIFRAS CONTROL"
   -- se asigna el nombre del reporte
   LET v_v_nom_reporte = p_v_usuario CLIPPED, "-AGRL28-", p_d_pid USING "&&&&&", "-", p_i_proceso_cod USING "&&&&&", "-", p_i_opera_cod USING "&&&&&"

   -- se invoca la funcion que genera el reporte del proceso de Intergración
   CALL f_genera_rpt_IntegraConfDSE(r_c_ruta_listados, v_v_nom_reporte) RETURNING r_b_existe_err

   -- en caso de no haber ocurrido error al generar el reporte se envía éste por correo
   IF NOT r_b_existe_err THEN
      DISPLAY " ENVIA CORREO DEL REPORTE"
      -- se asigna el titulo del correo
      LET v_s_titulo_correo = "Proceso: RECEPCIÓN CONFIRMACIÓN SALDOS EXC AGR"

      -- se asigna el archivo a adjuntar
      LET v_s_archivo_correo = r_c_ruta_listados CLIPPED||"/"||v_v_nom_reporte CLIPPED||".pdf"

      -- se asigna el cuerpo del correo
      LET v_s_mens_correo =  "ID Proceso   : ",p_d_pid,"\n",
                             "Proceso      : RECEPCIÓN CONFIRMACIÓN SALDOS EXC AGR\n",
                             "Operacion    : INTEGRA CONFIRMACIÓN DEVOLUCIÓN SDO EXC\n",
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
}
   DISPLAY "=FIN="
END MAIN
{
#Objetivo: Función que genera el reporte de Integración de recurrente
FUNCTION f_genera_rpt_IntegraConfDSE(p_c_ruta_listados, p_v_nom_reporte)
   DEFINE p_c_ruta_listados   LIKE seg_modulo.ruta_listados, -- ruta listados cta
          p_v_nom_reporte     VARCHAR(80), -- nombre del reporte
          v_r_rpt_res         RECORD -- registro de resumen
             folio            INTEGER, -- numero de folio con formato
             nom_archivo      LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
             fecha_hr_ini     LIKE bat_ctr_operacion.fecha_ini, -- fecha inicial del proceso
             fecha_hr_fin     LIKE bat_ctr_operacion.fecha_fin, -- fecha final de la operacion
             usuario          LIKE bat_ctr_operacion.usuario, -- nombre del usuario
             tot_registros    INTEGER, -- numero total de registros
             tot_aceptados    INTEGER, -- numero total de regs aceptados
             tot_rechazados   INTEGER -- numero total de regs rechazados
          END RECORD,
          v_r_dse_ctr_arch    RECORD LIKE dse_ctr_archivo.*, -- registro de dse ctr archivo
          v_r_bat_ctr_opera   RECORD LIKE bat_ctr_operacion.*, -- registro de bat ctr operación
          v_manejador_rpt     OM.SaxDocumentHandler, # Contenedor de Documentos para el reporte
          v_s_qryTxt          STRING, -- contiene una sentencia sql a ejecutar
          v_b_existe_err      BOOLEAN -- indica si ocurrió error durante la generación del reporte

   -- se inicializan variables
   LET v_b_existe_err = FALSE -- se asume que no ocurrirá error

   -- se indica que el reporte usara la plantilla creada
   IF fgl_report_loadCurrentSettings("AGRE101.4rp") THEN
      -- se indica la salida del reporte
      CALL fgl_report_setOutputFileName(p_c_ruta_listados CLIPPED||"/"||p_v_nom_reporte)

      -- sin indica que no es necesario el preview
      CALL fgl_report_selectPreview(0)

      -- se asigna la configuración en el menejo del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()
   ELSE
      DISPLAY "ERROR: No fue posible abrir plantilla del reporte"
      -- se indica que ha ocurrido un error y no continua
      LET v_b_existe_err = TRUE

      RETURN v_b_existe_err
   END IF

   -- se crea la sentencia sql que busca la información del archivo cargado
   LET v_s_qryTxt = " SELECT *\n",
                    " FROM dse_ctr_archivo\n",
                    " WHERE folio = ",p_d_folio

   PREPARE prp_cre_ctr_arch FROM v_s_qryTxt
   EXECUTE prp_cre_ctr_arch INTO v_r_dse_ctr_arch.*

   -- se crea la sentencia sql que busca la información de la operación
   LET v_s_qryTxt = " SELECT *\n",
                    "   FROM bat_ctr_operacion\n",
                    "  WHERE folio       = ",p_d_folio,"\n",
                    "    AND pid         = ",p_d_pid,"\n",
                    "    AND proceso_cod = ",p_i_proceso_cod,"\n",
                    "    AND opera_cod   = ",p_i_opera_cod

   PREPARE prp_bat_ctr_opera FROM v_s_qryTxt
   EXECUTE prp_bat_ctr_opera INTO v_r_bat_ctr_opera.*

   -- se asignan los valores del registro del reporte
   LET v_r_rpt_res.nom_archivo    = v_r_bat_ctr_opera.nom_archivo
   LET v_r_rpt_res.fecha_hr_ini   = v_r_bat_ctr_opera.fecha_ini
   LET v_r_rpt_res.fecha_hr_fin   = v_r_bat_ctr_opera.fecha_fin
   LET v_r_rpt_res.usuario        = v_r_bat_ctr_opera.usuario
   LET v_r_rpt_res.tot_registros  = v_r_dse_ctr_arch.tot_registros
   LET v_r_rpt_res.tot_aceptados  = v_r_dse_ctr_arch.tot_aceptados
   LET v_r_rpt_res.tot_rechazados = v_r_dse_ctr_arch.tot_rechazados
   LET v_r_rpt_res.folio          = p_d_folio

   -- inicia el reporte de registros con rechazo
   START REPORT reporte_integra_conf_dse TO XML HANDLER v_manejador_rpt

   -- salida del reporte
   OUTPUT TO REPORT reporte_integra_conf_dse(v_r_rpt_res.*)

   -- finaliza el reporte
   FINISH REPORT reporte_integra_conf_dse

   RETURN v_b_existe_err
END FUNCTION
}
{
#OBJETIVO: Genera el reporte de Integración de Recurrente
REPORT reporte_integra_conf_dse(p_r_res)
   DEFINE p_r_res           RECORD
             folio          INTEGER, -- numero de folio con formato
             nom_archivo    LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
             fecha_hr_ini   LIKE bat_ctr_operacion.fecha_ini, -- fecha inicial del proceso
             fecha_hr_fin   LIKE bat_ctr_operacion.fecha_fin, -- fecha final de la operacion
             usuario        LIKE bat_ctr_operacion.usuario, -- nombre del usuario
             tot_registros  INTEGER, -- numero total de registros
             tot_aceptados  INTEGER, -- numero total de regs aceptados
             tot_rechazados INTEGER -- numero total de regs rechazados
          END RECORD,
          v_fecha_reporte   DATE

   FORMAT

   FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY
      PRINTX v_fecha_reporte USING "DD-MM-YYYY"
      PRINTX p_v_usuario
      PRINTX p_r_res.folio
      PRINTX p_r_res.nom_archivo
      PRINTX p_r_res.fecha_hr_ini
      PRINTX p_r_res.fecha_hr_fin
      PRINTX p_r_res.usuario
      PRINTX p_r_res.tot_registros USING "#########&"
      PRINTX p_r_res.tot_aceptados USING "#########&"
      PRINTX p_r_res.tot_rechazados USING "#########&"
END REPORT
}
#Objetivo: Función que crea la tabla temporal de la integración de confirmación devolucion saldos
FUNCTION fn_crea_tmp_conf_devol_saldo()
   -- se especifica que la base a utilizar es la temporal
   DATABASE safre_tmp

   -- en caso de error continua
   WHENEVER ERROR CONTINUE

   -- se elimina la tabla temporal
   DROP TABLE tmp_deudor_conf_devol_saldo_agr

   -- al encontrar un error detiene el programa
   WHENEVER ERROR STOP

   CREATE TABLE tmp_deudor_conf_devol_saldo_agr(id_dse_devolucion  DECIMAL(9,0),
                                                id_derechohabiente DECIMAL(9,0),
                                                nss                CHAR(11),
                                                folio_archivo      INTEGER)

   -- regresamos a la base de datos safre viv
   DATABASE safre_viv
END FUNCTION
