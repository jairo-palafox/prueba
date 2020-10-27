--===============================================================
-- Versión: 1.0.0
-- Fecha última modificación:
--===============================================================

##########################################################################
#Modulo       => ACR                                                     #
#Programa     => ACRP38                                                  #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la #
#                agrupación para DSE devoluciones                        #
#Fecha inicio => Febrero 3, 2012                                         #
#Fecha inicio => 12 de febrero de 2016                                   #
#                Mauro Muñiz Caballero                                   #
#                Se agrega agrupación por Un cuarto mas "ucm"            #
#Fecha inicio => 16 de enero de 2018                                     #
#                Mauro Muñiz Caballero                                   #
#                Se agrega agrupación por crédito liquidado "ral"        #
##########################################################################

DATABASE safre_viv

GLOBALS

   DEFINE p_usuario_cod             LIKE seg_usuario.usuario_cod  -- clave del usuario firmado
   DEFINE p_pid                     LIKE bat_ctr_proceso.pid  -- ID del proceso
   DEFINE p_proceso_cod             LIKE cat_proceso.proceso_cod  -- código del proceso
   DEFINE p_opera_cod               LIKE cat_operacion.opera_cod  -- código de operación
   DEFINE p_folio                   LIKE glo_folio.folio  -- folio referencia
   DEFINE p_nombre_archivo          LIKE glo_ctr_archivo.nombre_archivo  -- nombre dle archivo

END GLOBALS

MAIN

   DEFINE v_c_tpo_transf            LIKE dse_agrupa_devolucion.tpo_transferencia  -- tipo de transferencia

   DEFINE v_r_mto_agrup RECORD
      sum_aivs97                    DECIMAL(24,6), -- suma aivs 97
      sum_pesos97                   DECIMAL(22,2), -- suma pesos 97
      sum_aivs92                    DECIMAL(24,6), -- suma aivs 92
      sum_pesos92                   DECIMAL(22,2), -- suma pesos 92
      num_registros                 INTEGER -- número de registros
   END RECORD

   DEFINE v_s_sql                   STRING -- cadena con una instruccion SQL
   DEFINE v_i_resultado             INTEGER -- resultado del proceso
   DEFINE v_bnd_fin_oper            SMALLINT
   DEFINE v_v_nom_reporte           VARCHAR(80) -- nombre del reporte
   DEFINE v_s_mens_correo           STRING -- contiene el cuerpo del correo
   DEFINE v_s_titulo_correo         STRING -- contiene el titulo del correo
   DEFINE v_s_archivo_correo        STRING -- ruta y nombre del archivo adjunto en el correo
   DEFINE v_s_qryTxt                STRING -- guarda una sentencia SQL a ejecutar
   DEFINE v_c_proceso_desc          LIKE cat_proceso.proceso_desc -- descripción del proceso
   DEFINE v_c_opera_desc            LIKE cat_operacion.opera_desc -- descripción de la operacion
   DEFINE v_c_programa_cod          LIKE cat_operacion.programa_cod -- nombrel del programa origen
   DEFINE r_ruta_ejecutable         LIKE seg_modulo.ruta_bin -- ruta del bin
   DEFINE r_ruta_listados           LIKE seg_modulo.ruta_listados -- ruta de los listados

   -- se recuperan los parametros que envia el programa lanzador
   -- usuario, pid, proceso_cod, opera_cod, folio, nombre_archivo
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)   

   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".ACRP38.log")

   DISPLAY "=INICIA ACRP38="
   DISPLAY " USUARIO       : ",p_usuario_cod
   DISPLAY " PID           : ",p_pid
   DISPLAY " ARCHIVO       : ",p_nombre_archivo

   -- se inicializan variables
   LET v_i_resultado  = 0 -- se asume que el proceso termina correctamente
   LET v_c_tpo_transf = "15" -- 15-DSE Transferencia de Acreditados

   -- se genera el folio
   LET p_folio = fn_genera_folio(p_proceso_cod, p_opera_cod, p_usuario_cod)

   DISPLAY " FOLIO         : ",p_folio USING "#########&"

   -- se hace el despliegue de la línea que indica que el proceso ha comenzado
   CALL fn_display_proceso(0, "AGRUPACION DE DSE")

   DISPLAY " EJECUTA PROCESO DE AGRUPACIÓN"
   -- se contruye el enuncionado SQL
   LET v_s_sql = "EXECUTE FUNCTION fn_dse_agrupa_devolucion(?,?,?)"

   -- se prepara la ejecucion del stored procedure para la preliquidacion
   PREPARE sid_preliquidadse FROM v_s_sql
   EXECUTE sid_preliquidadse USING p_folio,
                                   p_usuario_cod,
                                   v_c_tpo_transf
                              INTO v_i_resultado 
            
   IF ( v_i_resultado <> 0 ) THEN
      -- verificando si el error fue por carencia de valor del fondo
      IF v_i_resultado = -1 THEN
         DISPLAY "NO EXISTE VALOR DE AIV PARA LA FECHA ACTUAL"
      ELSE
         DISPLAY "OCURRIÓ UN ERROR DURANTE LA EJECUCIÓN DE AGRUPACIÓN DSE: ", v_i_resultado
      END IF

      -- ocurrio un error al terminar el stored procedure
      LET v_i_resultado = fn_error_opera(p_pid, p_proceso_cod, p_opera_cod)

      -- se verifica si fue posible marcar como error la operacion
      IF v_i_resultado <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(v_i_resultado)

         --EXIT PROGRAM
      END IF

      EXIT PROGRAM
   END IF

   -- se actualiza el folio en la tabla de control de operaciones
   LET v_s_qryTxt = " UPDATE bat_ctr_operacion\n",
                    "    SET folio = ",p_folio,"\n",
                    "  WHERE pid = ",p_pid,"\n",
                    "    AND proceso_cod = ",p_proceso_cod,"\n",
                    "    AND opera_cod = ",p_opera_cod

   PREPARE prp_actualiza_folio FROM v_s_qryTxt
   EXECUTE prp_actualiza_folio

   -- se invoca la finalizacion de la operacion
   CALL fn_actualiza_opera_fin(p_pid, p_proceso_cod, p_opera_cod) RETURNING v_bnd_fin_oper

   -- se verifica si fue posible finalizar la operacion
   IF v_bnd_fin_oper <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(v_bnd_fin_oper)

      EXIT PROGRAM
   END IF

   -- se consulta los montos agrupados
   LET v_s_sql = " SELECT SUM(aivs97), SUM(pesos97), SUM(aivs92), SUM(pesos92), COUNT(*)\n",
                 "   FROM dse_agrupa_devolucion\n",
                 "  WHERE tpo_transferencia = ",v_c_tpo_transf,"\n",
                 "    AND id_dse_grp_devolucion IN (\n",
                 "        SELECT id_dse_grp_devolucion\n",
                 "          FROM dse_his_devolucion\n",
                 "         WHERE tpo_transferencia = ",v_c_tpo_transf,"\n",
                 "           AND folio = ",p_folio,")"

   PREPARE prp_monto_agrupado FROM v_s_sql
   EXECUTE prp_monto_agrupado INTO v_r_mto_agrup.*

   DISPLAY "Núm. Registros: ",v_r_mto_agrup.num_registros
   DISPLAY "Monto Aivs97  : ",v_r_mto_agrup.sum_aivs97
   DISPLAY "Monto Pesos97 : ",v_r_mto_agrup.sum_pesos97
   DISPLAY "Monto Aivs92  : ",v_r_mto_agrup.sum_aivs92
   DISPLAY "Monto Pesos92 : ",v_r_mto_agrup.sum_pesos92

   DISPLAY " GENERA EL REPORTE DE AGRUPACIÓN"
   -- se obtienen las descripciones del proceso y la operacion
   LET v_c_proceso_desc = fn_proceso_cod_desc(p_proceso_cod)
   LET v_c_opera_desc   = fn_opera_cod_desc(p_proceso_cod, p_opera_cod)

   -- se obtiene la ruta de los listados
   CALL fn_rutas("acr") RETURNING r_ruta_ejecutable, r_ruta_listados

   -- se obtiene el nombrel del programa correspondiente
   LET v_c_programa_cod = fn_obten_nom_programa(p_proceso_cod , p_opera_cod)

   -- se asigna el nombre del reporte
   LET v_v_nom_reporte = p_usuario_cod CLIPPED || "-", v_c_programa_cod CLIPPED,"-",p_pid USING "&&&&&","-",p_proceso_cod USING "&&&&&", "-", p_opera_cod USING "&&&&&"

   -- genera el reporte de la agrupación
   CALL fn_gen_rpt_agrupacion(v_v_nom_reporte, r_ruta_listados, v_c_tpo_transf, p_folio)

   DISPLAY " ENVIA CORREO DEL REPORTE"
   -- se asigna el titulo del correo
   LET v_s_titulo_correo = "Proceso: ",v_c_proceso_desc

   -- se asigna el archivo a adjuntar
   LET v_s_archivo_correo = r_ruta_listados CLIPPED||"/"||v_v_nom_reporte CLIPPED||".pdf"

   -- se asigna el cuerpo del correo
   LET v_s_mens_correo =  "ID Proceso   : ",p_pid,"\n",
                          "Proceso      : ",v_c_proceso_desc CLIPPED,"\n",
                          "Operacion    : ",v_c_opera_desc CLIPPED,"\n",
                          "Fecha Inicio : ",TODAY,"\n",
                          "Fecha Fin    : ",TODAY

   -- se invoca la función que envía por correo el elemento generado
   CALL fn_correo_proceso(p_pid,
                          p_proceso_cod,
                          p_opera_cod,
                          v_s_archivo_correo,
                          v_s_titulo_correo,
                          v_s_mens_correo)

END MAIN

#Objetivo: Función que genera el reporte de agrupación
FUNCTION fn_gen_rpt_agrupacion(p_v_nom_reporte, p_ruta_listados, p_c_tpo_transf, p_d_folio)

   DEFINE p_v_nom_reporte           VARCHAR(80) -- nombre del reporte
   DEFINE p_ruta_listados           LIKE seg_modulo.ruta_listados -- ruta de los listados
   DEFINE p_c_tpo_transf            LIKE dse_ctr_archivo.tpo_transferencia -- tipo de transferencia
   DEFINE p_d_folio                 LIKE glo_folio.folio -- folio referencia

   DEFINE v_r_rpt_detalle RECORD
      nom_archivo                   LIKE dse_ctr_archivo.nom_archivo, -- nombre del archivo
      folio_archivo                 LIKE dse_ctr_archivo.folio, -- folio del archivo
      subcuenta                     LIKE cat_subcuenta.subcuenta, -- subcuenta
      subcuenta_desc                LIKE cat_subcuenta.subcuenta_desc, -- descripción de la subcuenta
      num_registros                 INTEGER, -- número de registros a agrupar
      monto_aivs                    DECIMAL(18,6), -- suma de aivs
      monto_pesos                   DECIMAL(22,2) -- suma de pesos
   END RECORD

   DEFINE v_r_agrup RECORD
      tot_reg_dse                   INTEGER, -- Número de registros DSE
      aivs97_dse                    DECIMAL(22,6), -- AIVS 97 DSE
      pesos97_dse                   DECIMAL(22,2), -- Pesos 97 DSE
      aivs92_dse                    DECIMAL(22,6), -- AIVS 92 DSE
      pesos92_dse                   DECIMAL(22,2), -- Pesos 92 DSE
      tot_reg_sep                   INTEGER, -- Número de registros SEP
      aivs97_sep                    DECIMAL(22,6), -- AIVS 97 SEP
      pesos97_sep                   DECIMAL(22,2), -- Pesos 97 SEP
      aivs92_sep                    DECIMAL(22,6), -- AIVS 92 SEP
      pesos92_sep                   DECIMAL(22,2), -- Pesos 92 SEP
      tot_reg_dis                   INTEGER, -- Número de registros DIS
      aivs97_dis                    DECIMAL(22,6), -- AIVS 97 DIS
      pesos97_dis                   DECIMAL(22,2), -- Pesos 97 DIS
      aivs92_dis                    DECIMAL(22,6), -- AIVS 92 DIS
      pesos92_dis                   DECIMAL(22,2), -- Pesos 92 DIS
      tot_reg_mjv                   INTEGER, -- Número de registros MJV
      aivs97_mjv                    DECIMAL(22,6), -- AIVS 97 MJV
      pesos97_mjv                   DECIMAL(22,2), -- Pesos 97 MJV
      aivs92_mjv                    DECIMAL(22,6), -- AIVS 92 MJV
      pesos92_mjv                   DECIMAL(22,2), -- Pesos 92 MJV
      tot_reg_mao                   INTEGER, -- Número de registros MAO
      aivs97_mao                    DECIMAL(22,6), -- AIVS 97 MAO
      pesos97_mao                   DECIMAL(22,2), -- Pesos 97 MAO
      aivs92_mao                    DECIMAL(22,6), -- AIVS 92 MAO
      pesos92_mao                   DECIMAL(22,2), -- Pesos 92 MAO
      tot_reg_ucm                   INTEGER, -- Número de registros UCM
      aivs97_ucm                    DECIMAL(22,6), -- AIVS 97 UCM
      pesos97_ucm                   DECIMAL(22,2), -- Pesos 97 UCM
      aivs92_ucm                    DECIMAL(22,6), -- AIVS 92 UCM
      pesos92_ucm                   DECIMAL(22,2), -- Pesos 92 UCM
      tot_reg_ral                   INTEGER, -- Número de registros RAL
      aivs97_ral                    DECIMAL(22,6), -- AIVS 97 RAL
      pesos97_ral                   DECIMAL(22,2), -- Pesos 97 RAL
      aivs92_ral                    DECIMAL(22,6), -- AIVS 92 RAL
      pesos92_ral                   DECIMAL(22,2), -- Pesos 92 RAL
      tot_reg_tot                   INTEGER, -- Número de registros TOT
      aivs97_tot                    DECIMAL(22,6), -- Total AIVS 97
      pesos97_tot                   DECIMAL(22,2), -- Total Pesos 97
      aivs92_tot                    DECIMAL(22,6), -- Total AIVS 92
      pesos92_tot                   DECIMAL(22,2) -- Total Pesos 92
   END RECORD

   DEFINE v_report_handler          om.SaxDocumentHandler -- handler para el reporte en PDF
   DEFINE v_s_qryTxt                STRING -- se asigna sentencia sql a ejecutar

   -- se indica que el reporte usara la plantilla creada
   IF ( fgl_report_loadCurrentSettings("ACRP381.4rp") ) THEN
      -- se indica que se escriba en archivo
      CALL fgl_report_setOutputFileName(p_ruta_listados CLIPPED||"/"||p_v_nom_reporte CLIPPED)

      -- sin indica que no es necesario el preview
      CALL fgl_report_selectPreview(0)

      -- se asigna la configuración en el menejador del reporte
      LET v_report_handler = fgl_report_commitCurrentSettings()
   ELSE
      DISPLAY "NO SE PUDO LEER LA PLANTILLA DEL REPORTE"

      EXIT PROGRAM
   END IF

   -- se inicia el reporte
   START REPORT rpt_agrupacion_acr TO XML HANDLER v_report_handler

   -- se obtienen los totales para DSE - ACR
   CALL fn_obt_agrup_modulo("acr", p_c_tpo_transf, p_d_folio)
   RETURNING v_r_agrup.tot_reg_dse, v_r_agrup.aivs97_dse, v_r_agrup.pesos97_dse, v_r_agrup.aivs92_dse, v_r_agrup.pesos92_dse

   -- se obtienen los totales para DSE - SEP
   CALL fn_obt_agrup_modulo("sep", p_c_tpo_transf, p_d_folio)
   RETURNING v_r_agrup.tot_reg_sep, v_r_agrup.aivs97_sep, v_r_agrup.pesos97_sep, v_r_agrup.aivs92_sep, v_r_agrup.pesos92_sep

   -- se obtienen los totales para DSE - DIS
   CALL fn_obt_agrup_modulo("dis", p_c_tpo_transf, p_d_folio)
   RETURNING v_r_agrup.tot_reg_dis, v_r_agrup.aivs97_dis, v_r_agrup.pesos97_dis, v_r_agrup.aivs92_dis, v_r_agrup.pesos92_dis

   -- se obtienen los totales para DSE - MJV
   CALL fn_obt_agrup_modulo("mjv", p_c_tpo_transf, p_d_folio)
   RETURNING v_r_agrup.tot_reg_mjv, v_r_agrup.aivs97_mjv, v_r_agrup.pesos97_mjv, v_r_agrup.aivs92_mjv, v_r_agrup.pesos92_mjv

   -- se obtienen los totales para DSE - MAO
   CALL fn_obt_agrup_modulo("mao", p_c_tpo_transf, p_d_folio)
   RETURNING v_r_agrup.tot_reg_mao, v_r_agrup.aivs97_mao, v_r_agrup.pesos97_mao, v_r_agrup.aivs92_mao, v_r_agrup.pesos92_mao

   -- se obtienen los totales para DSE - UCM
   CALL fn_obt_agrup_modulo("ucm", p_c_tpo_transf, p_d_folio)
   RETURNING v_r_agrup.tot_reg_ucm, v_r_agrup.aivs97_ucm, v_r_agrup.pesos97_ucm, v_r_agrup.aivs92_ucm, v_r_agrup.pesos92_ucm

   -- se obtienen los totales para DSE - RAL
   CALL fn_obt_agrup_modulo("ral", p_c_tpo_transf, p_d_folio)
   RETURNING v_r_agrup.tot_reg_ral, v_r_agrup.aivs97_ral, v_r_agrup.pesos97_ral, v_r_agrup.aivs92_ral, v_r_agrup.pesos92_ral

   -- se obtienen los folios de archivo agrupados
   LET v_s_qryTxt = " SELECT b.nom_archivo, b.folio, a.subcuenta,\n",
                    "        c.subcuenta_desc, COUNT(a.subcuenta),\n",
                    "        SUM(a.monto_aivs), SUM(a.monto_pesos)\n",
                    "   FROM dse_devolucion a, dse_ctr_archivo b,\n",
                    "        cat_subcuenta c\n",
                    "  WHERE a.tpo_transferencia = '",p_c_tpo_transf,"'\n",
                    "    AND a.folio = ",p_d_folio,"\n",
                    "    AND a.folio_referencia = b.folio\n",
                    "    AND a.tpo_transferencia = b.tpo_transferencia\n",
                    "    AND a.subcuenta = c.subcuenta\n",
                    "  GROUP BY 1,2,3,4\n",
                    "  ORDER BY 1,2,3"

   PREPARE prp_folio_archivo FROM v_s_qryTxt
   DECLARE cur_folio_archivo CURSOR FOR prp_folio_archivo

   FOREACH cur_folio_archivo INTO v_r_rpt_detalle.*
   -- salida del reporte
      OUTPUT TO REPORT rpt_agrupacion_acr(p_d_folio, v_r_rpt_detalle.*, v_r_agrup.*)
   END FOREACH

   IF v_r_rpt_detalle.folio_archivo IS NULL THEN
      LET v_r_rpt_detalle.nom_archivo    = "N/A"
      LET v_r_rpt_detalle.folio_archivo  = 0
      LET v_r_rpt_detalle.subcuenta      = 0
      LET v_r_rpt_detalle.subcuenta_desc = "N/A"
      LET v_r_rpt_detalle.num_registros  = 0
      LET v_r_rpt_detalle.monto_aivs     = 0
      LET v_r_rpt_detalle.monto_pesos    = 0

      OUTPUT TO REPORT rpt_agrupacion_acr(p_d_folio, v_r_rpt_detalle.*, v_r_agrup.*)
   END IF

   -- finaliza el reporte
   FINISH REPORT rpt_agrupacion_acr

END FUNCTION

-- OBJETIVO: Emitir el reporte de liquidacion/preliquidacion
REPORT rpt_agrupacion_acr(p_d_folio, p_r_rpt_detalle, p_r_agrup)

   DEFINE p_d_folio                 INTEGER

   DEFINE p_r_rpt_detalle RECORD
       nom_archivo                  LIKE dse_ctr_archivo.nom_archivo, -- nombre del archivo
       folio_archivo                INTEGER, --LIKE dse_ctr_archivo.folio, -- folio del archivo
       subcuenta                    LIKE cat_subcuenta.subcuenta, -- subcuenta
       subcuenta_desc               LIKE cat_subcuenta.subcuenta_desc, -- descripción de la subcuenta
       num_registros                INTEGER, -- número de registros a agrupar
       monto_aivs                   DECIMAL(18,6), -- suma de aivs
       monto_pesos                  DECIMAL(22,2) -- suma de pesos
   END RECORD

   DEFINE p_r_agrup RECORD
      tot_reg_dse                   INTEGER, -- Número de registros DSE
      aivs97_dse                    DECIMAL(22,6), -- AIVS 97 DSE
      pesos97_dse                   DECIMAL(22,2), -- Pesos 97 DSE
      aivs92_dse                    DECIMAL(22,6), -- AIVS 92 DSE
      pesos92_dse                   DECIMAL(22,2), -- Pesos 92 DSE
      tot_reg_sep                   INTEGER, -- Número de registros SEP
      aivs97_sep                    DECIMAL(22,6), -- AIVS 97 SEP
      pesos97_sep                   DECIMAL(22,2), -- Pesos 97 SEP
      aivs92_sep                    DECIMAL(22,6), -- AIVS 92 SEP
      pesos92_sep                   DECIMAL(22,2), -- Pesos 92 SEP
      tot_reg_dis                   INTEGER, -- Número de registros DIS
      aivs97_dis                    DECIMAL(22,6), -- AIVS 97 DIS
      pesos97_dis                   DECIMAL(22,2), -- Pesos 97 DIS
      aivs92_dis                    DECIMAL(22,6), -- AIVS 92 DIS
      pesos92_dis                   DECIMAL(22,2), -- Pesos 92 DIS
      tot_reg_mjv                   INTEGER, -- Número de registros MJV
      aivs97_mjv                    DECIMAL(22,6), -- AIVS 97 MJV
      pesos97_mjv                   DECIMAL(22,2), -- Pesos 97 MJV
      aivs92_mjv                    DECIMAL(22,6), -- AIVS 92 MJV
      pesos92_mjv                   DECIMAL(22,2), -- Pesos 92 MJV
      tot_reg_mao                   INTEGER, -- Número de registros MAO
      aivs97_mao                    DECIMAL(22,6), -- AIVS 97 MAO
      pesos97_mao                   DECIMAL(22,2), -- Pesos 97 MAO
      aivs92_mao                    DECIMAL(22,6), -- AIVS 92 MAO
      pesos92_mao                   DECIMAL(22,2), -- Pesos 92 MAO
      tot_reg_ucm                   INTEGER, -- Número de registros UCM
      aivs97_ucm                    DECIMAL(22,6), -- AIVS 97 UCM
      pesos97_ucm                   DECIMAL(22,2), -- Pesos 97 UCM
      aivs92_ucm                    DECIMAL(22,6), -- AIVS 92 UCM
      pesos92_ucm                   DECIMAL(22,2), -- Pesos 92 UCM
      tot_reg_ral                   INTEGER, -- Número de registros RAL
      aivs97_ral                    DECIMAL(22,6), -- AIVS 97 RAL
      pesos97_ral                   DECIMAL(22,2), -- Pesos 97 RAL
      aivs92_ral                    DECIMAL(22,6), -- AIVS 92 RAL
      pesos92_ral                   DECIMAL(22,2), -- Pesos 92 RAL
      tot_reg_tot                   INTEGER, -- Número de registros TOT
      aivs97_tot                    DECIMAL(22,6), -- Total AIVS 97
      pesos97_tot                   DECIMAL(22,2), -- Total Pesos 97
      aivs92_tot                    DECIMAL(22,6), -- Total AIVS 92
      pesos92_tot                   DECIMAL(22,2) -- Total Pesos 92
   END RECORD

   DEFINE v_f_fecha_rpt             DATE -- fecha de liquidacion/preliquidacion

   FORMAT

   FIRST PAGE HEADER

      -- se despliegan los datos del encabezado
      LET v_f_fecha_rpt = TODAY

      PRINTX p_usuario_cod
      PRINTX p_d_folio
      PRINTX v_f_fecha_rpt

   ON EVERY ROW

      PRINTX p_r_rpt_detalle.*

   ON LAST ROW
      -- se obtienen el sumario de los registros agrupados
      CALL f_obt_sumario_agrup(p_d_folio) RETURNING p_r_agrup.tot_reg_tot,
                                                    p_r_agrup.aivs97_tot,
                                                    p_r_agrup.pesos97_tot,
                                                    p_r_agrup.aivs92_tot,
                                                    p_r_agrup.pesos92_tot

      PRINTX p_r_agrup.*

END REPORT

#Objetivo: Obtiene el sumario de los registros agrupados en el proceso
FUNCTION f_obt_sumario_agrup(p_d_folio_agrup)

   DEFINE p_d_folio_agrup           LIKE glo_folio.folio
   DEFINE v_i_tot_registros         INTEGER -- total de registros agrupados
   DEFINE v_d_sum_aivs97            DECIMAL(18,6) -- suma de aivs 97 de los registros agrupados
   DEFINE v_d_sum_pesos97           DECIMAL(22,2) -- suma de pesos 97 de los registros agrupados
   DEFINE v_d_sum_aivs92            DECIMAL(18,6) -- suma de aivs 92 de los registros agrupados
   DEFINE v_d_sum_pesos92           DECIMAL(22,2) -- suma de pesos 92 de los registros agrupados
   DEFINE v_s_qryTxt                STRING

   -- se asigna la sentencia sql que obtiene el numero total de registros agrupados
   LET v_s_qryTxt = " SELECT SUM(aivs97), SUM(pesos97), SUM(aivs92), SUM(pesos92), COUNT(*)\n",
                    "   FROM dse_agrupa_devolucion\n",
                    "  WHERE id_dse_grp_devolucion IN (\n",
                    "        SELECT id_dse_grp_devolucion\n",
                    "          FROM dse_his_devolucion\n",
                    "         WHERE folio = ",p_d_folio_agrup,")"

   PREPARE prp_tot_registros FROM v_s_qryTxt
   EXECUTE prp_tot_registros INTO v_d_sum_aivs97, v_d_sum_pesos97, v_d_sum_aivs92, v_d_sum_pesos92, v_i_tot_registros

   -- se validan los totales
   IF v_d_sum_aivs97 IS NULL THEN
      LET v_d_sum_aivs97 = 0
   END IF

   IF v_d_sum_pesos97 IS NULL THEN
      LET v_d_sum_pesos97 = 0
   END IF

   IF v_d_sum_aivs92 IS NULL THEN
      LET v_d_sum_aivs92 = 0
   END IF

   IF v_d_sum_pesos92 IS NULL THEN
      LET v_d_sum_pesos92 = 0
   END IF

   RETURN v_i_tot_registros, v_d_sum_aivs97, v_d_sum_pesos97, v_d_sum_aivs92, v_d_sum_pesos92

END FUNCTION

#Objetivo: Se obtienen el total de AIVS97, Pesos97, AIVS92 y Pesos92 a agrupar
#          para el módulo que entra como parámetro
FUNCTION fn_obt_agrup_modulo(p_c_modulo_cod, p_c_tpo_transf, p_d_folio)

   DEFINE p_c_modulo_cod            LIKE seg_modulo.modulo_cod -- codigo del módulo
   DEFINE p_c_tpo_transf            LIKE dse_devolucion.tpo_transferencia -- tipo de transferencia
   DEFINE p_d_folio                 LIKE glo_folio.folio -- folio referencia
   DEFINE v_i_aux_num_regs          INTEGER -- número de registros (auxiliar)
   DEFINE v_i_num_regs              INTEGER -- número de registros
   DEFINE v_d_aivs97                DECIMAL(22,2) -- avis 97
   DEFINE v_d_pesos97               DECIMAL(22,2) -- pesos 97
   DEFINE v_d_aivs92                DECIMAL(22,2) -- avis 92
   DEFINE v_d_pesos92               DECIMAL(22,2) -- pesos 92
   DEFINE v_s_qryTxt                STRING

   -- se obtienen los totales a agrupar para el módulo en proceso
   LET v_s_qryTxt = " SELECT SUM(monto_aivs), SUM(monto_pesos), COUNT(*)\n",
                    "   FROM dse_devolucion\n",
                    "  WHERE tpo_transferencia = '",p_c_tpo_transf,"'",
                    "    AND folio = ",p_d_folio,"\n",
                    "    AND estado = 15\n",
                    "    AND modulo_cod = '",p_c_modulo_cod,"'\n",
                    "    AND subcuenta IN (4,44)"

   PREPARE prp_totales_97 FROM v_s_qryTxt
   EXECUTE prp_totales_97 INTO v_d_aivs97, v_d_pesos97, v_i_aux_num_regs

   -- se validan los aivs 97
   IF v_d_aivs97 IS NULL THEN
      LET v_d_aivs97 = 0
   END IF

   -- se validan los pesos 97
   IF v_d_pesos97 IS NULL THEN
      LET v_d_pesos97 = 0
   END IF

   -- se asigna el numero de registros en la variable de retorno
   LET v_i_num_regs = v_i_aux_num_regs

   -- se obtienen los totales a agrupar para el módulo en proceso
   LET v_s_qryTxt = " SELECT SUM(monto_aivs), SUM(monto_pesos), COUNT(*)\n",
                    "   FROM dse_devolucion\n",
                    "  WHERE tpo_transferencia = '",p_c_tpo_transf,"'",
                    "    AND folio = ",p_d_folio,"\n",
                    "    AND estado = 15\n",
                    "    AND modulo_cod = '",p_c_modulo_cod,"'\n",
                    "    AND subcuenta IN (8,42)"

   PREPARE prp_totales_92 FROM v_s_qryTxt
   EXECUTE prp_totales_92 INTO v_d_aivs92, v_d_pesos92, v_i_aux_num_regs

   -- se validan los aivs 92
   IF v_d_aivs92 IS NULL THEN
      LET v_d_aivs92 = 0
   END IF

   -- se validan los pesos 92
   IF v_d_pesos92 IS NULL THEN
      LET v_d_pesos92 = 0
   END IF

   -- se acumula el numero de registros en la variable de retorno
   LET v_i_num_regs = v_i_num_regs + v_i_aux_num_regs

   RETURN v_i_num_regs, v_d_aivs97, v_d_pesos97, v_d_aivs92, v_d_pesos92

END FUNCTION
