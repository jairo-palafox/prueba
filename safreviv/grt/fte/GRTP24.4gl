--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

###########################################################################
#Modulo       => GRT                                                      #
#Programa     => GRTP24                                                   #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la  #
#                agrupacion de DSE para el módulo de Créd. Garant. 43 bis #
#Autor        => Daniel Buendia, EFP                                      #
#Fecha inicio => 30 Mayo 2012                                             #
###########################################################################

DATABASE safre_viv

MAIN
   DEFINE p_usuario_cod      LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
          p_pid              LIKE bat_ctr_proceso.pid, --  ID del proceso
          p_proceso_cod      LIKE cat_proceso.proceso_cod, -- codigo del proceso
          p_opera_cod        LIKE cat_operacion.opera_cod, -- codigo de operacion
          p_folio            LIKE glo_folio.folio, -- folio referencia
          p_nombre_archivo   LIKE glo_ctr_archivo.nombre_archivo, -- nombre dle archivo
          v_c_tpo_transf     LIKE dse_agrupa_devolucion.tpo_transferencia, -- tipo de transferencia
          v_r_mto_agrup      RECORD
             num_registros   INTEGER, -- numero de registros
             sum_aivs97      DECIMAL(24,6), -- suma aivs 97
             sum_pesos97     DECIMAL(22,2), -- suma pesos 97
             sum_aivs92      DECIMAL(24,6), -- suma aivs 92
             sum_pesos92     DECIMAL(22,2) -- suma pesos 92
          END RECORD,
          v_s_sql            STRING, -- cadena con una instruccion SQL
          v_i_resultado      INTEGER, -- resultado del proceso
          v_bnd_fin_oper     SMALLINT,
          v_v_nom_reporte    VARCHAR(80), -- nombre del reporte
          v_s_mens_correo    STRING, -- contiene el cuerpo del correo
          v_s_titulo_correo  STRING, -- contiene el titulo del correo
          v_s_archivo_correo STRING, -- ruta y nombre del archivo adjunto en el correo
          v_s_qryTxt         STRING, -- guarda una sentencia SQL a ejecutar
          v_c_proceso_desc   LIKE cat_proceso.proceso_desc, -- descripción del proceso
          v_c_opera_desc     LIKE cat_operacion.opera_desc, -- descripción de la operacion
          v_c_programa_cod   LIKE cat_operacion.programa_cod, -- programa de la operación
          r_ruta_ejecutable  LIKE seg_modulo.ruta_bin, -- ruta del bin
          r_ruta_listados    LIKE seg_modulo.ruta_listados -- ruta de los listados

   -- se recuperan los parametros que envia el programa lanzador
   -- usuario, pid, proceso_cod, opera_cod, folio, nombre_archivo
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)   

   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".GRTP24.log")

   DISPLAY "=INICIA GRTP24="
   DISPLAY " USUARIO       : ",p_usuario_cod
   DISPLAY " PID           : ",p_pid
   DISPLAY " ARCHIVO       : ",p_nombre_archivo

   -- se inicializan variables
   LET v_i_resultado = 0 -- se asume que el proceso termina correctamente
   LET v_c_tpo_transf = "19" -- 19-DSE Créditos en Garantia 43 bis

   -- se genera el folio
   LET p_folio = fn_genera_folio(p_proceso_cod, p_opera_cod, p_usuario_cod)
   DISPLAY " FOLIO         : ",p_folio USING "#########&"

   -- se hace el despliegue de la línea que indica que el proceso ha comenzado
   CALL fn_display_proceso(0, "AGRUPACION DE DSE")
                             
   DISPLAY " EJECUTA PROCESO DE AGRUPACIÓN"
   -- se contruye el enuncionado SQL
   LET v_s_sql = "EXECUTE FUNCTION safre_viv:fn_dse_agrupa_devolucion(?,?,?)"

   -- se prepara la ejecucion del stored procedure para la preliquidacion
   PREPARE sid_preliquidadse FROM v_s_sql
   EXECUTE sid_preliquidadse USING p_folio,
                                   p_usuario_cod,
                                   v_c_tpo_transf
                              INTO v_i_resultado 
            
   IF ( v_i_resultado <> 0 ) THEN
      -- verificando si el error fue por carencia de valor del fondo
      IF v_i_resultado = -1 THEN
         DISPLAY " NO EXISTE VALOR DEL FONDO PARA LA FECHA ACTUAL"
      ELSE
         DISPLAY " OCURRIO UN ERROR DURANTE LA EJECUCION DE AGRUPACIÓN DSE: ", v_i_resultado
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

{
   -- se consulta los montos agrupados
   LET v_s_sql = " SELECT COUNT(*), SUM(aivs97), SUM(pesos97), SUM(aivs92), SUM(pesos92)\n",
                 "   FROM dse_agrupa_devolucion\n",
                 "  WHERE tpo_transferencia = ",v_c_tpo_transf
}
   -- se consulta los montos agrupados
   LET v_s_sql = " SELECT COUNT(*), SUM(aivs97), SUM(pesos97), SUM(aivs92), SUM(pesos92)\n",
                 "   FROM dse_agrupa_devolucion\n",
                 "  WHERE id_dse_grp_devolucion IN (\n",
                 "        SELECT id_dse_grp_devolucion\n",
                 "          FROM dse_his_devolucion\n",
                 "         WHERE tpo_transferencia = ",v_c_tpo_transf,"\n",
                 "           AND folio = ",p_folio,")\n",
                 "    AND tpo_transferencia = '",v_c_tpo_transf CLIPPED,"'"

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
   CALL fn_rutas("grt") RETURNING r_ruta_ejecutable, r_ruta_listados

   -- se obtiene el nombrel del programa correspondiente
   LET v_c_programa_cod = fn_obten_nom_programa(p_proceso_cod , p_opera_cod)

   -- se asigna el nombre del reporte
   LET v_v_nom_reporte = p_usuario_cod CLIPPED || "-", v_c_programa_cod CLIPPED,"-",p_pid USING "&&&&&","-",p_proceso_cod USING "&&&&&", "-", p_opera_cod USING "&&&&&"

   -- genera el reporte de la agrupación
   CALL fn_gen_rpt_agrupacion(v_v_nom_reporte, r_ruta_listados, v_c_tpo_transf, p_folio, p_usuario_cod)

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
FUNCTION fn_gen_rpt_agrupacion(p_v_nom_reporte, p_ruta_listados, p_c_tpo_transf, p_d_folio, p_usuario_cod)
   DEFINE p_v_nom_reporte    VARCHAR(80), -- nombre del reporte
          p_ruta_listados    LIKE seg_modulo.ruta_listados, -- ruta de los listados
          p_c_tpo_transf     LIKE dse_ctr_archivo.tpo_transferencia, -- tipo de transferencia
          p_d_folio          LIKE glo_folio.folio, -- folio referencia
          p_usuario_cod      LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
          v_r_rpt_detalle    RECORD
             nom_archivo     LIKE dse_ctr_archivo.nom_archivo, -- nombre del archivo
             folio_archivo   LIKE dse_ctr_archivo.folio, -- folio del archivo
             subcuenta       LIKE cat_subcuenta.subcuenta, -- subcuenta
             subcuenta_desc  LIKE cat_subcuenta.subcuenta_desc, -- descripción de la subcuenta
             num_registros   INTEGER, -- número de registros a agrupar
             monto_aivs      DECIMAL(18,6), -- suma de aivs
             monto_pesos     DECIMAL(22,2) -- suma de pesos
          END RECORD
   DEFINE v_r_agrup         RECORD 
             tot_reg_dse    INTEGER, -- Número de registros DSE
             aivs97_dse     DECIMAL(22,6), -- AIVS 97 DSE
             pesos97_dse    DECIMAL(22,2), -- Pesos 97 DSE
             aivs92_dse     DECIMAL(22,6), -- AIVS 92 DSE
             pesos92_dse    DECIMAL(22,2), -- Pesos 92 DSE
             tot_reg_sep    INTEGER, -- Número de registros SEP
             aivs97_sep     DECIMAL(22,6), -- AIVS 97 SEP
             pesos97_sep    DECIMAL(22,2), -- Pesos 97 SEP
             aivs92_sep     DECIMAL(22,6), -- AIVS 92 SEP
             pesos92_sep    DECIMAL(22,2), -- Pesos 92 SEP
             tot_reg_dis    INTEGER, -- Número de registros DIS
             aivs97_dis     DECIMAL(22,6), -- AIVS 97 DIS
             pesos97_dis    DECIMAL(22,2), -- Pesos 97 DIS
             aivs92_dis     DECIMAL(22,6), -- AIVS 92 DIS
             pesos92_dis    DECIMAL(22,2), -- Pesos 92 DIS
             tot_reg_tot    INTEGER, -- Número de registros TOT
             aivs97_tot     DECIMAL(22,6), -- Total AIVS 97
             pesos97_tot    DECIMAL(22,2), -- Total Pesos 97
             aivs92_tot     DECIMAL(22,6), -- Total AIVS 92
             pesos92_tot    DECIMAL(22,2) -- Total Pesos 92
          END RECORD,
          v_report_handler   om.SaxDocumentHandler, -- handler para el reporte en PDF
          v_s_qryTxt         STRING -- se asigna sentencia sql a ejecutar

   -- se indica que el reporte usara la plantilla creada
   IF ( fgl_report_loadCurrentSettings("GRTP241.4rp") ) THEN
      -- sin preview
      CALL fgl_report_selectPreview(0)

      -- se indica que se escriba en archivo
      CALL fgl_report_setOutputFileName(p_ruta_listados CLIPPED||"/"||p_v_nom_reporte CLIPPED)

      LET v_report_handler = fgl_report_commitCurrentSettings()
   ELSE
      DISPLAY "NO SE PUDO LEER LA PLANTILLA DEL REPORTE"

      EXIT PROGRAM
   END IF

   -- se inicia el reporte
   START REPORT rpt_agrupacion_grt TO XML HANDLER v_report_handler

   -- se obtienen los totales para DSE
   CALL fn_obt_agrup_modulo("grt", p_c_tpo_transf, p_d_folio) RETURNING v_r_agrup.tot_reg_dse, v_r_agrup.aivs97_dse, v_r_agrup.pesos97_dse, v_r_agrup.aivs92_dse, v_r_agrup.pesos92_dse

   -- se obtienen los totales para DSE
   CALL fn_obt_agrup_modulo("sep", p_c_tpo_transf, p_d_folio) RETURNING v_r_agrup.tot_reg_sep, v_r_agrup.aivs97_sep, v_r_agrup.pesos97_sep, v_r_agrup.aivs92_sep, v_r_agrup.pesos92_sep

   -- se obtienen los totales para DSE
   CALL fn_obt_agrup_modulo("dis", p_c_tpo_transf, p_d_folio) RETURNING v_r_agrup.tot_reg_dis, v_r_agrup.aivs97_dis, v_r_agrup.pesos97_dis, v_r_agrup.aivs92_dis, v_r_agrup.pesos92_dis

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
      OUTPUT TO REPORT rpt_agrupacion_grt(p_usuario_cod, p_d_folio, v_r_rpt_detalle.*, v_r_agrup.*)
   END FOREACH

   -- finaliza el reporte
   FINISH REPORT rpt_agrupacion_grt
END FUNCTION

-- OBJETIVO: Emitir el reporte de liquidacion/preliquidacion
REPORT rpt_agrupacion_grt(p_usuario_cod, p_d_folio, p_r_rpt_detalle, p_r_agrup)
   DEFINE p_usuario_cod     STRING,
          p_d_folio         INTEGER,
          p_r_rpt_detalle   RECORD
             nom_archivo    LIKE dse_ctr_archivo.nom_archivo, -- nombre del archivo
             folio_archivo  INTEGER, --LIKE dse_ctr_archivo.folio, -- folio del archivo
             subcuenta      LIKE cat_subcuenta.subcuenta, -- subcuenta
             subcuenta_desc LIKE cat_subcuenta.subcuenta_desc, -- descripción de la subcuenta
             num_registros  INTEGER, -- número de registros a agrupar
             monto_aivs     DECIMAL(18,6), -- suma de aivs
             monto_pesos    DECIMAL(22,2) -- suma de pesos
          END RECORD
   DEFINE p_r_agrup         RECORD 
             tot_reg_dse    INTEGER, -- Número de registros DSE
             aivs97_dse     DECIMAL(22,6), -- AIVS 97 DSE
             pesos97_dse    DECIMAL(22,2), -- Pesos 97 DSE
             aivs92_dse     DECIMAL(22,6), -- AIVS 92 DSE
             pesos92_dse    DECIMAL(22,2), -- Pesos 92 DSE
             tot_reg_sep    INTEGER, -- Número de registros SEP
             aivs97_sep     DECIMAL(22,6), -- AIVS 97 SEP
             pesos97_sep    DECIMAL(22,2), -- Pesos 97 SEP
             aivs92_sep     DECIMAL(22,6), -- AIVS 92 SEP
             pesos92_sep    DECIMAL(22,2), -- Pesos 92 SEP
             tot_reg_dis    INTEGER, -- Número de registros DIS
             aivs97_dis     DECIMAL(22,6), -- AIVS 97 DIS
             pesos97_dis    DECIMAL(22,2), -- Pesos 97 DIS
             aivs92_dis     DECIMAL(22,6), -- AIVS 92 DIS
             pesos92_dis    DECIMAL(22,2), -- Pesos 92 DIS
             tot_reg_tot    INTEGER, -- Número de registros TOT
             aivs97_tot     DECIMAL(22,6), -- Total AIVS 97
             pesos97_tot    DECIMAL(22,2), -- Total Pesos 97
             aivs92_tot     DECIMAL(22,6), -- Total AIVS 92
             pesos92_tot    DECIMAL(22,2) -- Total Pesos 92
          END RECORD,
          v_f_fecha_rpt     DATE -- fecha de liquidacion/preliquidacion
          --v_tot_registros   INTEGER, -- numero total de registros agrupados
          --v_monto_aivs97      DECIMAL(18,6),
          --v_monto_pesos97     DECIMAL(22,2),
          --v_monto_aivs92      DECIMAL(18,6),
          --v_monto_pesos92     DECIMAL(22,2)

   FORMAT   

   FIRST PAGE HEADER
      -- se despliegan los datos del encabezado
      --LET v_monto_aivs97 = 0
      --LET v_monto_pesos97 = 0
      --LET v_monto_aivs92 = 0
      --LET v_monto_pesos92 = 0
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
      --PRINTX v_tot_registros
      --PRINTX v_monto_aivs97
      --PRINTX v_monto_pesos97
      --PRINTX v_monto_aivs92
      --PRINTX v_monto_pesos92
END REPORT

#Objetivo: Obtiene el sumario de los registros agrupados en el proceso
FUNCTION f_obt_sumario_agrup(p_d_folio_agrup)
   DEFINE p_d_folio_agrup    LIKE glo_folio.folio,
          v_i_tot_registros  INTEGER, -- total de registros agrupados
          v_d_sum_aivs97     DECIMAL(18,6), -- suma de aivs 97 de los registros agrupados
          v_d_sum_pesos97    DECIMAL(22,2), -- suma de pesos 97 de los registros agrupados
          v_d_sum_aivs92     DECIMAL(18,6), -- suma de aivs 92 de los registros agrupados
          v_d_sum_pesos92    DECIMAL(22,2), -- suma de pesos 92 de los registros agrupados
          v_s_qryTxt         STRING

   -- se asigna la sentencia sql que obtiene el numero total de registros agrupados
   LET v_s_qryTxt = " SELECT COUNT(*), SUM(aivs97), SUM(pesos97), SUM(aivs92), SUM(pesos92)\n",
                    "   FROM dse_agrupa_devolucion\n",
                    "  WHERE id_dse_grp_devolucion IN (\n",
                    "        SELECT id_dse_grp_devolucion\n",
                    "          FROM dse_his_devolucion\n",
                    "         WHERE folio = ",p_d_folio_agrup,")"

   PREPARE prp_tot_registros FROM v_s_qryTxt
   EXECUTE prp_tot_registros INTO v_i_tot_registros, v_d_sum_aivs97, v_d_sum_pesos97, v_d_sum_aivs92, v_d_sum_pesos92

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
   DEFINE p_c_modulo_cod   LIKE seg_modulo.modulo_cod, -- codigo del módulo
          p_c_tpo_transf   LIKE dse_devolucion.tpo_transferencia, -- tipo de transferencia
          p_d_folio        LIKE glo_folio.folio, -- folio referencia
          v_i_aux_num_regs INTEGER, -- número de registros (auxiliar)
          v_i_num_regs     INTEGER, -- número de registros
          v_d_aivs97       DECIMAL(22,2), -- avis 97
          v_d_pesos97      DECIMAL(22,2), -- pesos 97
          v_d_aivs92       DECIMAL(22,2), -- avis 92
          v_d_pesos92      DECIMAL(22,2), -- pesos 92
          v_s_qryTxt       STRING

   -- se obtienen los totales a agrupar para el módulo en proceso
   LET v_s_qryTxt = " SELECT COUNT(*), SUM(monto_aivs), SUM(monto_pesos)\n",
                    "   FROM dse_devolucion\n",
                    "  WHERE tpo_transferencia = '",p_c_tpo_transf,"'",
                    "    AND folio = ",p_d_folio,"\n",
                    "    AND estado = 15\n",
                    "    AND modulo_cod = '",p_c_modulo_cod,"'\n",
                    "    AND subcuenta IN (4,44)"

   PREPARE prp_totales_97 FROM v_s_qryTxt
   EXECUTE prp_totales_97 INTO v_i_aux_num_regs, v_d_aivs97, v_d_pesos97

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
   LET v_s_qryTxt = " SELECT COUNT(*), SUM(monto_aivs), SUM(monto_pesos)\n",
                    "   FROM dse_devolucion\n",
                    "  WHERE tpo_transferencia = '",p_c_tpo_transf,"'",
                    "    AND folio = ",p_d_folio,"\n",
                    "    AND estado = 15\n",
                    "    AND modulo_cod = '",p_c_modulo_cod,"'\n",
                    "    AND subcuenta IN (8,42)"

   PREPARE prp_totales_92 FROM v_s_qryTxt
   EXECUTE prp_totales_92 INTO v_i_aux_num_regs, v_d_aivs92, v_d_pesos92

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
