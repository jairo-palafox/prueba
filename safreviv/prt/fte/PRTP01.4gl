--===============================================================
-- Versión: 1.0.0
-- Fecha última modificación:
--===============================================================

##########################################################################
#Modulo            => PRT                                                #
#Programa          => PRTP01                                             #
#Objetivo          => Programa que realiza la preliquidación para el     #
#                     Portabilidad                                       #
#Autor             => Héctor F. Jiménez Lara                             #
#Fecha inicio      => 05 Agosto 2015                                     #
##########################################################################

DATABASE safre_viv

   DEFINE v_s_qryTxt            STRING -- guarda una sentencia SQL a ejecutar

MAIN

   DEFINE p_v_usuario_cod       LIKE seg_usuario.usuario -- nombre del usuario
   DEFINE p_d_pid               LIKE bat_ctr_proceso.pid -- pid de la operación
   DEFINE p_i_proceso_cod       LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE p_i_opera_cod         LIKE cat_operacion.opera_cod -- codigo de la operacion de la etapa
   DEFINE p_d_folio             LIKE glo_folio.folio -- numero de folio
   DEFINE v_si_tpo_originacion  LIKE cre_acreditado.tpo_originacion -- tipo de originación
   DEFINE v_si_tpo_transferenc  LIKE cre_uso_garantia.tpo_transferencia -- tipo de transferencia
   DEFINE v_ind_tipo_ejecucion  LIKE bat_ctr_operacion.ind_tipo_ejecucion -- tipo de ejecucion (0-manual, 1-batch)
   DEFINE v_d_monto_aivs        DECIMAL(25,2) -- suma del monto en aivs
   DEFINE v_d_monto_pesos       DECIMAL(25,2) -- suma del monto en pesos
   DEFINE r_c_ruta_bin          LIKE seg_modulo.ruta_bin -- ruta bin del módulo
   DEFINE r_c_ruta_listados     LIKE seg_modulo.ruta_listados -- ruta listados del módulo
   DEFINE r_b_valida            SMALLINT -- booleana que indica si el proceso se puede ejecutar o no
   DEFINE v_id_derechohabiente  DECIMAL(9,0)
   DEFINE v_f_liquida           DATE
   DEFINE v_fondo_inversion     SMALLINT
   DEFINE v_id_referencia       DECIMAL(9,0)
   DEFINE v_subcuenta           SMALLINT
   DEFINE v_f_valor             DATE
   DEFINE v_f_registro          DATE
   DEFINE v_h_registro          DATE
   DEFINE v_origen              CHAR(20)
   DEFINE v_mov_liq             SMALLINT
   -- Variables para el reporte
   DEFINE v_nom_reporte         STRING
   DEFINE report_handler        om.SaxDocumentHandler -- handler para el reporte en PDF
   DEFINE p_r_encabezado    RECORD
      p_folio                   INTEGER,
      p_usuario_cod             STRING,
      p_fecha                   DATE -- fecha de liquidacion/preliduidacion
    END RECORD
    DEFINE v_tot_pesos          DECIMAL(25,2)
    DEFINE v_tot_aivs           DECIMAL(25,2)

   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario_cod  = ARG_VAL(1)
   LET p_d_pid          = ARG_VAL(2)
   LET p_i_proceso_cod  = ARG_VAL(3)
   LET p_i_opera_cod    = ARG_VAL(4)
   LET p_d_folio        = ARG_VAL(5)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario_cod CLIPPED|| ".PRTP01.log")
   
   DISPLAY "=INICIA PRTP01="
   DISPLAY "USUARIO    : ",p_v_usuario_cod
   DISPLAY "PID        : ",p_d_pid CLIPPED

   -- se inicializan variables
   LET v_si_tpo_originacion = 4 -- Anualidades Garantizadas
   LET v_si_tpo_transferenc = "43" -- Anualidades Garantizadas

   -- Se inicializan variables para la preliquidación
   LET v_mov_liq    = 1672
   LET v_f_liquida  = TODAY
   LET v_subcuenta  = 60
   LET v_f_registro = TODAY
   LET v_h_registro = CURRENT
   LET v_origen     = "CARGO AMORT SDO PORT"
   LET v_tot_aivs   = 0
   LET v_tot_pesos  = 0

   -- se obtiene el tipo de proceso de la tabla de control de procesos
   LET v_s_qryTxt = " SELECT ind_tipo_ejecucion\n",
                    "   FROM bat_ctr_operacion\n",
                    "  WHERE pid = ",p_d_pid,"\n",
                    "    AND proceso_cod = ",p_i_proceso_cod,"\n",
                    "    AND opera_cod = ",p_i_opera_cod

   PREPARE prp_tipo_ejecucion FROM v_s_qryTxt
   EXECUTE prp_tipo_ejecucion INTO v_ind_tipo_ejecucion

   -- se valida si la operacion se ejecutó manualmente o a través del batch
   IF v_ind_tipo_ejecucion = 1 THEN
      -- se genera el folio
      LET p_d_folio = fn_genera_folio(p_i_proceso_cod, p_i_opera_cod, p_v_usuario_cod)
   END IF
   DISPLAY "FOLIO      : ",p_d_folio USING "#########&"

   -- Se invoca la función que genera la tabla de preliquidación 
   CALL fn_crea_tbl_preliquidacion()

   LET v_s_qryTxt = " SELECT id_derechohabiente,
                             fondo_inversion,
                             id_referencia,
                             SUM(monto_pesos),
                             SUM(monto_acciones)
                        FROM cta_movimiento
                       WHERE subcuenta = " || v_subcuenta ||
                       " AND movimiento IN ( 1601,1672 )
                       GROUP BY 1,2,3"
   
   PREPARE prp_preliquida FROM v_s_qryTxt

   DECLARE cur_preliquida CURSOR FOR prp_preliquida

   FOREACH cur_preliquida INTO v_id_derechohabiente,v_fondo_inversion,v_id_referencia, v_d_monto_pesos, v_d_monto_aivs
      IF v_d_monto_pesos > 0 THEN
         -- Se invierte el monto en pesos
         LET v_d_monto_pesos = v_d_monto_pesos * -1

         -- Se invierte el monto en aivs
         LET v_d_monto_aivs = v_d_monto_aivs * -1 

         -- Se arma el total para las cifras control del reporte
         LET v_tot_pesos = v_tot_pesos + v_d_monto_pesos
         LET v_tot_aivs  = v_tot_aivs + v_d_monto_aivs

         -- Se obtiene la fecha en que se esta preliquidando
         LET v_f_valor = TODAY

         -- Se obtiene el primer dia natural de mes
         LET v_f_valor = v_f_valor - DAY( v_f_valor ) + 1

         LET v_s_qryTxt = "INSERT INTO prt_preliq_portabilidad 
                                VALUES( ?,?,?,?,?,?,?,?,?,?,?,?,? ) "

         PREPARE prp_ins_preliq FROM v_s_qryTxt
         EXECUTE prp_ins_preliq USING v_f_liquida,
                                      v_id_derechohabiente,
                                      v_subcuenta,
                                      v_fondo_inversion,
                                      v_mov_liq,
                                      p_d_folio,
                                      v_id_referencia,
                                      v_d_monto_aivs,
                                      v_d_monto_pesos,
                                      v_f_valor,
                                      v_f_registro,
                                      v_h_registro,
                                      v_origen


      END IF
   END FOREACH

   -- se actualiza el status de glo folio, para que el folio lo pueda tomar la función
   -- general de liquidación
   UPDATE glo_folio
      SET status = 1
    WHERE folio = p_d_folio

   -- se invoca la función que deja la operación en estado Finalizado
   LET r_b_valida = fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se informa de la finalización de la preliquidación
   DISPLAY "SE REALIZÓ LA PRELIQUIDACIÓN SATISFACTORIAMENTE CON PID: ", p_d_pid

   -- se obtiene la ruta bin y de listados del módulo
   CALL fn_rutas("prt") RETURNING r_c_ruta_bin, r_c_ruta_listados

   DISPLAY "SE GENERA REPORTE DE PRELIQUIDACIÓN"

   -- se asigna el nombre del reporte
   LET v_nom_reporte = p_v_usuario_cod CLIPPED || "-", "PRTP01" CLIPPED,"-",p_d_pid USING "&&&&&","-",p_i_proceso_cod USING "&&&&&", "-", p_i_opera_cod USING "&&&&&"

   IF fgl_report_loadCurrentSettings("PRTP011.4rp") THEN  -- if  the file loaded OK
      -- se crea el nombre del reporte
      LET v_nom_reporte = p_v_usuario_cod CLIPPED || "-","PRTP01" CLIPPED,"-", p_d_pid USING "&&&&&", "-", p_i_proceso_cod USING "&&&&&", "-", p_i_opera_cod USING "&&&&&"
      CALL fgl_report_setOutputFileName(r_c_ruta_listados CLIPPED||"/"||v_nom_reporte)

      -- sin indica que no es necesario el preview
      CALL fgl_report_selectPreview(0)

      -- se asigna la configuración en el menejador del reporte
      LET report_handler = fgl_report_commitCurrentSettings()      -- commit the file settings
   ELSE
      DISPLAY "NO FUE POSIBLE GENERAR EL REPORTE"
      CALL fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)
                         RETURNING r_b_valida

      IF(r_b_valida <> 0)THEN
         -- En caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF
      EXIT PROGRAM
   END IF

    -- se inicia el reporte
   START REPORT rpt_liquidacion_portabilidad TO XML HANDLER report_handler


   LET p_r_encabezado.p_folio       = p_d_folio
   LET p_r_encabezado.p_usuario_cod = p_v_usuario_cod

   -- se obtiene la fecha de liquidacion/preliquidacion
   LET v_s_qryTxt = " SELECT DISTINCT f_liquida\n",
                      " FROM prt_preliq_portabilidad \n",
                     " WHERE folio_liquida = ? \n"

   PREPARE sid_fliquida FROM v_s_qryTxt
   EXECUTE sid_fliquida USING p_d_folio INTO p_r_encabezado.p_fecha

   OUTPUT TO REPORT rpt_liquidacion_portabilidad(p_i_opera_cod, p_r_encabezado.*,v_tot_pesos, v_tot_aivs)

   FINISH REPORT rpt_liquidacion_portabilidad

END MAIN

REPORT rpt_liquidacion_portabilidad(p_opera_cod,p_encabezado,p_tot_pesos, p_tot_aivs)
   DEFINE p_opera_cod           LIKE cat_operacion.opera_cod
   DEFINE p_tot_pesos       DECIMAL(25,2)
   DEFINE p_tot_aivs        DECIMAL(25,2)
   DEFINE p_encabezado      RECORD
      p_folio                   INTEGER,
      p_usuario_cod             STRING,
      p_fecha                   DATE
   END RECORD
   DEFINE v_desc_mov            CHAR(35)
   DEFINE v_subcta_desc         CHAR(30)

   FORMAT
      FIRST PAGE HEADER
         PRINTX p_encabezado.p_usuario_cod
         PRINTX p_encabezado.p_folio
         PRINTX p_encabezado.p_fecha

      --ON EVERY ROW
         -- se obtiene la descripcion de la subcuenta
         LET v_s_qryTxt = " SELECT subcuenta || ' ' ||  subcuenta_desc
                              FROM cat_subcuenta
                             WHERE subcuenta = 60 "

         PREPARE prp_scta FROM v_s_qryTxt
         EXECUTE prp_scta INTO v_subcta_desc

         PRINTX v_subcta_desc
         PRINTX p_tot_pesos
         PRINTX p_tot_aivs

         LET v_desc_mov= "1672 CARGO AMORT SDO PORT"

         PRINTX v_desc_mov

         {-- se obtienen los movimientos de la subcuenta
         LET v_s_qryTxt = "SELECT p.movimiento || ' '  || c.movimiento_desc
                             FROM prt_preliq_portabilidad p,
                                  cat_movimiento c
                            WHERE p.movimiento = c.movimiento " ||
                           " AND folio_liquida = " || p_encabezado.p_folio

         PREPARE prp_rpt_preliq FROM v_s_qryTxt

         DECLARE cur_rpt_preliq CURSOR FOR prp_rpt_preliq

         FOREACH cur_rpt_preliq INTO v_desc_mov
            PRINTX v_desc_mov
            EXIT FOREACH 
         END FOREACH}

END REPORT


FUNCTION fn_crea_tbl_preliquidacion()

   DEFINE v_s_qryTxt STRING -- guarda una sentencia SQL a ejecutar

   -- se crea la sentencia sql que elimina la tabla de preliquidación
   LET v_s_qryTxt = " DROP TABLE IF EXISTS prt_preliq_portabilidad;"

   PREPARE prp_drop_preliquida FROM v_s_qryTxt
   EXECUTE prp_drop_preliquida

   -- se crea la sentencia sql que crea la tabla de preliquidación
   LET v_s_qryTxt = " CREATE TABLE prt_preliq_portabilidad\n",
                    " (f_liquida          DATE NOT NULL,\n",
                    "  id_derechohabiente DECIMAL(9,0) NOT NULL,\n",
                    "  subcuenta          SMALLINT NOT NULL,\n",
                    "  fondo_inversion    SMALLINT NOT NULL,\n",
                    "  movimiento         SMALLINT NOT NULL,\n",
                    "  folio_liquida      DECIMAL(9,0) NOT NULL,\n",
                    "  id_referencia      DECIMAL(9,0) NOT NULL,\n",
                    "  monto_acciones     DECIMAL(22,2),\n",
                    "  monto_pesos        DECIMAL(22,2),\n",
                    "  f_valor            DATE,\n",
                    "  f_registro         DATE,\n",
                    "  h_registro         DATETIME HOUR TO SECOND,\n",
                    "  origen             CHAR(20)\n",
                    " ) IN cre_dbs;"

   PREPARE prp_create_preliquida FROM v_s_qryTxt
   EXECUTE prp_create_preliquida 

END FUNCTION