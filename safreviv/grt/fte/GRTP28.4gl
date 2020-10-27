--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

#########################################################################
#Modulo            =>GRT                                                #
#Programa          =>GRTP28                                             #
#Objetivo          =>Programa que ejecuta los procesos que realiza la   #
#                    actualizacion de estados despues de la liquidacion #
#                    y genera el reporte de liquidacion DSE de GRT      #
#Autor             =>Daniel Buendia, EFP                                #
#Fecha inicio      =>31 Mayo 2012                                       #
#########################################################################
DATABASE safre_viv

GLOBALS
DEFINE g_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       g_pid            LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod    LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod      LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_folio_liquida  LIKE cta_movimiento.folio_liquida -- folio de liquidación
END GLOBALS

MAIN
   DEFINE p_v_arch_proceso       LIKE bat_ctr_operacion.nom_archivo, -- archivo del proceso
          v_s_qryTxt             STRING, -- guarda una sentencia SQL a ejecutar
          v_c_tpo_transferencia  LIKE dse_agrupa_devolucion.tpo_transferencia, -- tipo de trasnferencia
          v_r_cifras_liq         RECORD
             subcuenta           LIKE cat_subcuenta.subcuenta, -- subcuenta
             subcuenta_desc      LIKE cat_subcuenta.subcuenta_desc, -- descripción de subcuenta
             movimiento          LIKE cat_movimiento.movimiento, -- movimiento
             movto_desc          LIKE cat_movimiento.movimiento_desc, -- descripción de movimiento
             monto_aivs          LIKE cta_movimiento.monto_acciones, -- monto de aivs
             monto_pesos         DECIMAL(22,2) --LIKE cta_movimiento.monto_pesos -- monto en pesos
          END RECORD,
          v_s_titulo_correo      STRING, -- contiene el titulo del correo
          v_s_archivo_correo     STRING, -- ruta y nombre del archivo adjunto en el correo
          v_s_mens_correo        STRING, -- contiene el cuerpo del correo
          v_v_nom_reporte        VARCHAR(80), -- nombre del reporte
          v_c_ruta_bin           LIKE seg_modulo.ruta_bin, -- ruta bin
          v_c_ruta_listados      LIKE seg_modulo.ruta_listados -- ruta listados

   -- se recuperan los parametros que envia el programa lanzador
   LET g_usuario_cod       = ARG_VAL(1)
   LET g_pid               = ARG_VAL(2)
   LET g_proceso_cod       = ARG_VAL(3)
   LET g_opera_cod         = ARG_VAL(4)
   LET g_folio_liquida     = ARG_VAL(5)
   LET p_v_arch_proceso    = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(g_usuario_cod CLIPPED|| ".GRTP28.log")

   DISPLAY "=INICIA GRTP28="
   DISPLAY " USUARIO       : ",g_usuario_cod
   DISPLAY " PID           : ",g_pid
   DISPLAY " FOLIO         : ",g_folio_liquida USING "#########&"

   # Recupera la ruta de listados en el que se enviara el archivo
   CALL fn_rutas("grt") RETURNING v_c_ruta_bin, v_c_ruta_listados

   LET v_v_nom_reporte = v_c_ruta_listados CLIPPED,"/",g_usuario_cod CLIPPED,"-GRTL40-",g_pid USING "&&&&&","-",g_proceso_cod USING "&&&&&","-",g_opera_cod USING "&&&&&"
   
   -- se inicializan variables
   LET v_c_tpo_transferencia = "19" -- 19-DSE Créditos en Garantía 43 bis

   DISPLAY " ACTUALIZA ESTATUS A ESTADO 'LIQUIDADADO'"
   -- se actualiza el estado del registro en dse_devolucion a 130
   LET v_s_qryTxt = " UPDATE dse_devolucion\n",
                    "    SET estado = 140\n",
                    "  WHERE id_derechohabiente IN (\n",
                    "        SELECT id_derechohabiente\n",
                    "          FROM dse_agrupa_devolucion\n",
                    "         WHERE tpo_transferencia = '",v_c_tpo_transferencia,"'\n",
                    "           AND estado = 130\n",
                    "           AND edo_procesar IN (5, 10, 20))\n",
                    "    AND tpo_transferencia = '",v_c_tpo_transferencia,"'\n",
                    "    AND estado = 130"

   PREPARE prp_act_edo_dse_dev FROM v_s_qryTxt
   EXECUTE prp_act_edo_dse_dev

   -- se crea la sentencia sql actualiza los estatus a 140-Liquidados
   LET v_s_qryTxt = " UPDATE dse_agrupa_devolucion\n",
                    "    SET estado = 140\n",
                    "  WHERE tpo_transferencia = '",v_c_tpo_transferencia,"'\n",
                    "    AND estado = 130\n",
                    "    AND edo_procesar IN (5, 10, 20)"

   PREPARE prp_act_estado FROM v_s_qryTxt
   EXECUTE prp_act_estado

   -- se crea la sentencia sql actualiza los estatus a 140-Liquidados
   LET v_s_qryTxt = " UPDATE dse_his_devolucion\n",
                    "    SET estado = 140\n",
                    "  WHERE tpo_transferencia = '",v_c_tpo_transferencia,"'\n",
                    "    AND estado = 130\n",
                    "    AND edo_procesar IN (5, 10, 20)"

   PREPARE prp_act_estado_his FROM v_s_qryTxt
   EXECUTE prp_act_estado_his

   DISPLAY " = CIFRAS LIQUIDADAS = "
   -- se realiza la consulta de las cifras liquidadas
   LET v_s_qryTxt = " SELECT cta.subcuenta, cats.subcuenta_desc,\n",
                    "        cta.movimiento, catm.movimiento_desc,\n",
                    "        SUM(cta.monto_acciones), SUM(cta.monto_pesos)\n",
                    "   FROM cta_movimiento cta, cat_movimiento catm, cat_subcuenta cats\n",
                    "  WHERE cta.folio_liquida = ",g_folio_liquida,"\n",
                    "    AND cta.subcuenta = cats.subcuenta\n",
                    "    AND cta.movimiento = catm.movimiento\n",
                    "  GROUP BY 1,2,3,4\n",
                    "  ORDER BY 1,3"

   PREPARE prp_cifras_liq FROM v_s_qryTxt
   DECLARE cur_cifras_liq CURSOR FOR prp_cifras_liq

   FOREACH cur_cifras_liq INTO v_r_cifras_liq.*
      DISPLAY "SUBCUENTA       ", v_r_cifras_liq.subcuenta, "-", v_r_cifras_liq.subcuenta_desc
      DISPLAY "MOVIMIENTO      ", v_r_cifras_liq.movimiento, "-", v_r_cifras_liq.movto_desc
      DISPLAY "MONTO ACCIONES  ", v_r_cifras_liq.monto_aivs
      DISPLAY "MONTO PESOS     ", v_r_cifras_liq.monto_pesos
   END FOREACH

   DISPLAY " GENERA REPORTE DE LIQUIDACIÓN"
   -- se informa a usuario el termino del proceso   
   CALL fn_gen_reporte_liquida()
{
   DISPLAY " ENVIA CORREO DEL REPORTE"
   -- se asigna el titulo del correo
   LET v_s_titulo_correo = "Proceso: LIQUIDACIÓN DE DEVOLUCIÓN DE SALDOS EXCEDENTES CRÉDITOS EN GARANTÍA 43BIS"

   -- se asigna el archivo a adjuntar
   LET v_s_archivo_correo = v_c_ruta_listados CLIPPED||"/"||v_v_nom_reporte CLIPPED||".pdf"

   -- se asigna el cuerpo del correo
   LET v_s_mens_correo =  "ID Proceso   : ",g_pid,"\n",
                          "Proceso      : DEVOLUCIÓN DE SALDOS EXCEDENTES CRÉDITOS EN GARANTÍA 43BIS\n",
                          "Operacion    : LIQUIDACIÓN\n",
                          "Fecha Inicio : ",TODAY,"\n",
                          "Fecha Fin    : ",TODAY

   -- se invoca la función que envía por correo el elemento generado
   CALL fn_correo_proceso(g_pid,
                          g_proceso_cod,
                          g_opera_cod,
                          v_s_archivo_correo,
                          v_s_titulo_correo,
                          v_s_mens_correo) 
}
   DISPLAY "=FIN="
END MAIN

#Funcion que genera el reporte de liquidacion
FUNCTION fn_gen_reporte_liquida()
   DEFINE v_s_comando      STRING, -- comando a ejecutar
          v_v_tabla        STRING, -- nombre de la tabla para el reporte
          v_c_programa_cod LIKE cat_operacion.programa_cod, -- nombre del programa
          r_c_ruta_bin     LIKE seg_modulo.ruta_bin, -- ruta bin
          r_ruta_listados  LIKE seg_modulo.ruta_listados -- ruta listados

   -- se inicializan variables
   LET v_v_tabla = "cta_movimiento" -- tabla de preliquidación

   -- se obtiene el nombrel del programa correspondiente
   --LET v_c_programa_cod = "GRTL40" -- programa lanzador
   LET v_c_programa_cod = fn_obten_nom_programa(g_proceso_cod , g_opera_cod)

   -- recupera la ruta de listados en el que se enviara el archivo
   CALL fn_rutas("grt") RETURNING r_c_ruta_bin, r_ruta_listados

   -- se crea el comando que ejecuta el modulo que reliza la integracion del archivo
   LET v_s_comando = "fglrun ",r_c_ruta_bin CLIPPED,"/GRTP19 ",
                                                 g_usuario_cod, " ",                                                 
                                                 g_pid, " ",
                                                 g_proceso_cod, " ",
                                                 g_opera_cod, " ",
                                                 g_folio_liquida, " ",
                                                 v_v_tabla, " ", 
                                                 v_c_programa_cod, " "

   RUN v_s_comando
END FUNCTION
