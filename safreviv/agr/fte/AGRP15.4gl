--===============================================================
-- Versión: 1.0.0
-- Fecha última modificación:
--===============================================================

##########################################################################
#Modulo            => AGR                                                #
#Programa          => AGRP15                                             #
#Objetivo          => Programa que realiza la preliquidación para el     #
#                     módulo de Anualidades Garantizadas                 #
#Autor             => Daniel Buendia, EFP                                #
#Fecha inicio      => 11 Mayo 2012                                       #
#Actualización     => Mauro Muñiz Caballero                              #
#Fecha actualiz    => 20 de noviembre de 2014                            #
#                     se debe liquidar con cargo a capital los deudores  #
#                     con marca rechazada por Trámite Judicial           #
#                     REQUERIMIENTO CANCELADO                            #
#Actualización     => Mauro Muñiz Caballero                              #
#Fecha actualiz    => 26 de agosto de 2015                               #
#                     liquidar ministraciones                            #
#Modifica:         => Mauro Muñiz Caballero                              #
#Fecha modif:      => 9 de noviembre de 2015                             #
#Adecuación        => Liquidación de deudor para registros marcados      #
#                     por Procesar                                       #
#Modifica:         => Mauro Muñiz Caballero                              #
#Fecha modif:      => 24 de mayo de 2016                                 #
#Adecuación        => Liquidación proyectada al 1 día siguiente mes      #
#                     Generación de archivos de deudor, amortización     #
#                     y liquidación usos de anualidad                    #
#Modifica:         => Mauro Muñiz Caballero                              #
#Fecha modif:      => 14 de agosto de 2017                               #
#Adecuación        => Liquidación regitros crédito liquidado             #
#Autor modifica    => Emilio Abarca, EFP.                                #
#Fecha modifica    => 24/07/2019                                         #
#Objetivo          => Carga de registros con adelantos conciliados.      #
##########################################################################

DATABASE safre_viv

GLOBALS "AGRG01.4gl"

GLOBALS

   DEFINE p_v_usuario_cod           LIKE seg_usuario.usuario -- nombre del usuario
   DEFINE p_d_pid                   LIKE bat_ctr_proceso.pid -- pid de la operación
   DEFINE p_i_proceso_cod           LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE p_i_opera_cod             LIKE cat_operacion.opera_cod -- codigo de la operacion de la etapa
   DEFINE p_d_folio                 LIKE glo_folio.folio -- numero de folio
   DEFINE p_v_arch_proceso          VARCHAR(100) -- nombre del archivo a reversar
   DEFINE p_f_liq                   DATE
   DEFINE v_si_subcuenta            LIKE cta_movimiento.subcuenta -- subcuenta
   DEFINE v_c_subcuenta_desc        LIKE cat_subcuenta.subcuenta_desc -- descripción de subcuenta
   DEFINE v_si_movimiento           LIKE cre_saldo_deudor.movimiento -- movimiento
   DEFINE v_c_movto_desc            LIKE cat_movimiento.movimiento_desc -- descripción del movimiento
   DEFINE v_si_tpo_originacion      LIKE cre_acreditado.tpo_originacion -- tipo de originación
   DEFINE v_si_tpo_transferenc      LIKE cre_uso_garantia.tpo_transferencia -- tipo de transferencia
   DEFINE v_ind_tipo_ejecucion      LIKE bat_ctr_operacion.ind_tipo_ejecucion -- tipo de ejecucion (0-manual, 1-batch)
   DEFINE v_c_programa_cod          LIKE cat_operacion.programa_cod -- programa de la operación
   DEFINE v_d_monto_aivs            DECIMAL(25,2) -- suma del monto en aivs
   DEFINE v_d_monto_pesos           DECIMAL(25,2) -- suma del monto en pesos
   DEFINE v_s_qryTxt                STRING -- guarda una sentencia SQL a ejecutar
   DEFINE v_s_comando               STRING -- contiene al comando a correr
   DEFINE v_r_dse_ctr_archivo       RECORD LIKE dse_ctr_archivo.* -- registro de dse ctr archivo
   DEFINE v_i_tot_registros         INTEGER -- total de registros
   DEFINE r_si_lote                 LIKE dse_ctr_archivo.lote -- lote de la tabla de control de archivos DSE
   DEFINE r_c_ruta_bin              LIKE seg_modulo.ruta_bin -- ruta bin del módulo
   DEFINE r_c_ruta_listados         LIKE seg_modulo.ruta_listados -- ruta listados del módulo
   DEFINE r_cod_error               SMALLINT -- contiene el codigo de error en caso de excepción
   DEFINE r_i_estado                SMALLINT -- estado al que se va a actualizar
   DEFINE r_isam_err                INTEGER 
   DEFINE r_c_msj                   VARCHAR(250)
   DEFINE r_b_valida                SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

END GLOBALS

MAIN

   -- se recuperan los parámetros que envia el programa lanzador
   LET p_v_usuario_cod  = ARG_VAL(1)
   LET p_d_pid          = ARG_VAL(2)
   LET p_i_proceso_cod  = ARG_VAL(3)
   LET p_i_opera_cod    = ARG_VAL(4)
   LET p_d_folio        = ARG_VAL(5)
   LET p_f_liq          = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario_cod CLIPPED|| ".AGRP15.log")

   DISPLAY "=INICIA AGRP15="
   DISPLAY "USUARIO    : ",p_v_usuario_cod
   DISPLAY "PID        : ",p_d_pid CLIPPED
   DISPLAY "PROCESO    : ",p_i_proceso_cod

   -- Realiza carga de archivo para conciliación de adelantos
   CALL fn_crea_temporal()
   CALL fn_carga_adelantos()

   -- se inicializan variables
   LET v_si_tpo_originacion = 4 -- Anualidades Garantizadas
   LET v_si_tpo_transferenc = "43" -- Anualidades Garantizadas

   -- se obtiene el tipo de proceso de la tabla de control de procesos
   LET v_s_qryTxt = " SELECT ind_tipo_ejecucion\n",
                    "   FROM bat_ctr_operacion\n",
                    "  WHERE pid = ",p_d_pid,"\n",
                    "    AND proceso_cod = ",p_i_proceso_cod,"\n",
                    "    AND opera_cod = ",p_i_opera_cod

   PREPARE prp_tipo_ejecucion FROM v_s_qryTxt
   EXECUTE prp_tipo_ejecucion INTO v_ind_tipo_ejecucion

   -- se valida si la operaciÓn se ejecutó manualmente o a través del batch
   IF v_ind_tipo_ejecucion = 1 THEN
      -- se genera el folio
      LET p_d_folio = fn_genera_folio(p_i_proceso_cod, p_i_opera_cod, p_v_usuario_cod)
   END IF

   DISPLAY "FOLIO      : ",p_i_proceso_cod

   IF p_f_liq IS NULL THEN
      SELECT MAX(f_presentacion)
        INTO p_f_liq
        FROM safre_tmp:tmp_sdo_transf_det_agr

      LET p_f_liq = MDY(MONTH(p_f_liq),1,YEAR(p_f_liq)) + 1 UNITS MONTH
   END IF

   DISPLAY "Folio liquidación       : ",p_d_folio USING "#########&"
   DISPLAY "Fecha valor liquidación : ",p_f_liq USING "dd-mm-yyyy"

   -- se invoca la funcion que crea la tablas de preliquidación
   LET v_s_qryTxt = "EXECUTE PROCEDURE sp_crea_tbl_preliq_ag()"

   PREPARE prp_preliq FROM v_s_qryTxt

   EXECUTE prp_preliq

   DATABASE safre_tmp
   -- se invoca la funcion que crea la tabla temporal de saldo deudor
   LET v_s_qryTxt = "EXECUTE PROCEDURE sp_crea_tbl_preliq_tmp()"

   PREPARE prp_preliq_tmp FROM v_s_qryTxt

   EXECUTE prp_preliq_tmp
   DATABASE safre_viv

   -- se ejecuta el la función que procesa los créditos y los preliquida
   LET v_s_qryTxt = "EXECUTE FUNCTION fn_cre_preliquidacion(?,?,?,?,?)"

   PREPARE prp_preliquida FROM v_s_qryTxt
   EXECUTE prp_preliquida USING p_d_folio,
                                v_si_tpo_originacion,
                                p_v_usuario_cod,
                                p_i_proceso_cod,
                                p_f_liq
                           INTO r_cod_error, r_i_estado

   IF r_cod_error <> 0 THEN
      DISPLAY "OCURRIÓ UN ERROR EN LA PRELIQUIDACIÓN (CRE): ",r_cod_error

      -- se invoca la función que deja la operación en estado de ERROR
      LET r_b_valida = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

      EXIT PROGRAM
   END IF

   -- se ejecuta el la función que procesa los creditos de uso y los preliquida
   LET v_s_qryTxt = "EXECUTE FUNCTION fn_uso_preliquidacion(?,?,?)"

   PREPARE prp_uso_preliquida FROM v_s_qryTxt
   EXECUTE prp_uso_preliquida USING p_d_folio, v_si_tpo_transferenc, p_f_liq
                               INTO r_cod_error,
                                    r_i_estado,
                                    r_isam_err,
                                    r_c_msj

   IF r_cod_error <> 0 THEN
      DISPLAY "OCURRIÓ UN ERROR EN EL PROCESO DE PRELIQUIDACIÓN USO"
      DISPLAY "CÓD. ERROR : ",r_cod_error
      DISPLAY "ISAM ERR   : ",r_isam_err
      DISPLAY "MENSAJE ERR: ",r_c_msj

      -- se invoca la función que deja la operación en estado de ERROR
      LET r_b_valida = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

      EXIT PROGRAM
   END IF

     -- se consulta el total de registros insertados
   LET v_i_tot_registros = f_obt_tot_regs_dse(p_d_folio)

   -- verifica si ya existe el registro en la tabla de control de archivo DSE
   CALL fn_obtiene_lote_dse(v_si_tpo_transferenc) RETURNING r_si_lote

   DISPLAY " INSERTA REGISTRO EN LA TABLA DE CONTROL DSE"
   LET v_r_dse_ctr_archivo.tpo_transferencia = v_si_tpo_transferenc
   LET v_r_dse_ctr_archivo.lote              = r_si_lote
   LET v_r_dse_ctr_archivo.f_lote            = TODAY
   LET v_r_dse_ctr_archivo.tot_registros     = v_i_tot_registros
   LET v_r_dse_ctr_archivo.tot_aceptados     = v_i_tot_registros
   LET v_r_dse_ctr_archivo.tot_rechazados    = 0
   LET v_r_dse_ctr_archivo.estado            = 20
   LET v_r_dse_ctr_archivo.f_proceso         = TODAY
   LET v_r_dse_ctr_archivo.usuario           = p_v_usuario_cod
   LET v_r_dse_ctr_archivo.folio             = p_d_folio
   LET v_r_dse_ctr_archivo.nom_archivo       = fn_obt_nombre_recurrente()

   --se inserta registro
   INSERT INTO dse_ctr_archivo VALUES (v_r_dse_ctr_archivo.*)

   -- se actualiza el status de glo folio, para que el folio lo pueda tomar la función
   -- general de liquidación
   UPDATE glo_folio
      SET status = 1
    WHERE folio  = p_d_folio

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
   DISPLAY " = SALDO DEUDOR ="

   -- se realiza la consulta para para mostrar a usuario
   LET v_s_qryTxt = " SELECT sdo.movimiento, cat.movimiento_desc, SUM(sdo.monto_aivs), SUM(sdo.monto_pesos)\n",
                    "   FROM cre_saldo_deudor sdo, cat_movimiento cat\n",
                    "  WHERE sdo.id_cre_acreditado IN (\n",
                    "        SELECT id_cre_acreditado\n",
                    "          FROM cre_acreditado\n",
                    "         WHERE estado in (130,135,138,137,330,335)\n",
                    "           AND tpo_originacion = ",v_si_tpo_originacion,")\n",
                    "    AND sdo.movimiento = cat.movimiento\n",
                    "  GROUP BY 1, 2\n",
                    "  ORDER BY 1"

   PREPARE prp_cre_sdo_deudor FROM v_s_qryTxt
   DECLARE cur_cre_sdo_deudor CURSOR FOR prp_cre_sdo_deudor

   FOREACH cur_cre_sdo_deudor INTO v_si_movimiento, v_c_movto_desc, v_d_monto_aivs, v_d_monto_pesos
      DISPLAY "MOVIMIENTO   ",v_si_movimiento, "-",v_c_movto_desc
      DISPLAY "MONTO AIVS   ",v_d_monto_aivs
      DISPLAY "MONTO_PESOS  ",v_d_monto_pesos
   END FOREACH

   DISPLAY " = PRELIQUIDACIÓN ="
   -- se realiza la consulta para para mostrar a usuario
   LET v_s_qryTxt = " SELECT pre.subcuenta, sub.subcuenta_desc, pre.movimiento, mto.movimiento_desc,\n",
                    "        SUM(pre.monto_acciones), SUM(pre.monto_pesos)\n",
                    "   FROM cre_ag_preliquida pre, cat_subcuenta sub, cat_movimiento mto\n",
                    "  WHERE pre.subcuenta = sub.subcuenta\n",
                    "    AND pre.movimiento = mto.movimiento\n",
                    "  GROUP BY 1,2,3,4\n",
                    "  ORDER BY 1,3"

   PREPARE prp_preliquida_agr FROM v_s_qryTxt
   DECLARE cur_preliquida_agr CURSOR FOR prp_preliquida_agr

   FOREACH cur_preliquida_agr INTO v_si_subcuenta, v_c_subcuenta_desc, v_si_movimiento,
                                   v_c_movto_desc, v_d_monto_aivs, v_d_monto_pesos
      DISPLAY "SUBCUENTA    ", v_si_subcuenta, "-", v_c_subcuenta_desc
      DISPLAY "MOVIMIENTO   ", v_si_movimiento, "-", v_c_movto_desc
      DISPLAY "MONTO AIVS   ", v_d_monto_aivs
      DISPLAY "MONTO PESOS  ", v_d_monto_pesos
   END FOREACH

   -- se obtiene la ruta bin y de listados del módulo
   CALL fn_rutas("agr") RETURNING r_c_ruta_bin, r_c_ruta_listados

   DISPLAY "SE GENERA REPORTE DE PRELIQUIDACIÓN"
   -- se obtiene el nombrel del programa correspondiente
   LET v_c_programa_cod = fn_obten_nom_programa(p_i_proceso_cod , p_i_opera_cod)

   --ejecuta el programa que genera el reporte de preliquidacion
   LET v_s_comando = "fglrun ",r_c_ruta_bin CLIPPED,"/AGRP16 ",
                               p_v_usuario_cod, " ",
                               p_d_pid, " ",
                               p_i_proceso_cod, " ",
                               p_i_opera_cod, " ",
                               p_d_folio, " ",
                               "cre_ag_preliquida", " ",
                               v_c_programa_cod --"AGRL17"

   --DISPLAY " v_s_comando ", v_s_comando

   RUN v_s_comando

   DISPLAY ""
   DISPLAY "SE GENERAN ARCHIVOS DE LIQUIDACIÓN DE DEUDOR, AMORTIZACIÓN, CARGO A CAPITAL Y LIQUIDACIÓN DE USOS DE ANUALIDAD Y GARANTÍA AG"

   -- se crea el comando que ejecuta el módulo que genera el archivo de salida de liquidación
   LET v_s_qryTxt = " fglrun ",r_c_ruta_bin CLIPPED,"/AGRP38 ",
                    p_v_usuario_cod, " ",
                    p_d_pid, " ",
                    p_i_proceso_cod, " ",
                    p_i_opera_cod, " ",
                    p_d_folio

   DISPLAY "v_s_qry: ",v_s_qryTxt

   RUN v_s_qryTxt

   DISPLAY "PROCESO DE PRELIQUIDACIÓN FINALIZADO"

END MAIN

#Objetivo: Función que realiza la consulta de registros insertados en la tabla de
#          agrupación para el folio que entra como parámetro
FUNCTION f_obt_tot_regs_dse(p_d_folio)

   DEFINE p_d_folio     LIKE glo_folio.folio -- folio
   DEFINE v_i_tot_regs  INTEGER -- total de registros consultados
   DEFINE v_s_qryTxt    STRING -- contiene una sentencia sql a ejecutar

   -- se inicializan variables
   LET v_i_tot_regs = 0

   -- se consulta el numero de registros insertados en la tabla de agrupación
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM dse_agrupa_devolucion\n",
                    "  WHERE folio_liquida = ",p_d_folio

   PREPARE prp_count_agrupa_folio FROM v_s_qryTxt
   EXECUTE prp_count_agrupa_folio INTO v_i_tot_regs

   RETURN v_i_tot_regs

END FUNCTION

#Objetivo: Obtiene el nombre del archivo recurrente
FUNCTION fn_obt_nombre_recurrente()

   DEFINE v_nom_archivo             LIKE dse_ctr_archivo.nom_archivo
   DEFINE v_si_id_proceso           LIKE cre_ctr_archivo.id_proceso -- identificador del proceso
   DEFINE v_s_qryTxt                STRING -- se asigna consulta sql a ejecutar

   -- se inicializan variables
   LET v_si_id_proceso = g_id_proceso_agr

   -- se crea la sentencia que consulta el nombre del archivo
   LET v_s_qryTxt = " SELECT FIRST 1 nom_archivo ",
                    "   FROM cre_ctr_archivo ",
                    "  WHERE id_proceso = ",v_si_id_proceso,
                    "    AND operacion = 21 ",
                    "  ORDER BY id_cre_ctr_archivo DESC "

   PREPARE prp_obt_nom FROM v_s_qryTxt
   EXECUTE prp_obt_nom INTO v_nom_archivo

   RETURN v_nom_archivo

END FUNCTION 

#Objetivo: Obtiene el lote DSE correspondiente para el tipo de transferencia que
#          entra como paramentro
FUNCTION fn_obtiene_lote_dse(p_c_tpo_transferencia)

   DEFINE p_c_tpo_transferencia     LIKE dse_ctr_archivo.tpo_transferencia -- tipo de transferencia
   DEFINE v_si_lote                 LIKE dse_ctr_archivo.lote -- lote DSE
   DEFINE v_s_qry_txt               STRING

   -- se obtiene el máximo lote para el tpo de transferencia y la fecha de lote (TODAY)
   LET v_s_qry_txt = " SELECT MAX(lote)\n",
                     "   FROM dse_ctr_archivo\n",
                     "  WHERE tpo_transferencia = '",p_c_tpo_transferencia,"'\n",
                     "    AND f_lote = TODAY"

   PREPARE prp_max_lote_dse FROM v_s_qry_txt
   EXECUTE prp_max_lote_dse INTO v_si_lote

   -- se valida el lote obtenido
   IF v_si_lote IS NULL THEN
      LET v_si_lote = 1
   ELSE
      LET v_si_lote = v_si_lote + 1
   END IF

   RETURN v_si_lote

END FUNCTION

# Objetivo: Función que realiza conciliación de adelantos
FUNCTION fn_carga_adelantos()

   DEFINE v_ruta_rescate      CHAR(40)
   DEFINE v_ruta_envio        CHAR(40)
   DEFINE v_archivo           CHAR(20)
   DEFINE v_ruta_absoluta     CHAR(80)
   DEFINE v_archivo_res       CHAR(30)
   DEFINE v_ruta_absoluta_res CHAR(90)
   DEFINE v_indicador         SMALLINT
   DEFINE v_t_modulo          INTEGER
   DEFINE v_unix_comando      STRING
   DEFINE r_conciliacion      RECORD
      modulo      CHAR(3),
      subcuenta   SMALLINT,
      total       INTEGER
   END RECORD

   WHENEVER ERROR CONTINUE

   SELECT ruta_rescate,
          ruta_envio
     INTO v_ruta_rescate,
          v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'agr'
   
   DISPLAY ""
   DISPLAY " Carga registros con adelanto"

   LET v_archivo = "Arhconcad.cnca"
   LET v_ruta_absoluta = v_ruta_rescate CLIPPED,"/",v_archivo CLIPPED

   DISPLAY " - Archivo a cargar: ", v_ruta_absoluta
   
   LOAD FROM v_ruta_absoluta INSERT INTO safre_tmp:tmp_nss_concilia;
   
   -- Obtiene derechohabiente
    SELECT a.id_derechohabiente,
           t.nss,
           t.subcuenta,
           t.monto,
           t.modulo
      FROM safre_tmp:tmp_nss_concilia t,
           afi_derechohabiente a
     WHERE t.nss = a.nss
     INTO TEMP reg_tmp_concilia;

   DISPLAY " Respalda temporal adelantos"

   LET v_archivo_res = "tmp_adelanto_", TODAY USING "ddmmyyyy",".unl"
   LET v_ruta_absoluta_res = v_ruta_envio CLIPPED,"/",v_archivo_res

   UNLOAD TO v_ruta_absoluta_res SELECT * FROM safre_tmp:tmp_adelanto;

   DISPLAY " - Respaldo Adelantos: ",v_ruta_absoluta_res
   
   -- Verifica registros conciliados
    SELECT tc.id_derechohabiente,
           tc.nss,
           ta.f_liquida,
           tc.subcuenta,
           tc.monto,
           tc.modulo
      FROM reg_tmp_concilia tc, 
           safre_tmp:tmp_adelanto ta
     WHERE tc.id_derechohabiente = ta.id_derechohabiente
       AND tc.modulo    = ta.modulo
       AND tc.subcuenta = ta.subcuenta
     INTO TEMP reg_tmp_adelanto;

    DISPLAY " Respalda temporal reg. conciliados"

   LET v_archivo_res = "reg_tmp_adelanto_", TODAY USING "ddmmyyyy",".unl"
   LET v_ruta_absoluta_res = v_ruta_envio CLIPPED,"/",v_archivo_res

   UNLOAD TO v_ruta_absoluta_res SELECT * FROM reg_tmp_adelanto;

   DISPLAY " - Respaldo reg. conciliados: ",v_ruta_absoluta_res
   DISPLAY ""

   # El total de registros modulo-subcuenta de reg_tmp_adelanto debe ser menor
   # a el total de registros modulo_subcuenta de tmp_adelanto

   DECLARE cur_conciliacion CURSOR FOR
   SELECT modulo,
          subcuenta,
          COUNT(*)
     FROM reg_tmp_adelanto
     GROUP BY 1,2
     ORDER BY 1,2

   LET v_indicador = 0
   LET v_t_modulo  = 0

   FOREACH cur_conciliacion INTO r_conciliacion.modulo,
                                 r_conciliacion.subcuenta,
                                 r_conciliacion.total

      SELECT COUNT(*)
        INTO v_t_modulo
        FROM safre_tmp:tmp_adelanto 
       WHERE modulo    = r_conciliacion.modulo
         AND subcuenta = r_conciliacion.subcuenta

      IF ( r_conciliacion.total > v_t_modulo ) THEN
         LET v_indicador = 1
         EXIT FOREACH
      END IF

   END FOREACH

   IF ( v_indicador = 1 ) THEN
      DISPLAY ""
      DISPLAY " ***************************************************************"
      DISPLAY " EL TOTAL DE REGISTROS DEL ARCHIVO DE ADELANTOS NO ES CORRECTO "
      DISPLAY " ***************************************************************"
      DISPLAY ""
   ELSE
      DISPLAY ""
      DISPLAY " ***************************************************************"
      DISPLAY " EL ARCHIVO DE ADELANTOS SE CARGÓ CORRECTAMENTE"
      DISPLAY " ***************************************************************"
      DISPLAY ""
    
      -- Procede a eliminar registros de tmp_adelanto
      DELETE FROM safre_tmp:tmp_adelanto;

      -- Realiza conciliación
      INSERT INTO safre_tmp:tmp_adelanto
      SELECT *
        FROM reg_tmp_adelanto;

      -- Respaldo del archivo conciliado
      LET v_unix_comando = " cp "||v_ruta_absoluta CLIPPED||"  "||v_ruta_rescate CLIPPED,"/Arhconcad_", TODAY USING "ddmmyyyy",".cnca"
      RUN v_unix_comando
      
   END IF

   WHENEVER ERROR STOP

END FUNCTION

# Objetivo: Función que crea tabla para archivo de adelantos
FUNCTION fn_crea_temporal()

   DATABASE safre_tmp

   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_nss_concilia;
      CREATE TABLE tmp_nss_concilia(modulo    CHAR(3),
                                    nss       CHAR(11),
                                    subcuenta SMALLINT,
                                    monto     DECIMAL(12,2));

   WHENEVER ERROR STOP
      DATABASE safre_viv

END FUNCTION
