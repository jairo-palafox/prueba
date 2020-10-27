--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

#########################################################################
#Modulo            =>GRT                                                #
#Programa          =>GRTP29                                             #
#Objetivo          =>Programa que ejecuta la preliquidación de          #
#                    fortalecmiento de crédito GRT                      #
#Autor             =>Daniel Buendia, EFP                                #
#Fecha inicio      =>17 Julio 2012                                      #
#########################################################################

DATABASE safre_viv
GLOBALS "GRTG01.4gl"

MAIN
   DEFINE p_usuario_cod          LIKE seg_usuario.usuario_cod  -- clave del usuario firmado
   DEFINE p_pid                  LIKE bat_ctr_proceso.pid      --  ID del proceso
   DEFINE p_proceso_cod          LIKE cat_proceso.proceso_cod  -- codigo del proceso
   DEFINE p_opera_cod            LIKE cat_operacion.opera_cod  -- codigo de operacion
   DEFINE p_folio_liquida        LIKE cta_movimiento.folio_liquida  -- folio de liquidación
   DEFINE p_v_arch_proceso       LIKE bat_ctr_operacion.nom_archivo -- archivo del proceso
   DEFINE p_id_cre_ctr_archivo   LIKE cre_acreditado.id_cre_ctr_archivo -- id del archivo 
   DEFINE v_c_programa_cod       LIKE cat_operacion.programa_cod -- nombre del programa
   DEFINE v_s_qryTxt             STRING   -- guarda una sentencia SQL a ejecutar
   DEFINE v_i_tot_registros      INTEGER  -- total de registros insertados en DSE
   DEFINE v_r_dse_ctr_archivo    RECORD LIKE dse_ctr_archivo.* -- registro de la tabla de control DSE
   DEFINE v_c_tpo_transferencia  LIKE dse_agrupa_devolucion.tpo_transferencia -- tipo de trasnferencia
   DEFINE r_si_lote              LIKE dse_ctr_archivo.lote -- lote de la tabla de control de archivos DSE
   DEFINE r_si_cod_error         SMALLINT  -- contiene el codigo de error en caso de excepción
   DEFINE r_b_exist_err          SMALLINT  -- contiene el estado de la preliquidación
   DEFINE r_isam_err             INTEGER 
   DEFINE r_c_msj                VARCHAR(250)
   DEFINE r_b_valida             SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   -- se recuperan los parametros que envia el programa lanzador
   LET p_usuario_cod           = ARG_VAL(1)
   LET p_pid                   = ARG_VAL(2)
   LET p_proceso_cod           = ARG_VAL(3)
   LET p_opera_cod             = ARG_VAL(4)
   LET p_folio_liquida         = ARG_VAL(5)
   LET p_id_cre_ctr_archivo    = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".GRTP29.log")

   DISPLAY "=INICIA GRTP29="
   DISPLAY " USUARIO       : ",p_usuario_cod
   DISPLAY " PID           : ",p_pid

   -- se inicializan variables
   LET v_c_programa_cod = "GRTP29"
   LET v_c_tpo_transferencia = 16 -- Solicitud Saldo Garantía

   -- se invoca la funcion que inicializa el proceso
   LET r_b_valida = fn_inicializa_proceso(p_pid,
                                          p_proceso_cod,
                                          p_opera_cod,
                                          p_folio_liquida,
                                          v_c_programa_cod,
                                          "N/A",
                                          p_usuario_cod)

   -- se verifica si fue posible inicializar el proceso
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF

   -- se invoca la función que deja la operación en estado Procesando
   LET r_b_valida = fn_actualiza_opera_ini(p_pid,
                                           p_proceso_cod,
                                           p_opera_cod,
                                           p_folio_liquida,
                                           "GRTP29",
                                           "N/A",
                                           p_usuario_cod)

   -- se verifica si fue posible inicializar el proceso
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      EXIT PROGRAM
   END IF


   -- se crea la tabla de preliquidación correspondiente a GRT
   CALL fn_crea_tbl_preliquidacion()

   -- se invoca la funcion crea la tabla temporal liquida_deudor
   CALL fn_crea_tbl_temp_liquida_deudor()

   -- se genera el folio
   LET p_folio_liquida = fn_genera_folio(p_proceso_cod, p_opera_cod, p_usuario_cod)
   DISPLAY " FOLIO         : ",p_folio_liquida USING "#########&"

   -- se ejecuta el la función que procesa los creditos y los preliquida
   LET v_s_qryTxt = "EXECUTE FUNCTION fn_grt_preliquidacion(?,?)"

   PREPARE prp_preliquida FROM v_s_qryTxt
   EXECUTE prp_preliquida USING p_folio_liquida,p_id_cre_ctr_archivo
                           INTO r_si_cod_error, r_b_exist_err, r_isam_err, r_c_msj

   IF r_si_cod_error <> 0 OR r_b_exist_err = 1 THEN
      -- se invoca la función que deja la operación en estado ERRONEA
      LET r_b_valida = fn_error_opera(p_pid, p_proceso_cod, p_opera_cod)

      -- se verifica si fue posible finalizar la operacion
      IF r_b_valida <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF
      
      
      DISPLAY "OCURRIÓ UN ERROR EN LA PRELIQUIDACIÓN: ",r_si_cod_error
      DISPLAY "CÓD. ERROR : ",r_si_cod_error
      DISPLAY "ESTADO     : ",r_b_exist_err
      DISPLAY "ISAM ERR   : ",r_isam_err
      DISPLAY "MENSAJE ERR: ",r_c_msj

      EXIT PROGRAM
   ELSE


      -- se consulta el total de registros insertados
      LET v_i_tot_registros = f_obt_tot_regs_dse(p_folio_liquida)
      DISPLAY "REGISTROS INSERTADOS : ",v_i_tot_registros 
      -- verifica si ya existe el registro en la tabla de control de archivo DSE
      CALL fn_obtiene_lote_dse(v_c_tpo_transferencia) RETURNING r_si_lote
      
      DISPLAY " INSERTA REGISTRO EN LA TABLA DE CONTROL DSE"
      LET v_r_dse_ctr_archivo.tpo_transferencia = v_c_tpo_transferencia
      LET v_r_dse_ctr_archivo.lote              = r_si_lote
      LET v_r_dse_ctr_archivo.f_lote            = TODAY
      LET v_r_dse_ctr_archivo.tot_registros     = v_i_tot_registros
      LET v_r_dse_ctr_archivo.tot_aceptados     = v_i_tot_registros
      LET v_r_dse_ctr_archivo.tot_rechazados    = 0
      LET v_r_dse_ctr_archivo.estado            = 20
      LET v_r_dse_ctr_archivo.f_proceso         = TODAY
      LET v_r_dse_ctr_archivo.usuario           = p_usuario_cod
      LET v_r_dse_ctr_archivo.folio             = p_folio_liquida
      LET v_r_dse_ctr_archivo.nom_archivo       = fn_obt_nombre_recurrente()

      -- se inserta registro
      INSERT INTO dse_ctr_archivo VALUES (v_r_dse_ctr_archivo.*)

      -- se actualiza el folio a estatus "PRELIQUIDADO"
      UPDATE glo_folio
         SET status = 1
       WHERE folio = p_folio_liquida

      -- se invoca la función que deja la operación en estado Finalizado
      LET r_b_valida = fn_actualiza_opera_fin(p_pid, p_proceso_cod, p_opera_cod)

      -- se verifica si fue posible finalizar la operacion
      IF r_b_valida <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)

         EXIT PROGRAM

      END IF
   END IF

   DISPLAY "=FIN="
END MAIN

#Objetivo: Función que realiza la consulta de registros insertados en la tabla de
#          agrupación para el folio que entra como parámetro
FUNCTION f_obt_tot_regs_dse(p_d_folio)
   DEFINE p_d_folio     LIKE glo_folio.folio, -- folio
          v_i_tot_regs  INTEGER, -- total de registros consultados
          v_s_qryTxt    STRING -- contiene una sentencia sql a ejecutar

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
   DEFINE v_nom_archivo   LIKE dse_ctr_archivo.nom_archivo
   DEFINE v_si_id_proceso LIKE cre_ctr_archivo.id_proceso -- identificador del proceso
   DEFINE v_s_qryTxt      STRING -- se asigna consulta sql a ejecutar

   -- se inicializan variables
   LET v_si_id_proceso = g_id_proceso_grt

   -- se asigna la sentencia que obtiene el nombre del archivo
   LET v_s_qryTxt = " SELECT FIRST 1 nom_archivo\n",
                    "   FROM cre_ctr_archivo\n",
                    "  WHERE id_proceso = ",v_si_id_proceso,"\n",
                    "    AND operacion = 21\n",
                    "  ORDER BY id_cre_ctr_archivo DESC "

   PREPARE prp_obt_nom FROM v_s_qryTxt
   EXECUTE prp_obt_nom INTO v_nom_archivo
   
   RETURN v_nom_archivo   

END FUNCTION 

# Objetivo: Función que crea la tabla de preliquidación
FUNCTION fn_crea_tbl_preliquidacion()
   DEFINE v_s_qryTxt STRING -- guarda una sentencia SQL a ejecutar

   -- se crea la sentencia sql que elimina la tabla de preliquidación
   LET v_s_qryTxt = " DROP TABLE IF EXISTS cre_sg_preliquida;"

   PREPARE prp_drop_preliquida FROM v_s_qryTxt
   EXECUTE prp_drop_preliquida

   -- se crea la sentencia sql que crea la tabla de preliquidación
   LET v_s_qryTxt = " CREATE TABLE cre_sg_preliquida\n",
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

# Objetivo: Función que crea la tabla temporal de deudor
FUNCTION fn_crea_tbl_temp_liquida_deudor()
   -- se declara la base de datos temporal
   DATABASE safre_tmp

   WHENEVER ERROR CONTINUE

   DROP TABLE tmp_liquida_deudor_grt
   CREATE TABLE tmp_liquida_deudor_grt(id_cre_acreditado DECIMAL(9,0),
                                       folio_liquida DECIMAL(9,0),
                                       f_liquida DATE,
                                       movimiento SMALLINT,
                                       monto_aivs DECIMAL(22,2),
                                       monto_pesos DECIMAL(22,2),
                                       f_movimiento DATE)

   WHENEVER ERROR STOP

   -- regresa a la base de datoa safre viv
   DATABASE safre_viv
END FUNCTION

#Objetivo: Obtiene el lote DSE correspondiente para el tipo de transferencia que
#          entra como paramentro
FUNCTION fn_obtiene_lote_dse(p_c_tpo_transferencia)
   DEFINE p_c_tpo_transferencia   LIKE dse_ctr_archivo.tpo_transferencia -- tipo de transferencia
   DEFINE v_si_lote               LIKE dse_ctr_archivo.lote -- lote DSE
   DEFINE v_s_qry_txt             STRING

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
