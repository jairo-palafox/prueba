-----------------------------------------------------------------------------------------
-- Modulo        => PAG                                                                    
-- Programa      => PAGB90
-- Objetivo      => Programa que preliquida pagos cambiaVit en automático
--               => por medio del monitor de proceso
-- Autor         => GERARDO ALFONSO VEGA PAREDES
-- Fecha inicio  => 29 de Agosto de 2018
-- Requerimiento => 
-----------------------------------------------------------------------------------------
-- Modificación => 
-- Fehca        => 
-- Autor        => 
-- Clave cambio => 
-----------------------------------------------------------------------------------------

DATABASE safre_viv

GLOBALS "PAGG01.4gl"  ---archivo de variables globales proceso_cod, opera_cod

GLOBALS
   DEFINE p_usuario_cod  LIKE seg_usuario.usuario_cod,
          g_pid          LIKE bat_ctr_proceso.pid,       --  ID del proceso
          g_proceso_cod  LIKE cat_proceso.proceso_cod,
          g_opera_cod    LIKE cat_operacion.opera_cod,
          p_folio        LIKE glo_folio.folio, 
          g_nom_archivo  STRING,
          g_programa     LIKE cat_operacion.programa_cod

END GLOBALS

MAIN

   -- se reciben los parametros del programa
   LET p_usuario_cod = ARG_VAL(1)
   LET g_pid         = ARG_VAL(2)
   LET g_proceso_cod = ARG_VAL(3)
   LET g_opera_cod   = ARG_VAL(4)
   LET p_folio       = ARG_VAL(5)
   LET g_nom_archivo = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".PAGB90.log")

   DISPLAY "Iniciando proceso de preliquidación de pagos cambiavit"

   LET p_folio = fn_genera_folio(g_proceso_cod, g_opera_cod, p_usuario_cod)
   
   CALL fn_genera_operacion_liq()

END MAIN


FUNCTION fn_genera_operacion_liq()

   DEFINE r_resultado_opera SMALLINT
   DEFINE v_mensaje STRING
   DEFINE v_proceso_desc LIKE cat_proceso.proceso_desc
   DEFINE v_fecha_inicio  DATE

   SELECT proceso_desc
   INTO   v_proceso_desc
   FROM   cat_proceso
   WHERE  proceso_cod = g_proceso_cod   

   LET v_fecha_inicio = TODAY
   
   CALL fn_display_proceso(0,"INICIO PRELIQUIDACIÓN CAMBIAVIT")

   CALL fn_genera_preliquidacion(v_fecha_inicio)

   CALL fn_display_proceso(1,"FIN PRELIQUIDACIÓN CAMBIAVIT")

   --DISPLAY "fn_actualiza_opera_fin"
   CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod) RETURNING r_resultado_opera
   LET v_mensaje = "El proceso de preliquidación ha finalizado correctamente"

   --DISPLAY "fn_correo_proceso"
   CALL fn_correo_proceso(g_pid,
                          g_proceso_cod,
                          g_opera_cod,
                          '', # Archivo adjunto
                          'Finalización de operación - '||v_proceso_desc CLIPPED||' - LIQUIDACIÓN CAMBIAVIT',
                           v_mensaje
                          )
END FUNCTION

FUNCTION fn_genera_preliquidacion(p_fecha_inicio)

   DEFINE v_sql STRING
   DEFINE p_fecha_inicio DATE

   DEFINE reg_preliquida RECORD
      f_liquida          DATE,
      id_derechohabiente DECIMAL(9,0),
      subcuenta          SMALLINT,
      fondo_inversion    SMALLINT,
      movimiento         SMALLINT,
      folio_liquida      DECIMAL(9,0),
      id_referencia      DECIMAL(9,0),
      monto_acciones     DECIMAL(16,6),
      monto_pesos        DECIMAL(12,2),
      f_valor            DATE,
      f_registro         DATE,
      h_registro         DATETIME HOUR TO SECOND,
      origen             CHAR(20)
   END RECORD
   
   DEFINE v_preliq_h_registro DATETIME HOUR TO SECOND

   DEFINE v_ruta_envio LIKE seg_modulo.ruta_envio
   DEFINE v_contador   DECIMAL(9,0)
   DEFINE v_cuantos    DECIMAL(9,0)
   DEFINE v_id_datos   SMALLINT
   DEFINE v_comando    STRING

{   WHENEVER ERROR CONTINUE
   LET v_comando = "DROP TABLE pag_preliquida_cvt"
   PREPARE cla_drop FROM v_comando
   EXECUTE cla_drop
   WHENEVER ERROR STOP

   LET v_comando = " CREATE TABLE pag_preliquida_cvt ",
                   " (f_liquida          DATE NOT NULL, ",
                   " id_derechohabiente DECIMAL(9,0) NOT NULL, ",
                   " subcuenta          SMALLINT NOT NULL, ",
                   " fondo_inversion    SMALLINT NOT NULL, ",
                   " movimiento         SMALLINT NOT NULL, ",
                   " folio_liquida      DECIMAL(9,0) NOT NULL, ",
                   " id_referencia      DECIMAL(9,0) NOT NULL, ",
                   " monto_acciones     DECIMAL(22,2), ",
                   " monto_pesos        DECIMAL(22,2), ",
                   " f_valor            DATE, ",
                   " f_registro         DATE, ",
                   " h_registro         DATETIME HOUR TO SECOND, ",
                   " origen             char(20) ",
                   " ) fragment by round robin in pag_1_dbs, pag_2_dbs "

   PREPARE cla_crea FROM v_comando
   EXECUTE cla_crea
}

   SELECT ruta_envio
   INTO   v_ruta_envio
   FROM   seg_modulo
   WHERE  modulo_cod = "pag"

   SELECT count(*)
   INTO   v_cuantos
   FROM   pag_det_cvt 
   WHERE  result_operacion = "01"

   IF v_cuantos IS NULL THEN
      LET v_cuantos = 0 
   END IF

   DISPLAY "Registros a Liquidar CAmbiaVit:", v_cuantos

   IF v_cuantos > 0 THEN

      LET v_id_datos = 1
      LET v_preliq_h_registro = CURRENT HOUR TO SECOND

      LET v_sql = " SELECT ","'",p_fecha_inicio,"',",
                          "id_derechohabiente,",
                          "64,",    --subcuenta
                          "14,",    --fondo inversión
                          "1841,",  --movimiento
                           p_folio,",",
                          "id_referencia,",
                          "fn_consulta_precio_fondo(monto_deposito,TODAY,14),",
                          "monto_deposito,",
                          "f_pago,",
                       "'",p_fecha_inicio,"',",
                       "'",v_preliq_h_registro,"',",
                         "num_caso",
                  " FROM  pag_det_cvt det",
                  " WHERE result_operacion = '01'              "

--      DISPLAY v_sql
      PREPARE exe_consulta_extractor FROM v_sql
      DECLARE cur_extractor CURSOR FOR exe_consulta_extractor

      LET v_contador = 0

      FOREACH cur_extractor INTO reg_preliquida.*
         IF reg_preliquida.monto_acciones IS NULL THEN
            LET reg_preliquida.monto_acciones = 0
         END IF

         INSERT INTO pag_preliquida_cvt VALUES (reg_preliquida.*)
         
         UPDATE pag_det_cvt
         SET    folio = p_folio,
                result_operacion = "03"
         WHERE  result_operacion = "01"

         UPDATE glo_folio
         SET    status = 1       --para que salga en la consulta de prelquidación
         WHERE  folio = p_folio
         AND    proceso_cod = g_proceso_cod
         AND    status = 0 

         UPDATE bat_ctr_proceso
         SET    folio = p_folio
         WHERE  pid = g_pid
         AND    proceso_cod = g_proceso_cod

         UPDATE bat_ctr_operacion
         SET    folio = p_folio
         WHERE  pid = g_pid
         AND    proceso_cod = g_proceso_cod
         AND    opera_cod   = g_opera_cod

         LET v_contador = v_contador + 1
      END FOREACH

   ELSE

      UPDATE bat_ctr_proceso
      SET    estado_cod = 4
      WHERE  pid = g_pid
      AND    proceso_cod = g_proceso_cod

      UPDATE bat_ctr_operacion
      SET    estado_cod = 4
      WHERE  pid = g_pid
      AND    proceso_cod = g_proceso_cod
      AND    opera_cod   = 4

      CALL fn_display_proceso(1,"NO EXISTEN DATOS PARA PRELIQUIDAR CAMBIAVIT")
   END IF

END FUNCTION