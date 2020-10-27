##############################################################################################
#Modulo            =>RET                                                                     #
#Programa          =>RETS131                                                                 #
#Objetivo          =>Programa que ejecuta el proceso de generacion                           #
#                    de archivo de de salida en texto, separado por pipes, al realizar la    #
#                    preliquidación, que indique los registros en los que el abono fue mayor #
#                    a 1000 AIVS, el archivo deberá de contener NSS, Importe del movimiento  #
#                    de abono en la subcuenta 92 en AIVS, Importe del movimiento de abono en #
#                    la subcuenta 97 en AIVS, Saldo en AIVS de la SSV 92, Saldo en AIVS de   #
#                    la SSV 92                                                               #
##############################################################################################

DATABASE safre_viv
GLOBALS "RETG01.4gl" --Archivo que almacena las variables globales del modulo
GLOBALS
DEFINE g_pid            LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod    LIKE cat_proceso.proceso_cod  -- codigo del proceso
END GLOBALS

MAIN
   DEFINE p_usuario_cod       LIKE seg_usuario.usuario_cod, -- nombre del usuario        
          p_folio             LIKE glo_folio.folio          -- numero de folio

   -- se reciben los parametros del programa
   LET p_folio            = ARG_VAL(1)
   LET p_usuario_cod      = ARG_VAL(2)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".RETS131.log")
   
   -- Llamado a función que genera el archivo de salida
   CALL fn_archivo_salida(p_folio, p_usuario_cod)
END MAIN

-- Funcion que se encarga de crear el archivo de salida
FUNCTION fn_archivo_salida( p_folio, p_usuario_cod)
DEFINE p_folio                      LIKE glo_folio.folio,
       p_usuario_cod                LIKE seg_usuario.usuario_cod,      
       v_v_nom_archivo              STRING, -- nombre del archivo de salida
       v_v_ruta_nomarch             STRING, -- ruta y nombre del archivo de salida
       v_c_ruta_env_acr             LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
       v_ch_arch_disposicion        BASE.CHANNEL, -- manejador de apuntador hacia archivo
       -- registro de salida
       reg_detalle                  RECORD     
         nss                        CHAR(11),
         importe_movimiento_92      DECIMAL(16,6),
         importe_movimiento_97      DECIMAL(16,6),
         saldo_92                   DECIMAL(16,6),
         saldo_97                   DECIMAL(16,6)
       END RECORD,
       v_nss                  CHAR(11),
       v_s_detalle            STRING,
       v_id_derechohabiente   DECIMAL(9,0),
       v_subcuenta            SMALLINT,
       v_monto_acciones       DECIMAL(16,6),
       v_saldo_acciones       DECIMAL(16,6),
       v_saldo_pesos          DECIMAL(16,6),
       v_i_contador_registros INTEGER,
       v_sql                  STRING,
       v_resultado            SMALLINT

   INITIALIZE v_s_detalle TO NULL
  
   LET v_i_contador_registros = 0

   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio 
   INTO   v_c_ruta_env_acr
   FROM   seg_modulo
   WHERE  modulo_cod = 'ret'

   -- Genero el nombre del archivo

   LET v_v_nom_archivo = "SACI_movs_variacion_",TODAY USING "yyyymmdd",".dat"

   SELECT COUNT(*)
   INTO   v_i_contador_registros
   FROM   ret_preliquida
   WHERE  folio_liquida  = p_folio
   AND    movimiento     = 1671
   AND    monto_acciones > 1000
   
   -- si no hay aceptados, no se genera el archivo y se finaliza la ejecucion
   IF ( v_i_contador_registros IS NULL OR v_i_contador_registros < 1 ) THEN
      DISPLAY "____________________________________________________________"
      DISPLAY "   GENERACION DE ARCHIVO DE SALIDA DE CARGOS A CUENTA 1671  "
      DISPLAY ""
      DISPLAY "   A T E N C I O N: No se tienen registros Mayores a 1000 AIVS"
      DISPLAY "                    en el folio ", p_folio
      DISPLAY "                    No se generó archivo de salida."
      DISPLAY "\n____________________________________________________________"
      EXIT PROGRAM
   END IF

   LET v_v_ruta_nomarch       = v_c_ruta_env_acr CLIPPED || "/" || v_v_nom_archivo

   DISPLAY "   Archivo        : ", v_v_ruta_nomarch
   
   -- se crea el manejador de archivo y su copia
   LET v_ch_arch_disposicion = base.Channel.create()

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_disposicion.openFile(v_v_ruta_nomarch, "w" )

   LET v_sql = "\nDROP TABLE IF EXISTS tmp_det_arch_1671;",
               "\nCREATE TABLE tmp_det_arch_1671 (",
               "\n nss                   CHAR(11)     ,",
               "\n importe_movimiento_92 DECIMAL(16,6),",
               "\n importe_movimiento_97 DECIMAL(16,6),",
               "\n saldo_92              DECIMAL(16,6),",
               "\n saldo_97              DECIMAL(16,6) ",
               ") IN tmp_2_dbs;"
                         
   PREPARE sid_tablatemporal FROM v_sql
   EXECUTE sid_tablatemporal

   DECLARE cur_movimiento CURSOR FOR
   SELECT id_derechohabiente, subcuenta, monto_acciones
   FROM   ret_preliquida
   WHERE  folio_liquida  = p_folio
   AND    movimiento     = 1671
   AND    monto_acciones > 1000
   FOREACH cur_movimiento INTO v_id_derechohabiente, v_subcuenta, v_monto_acciones
   
      SELECT nss
      INTO v_nss
      FROM afi_derechohabiente
      WHERE id_derechohabiente = v_id_derechohabiente

      --Obtengo Saldo
      --Genero datos para obtener saldos
      PREPARE prp_saldo_dia FROM "EXECUTE FUNCTION fn_saldo_dia(?,?,?,TODAY)"
      EXECUTE prp_saldo_dia USING v_nss,v_id_derechohabiente,v_subcuenta
      INTO v_resultado,v_saldo_acciones,v_saldo_pesos

      -- Obtengo datos de la tabla creada
      SELECT *
      FROM tmp_det_arch_1671
      WHERE nss = v_nss
      IF SQLCA.sqlcode = NOTFOUND THEN
         --Inserta
         CASE v_subcuenta
            WHEN 4   --Vivienda 97
               INSERT INTO tmp_det_arch_1671 VALUES (v_nss,0,v_monto_acciones,0,v_saldo_acciones)
            WHEN 8   --Vivienda 92
               INSERT INTO tmp_det_arch_1671 VALUES (v_nss,v_monto_acciones,0,v_saldo_acciones,0)
         END CASE
      ELSE
         --Actualiza
         CASE v_subcuenta
            WHEN 4   --Vivienda 97
               UPDATE tmp_det_arch_1671
               SET importe_movimiento_97 = v_monto_acciones,
                   saldo_97              = v_saldo_acciones
               WHERE nss = v_nss
            WHEN 8   --Vivienda 92
               UPDATE tmp_det_arch_1671
               SET importe_movimiento_92 = v_monto_acciones,
                   saldo_92              = v_saldo_acciones
               WHERE nss = v_nss
         END CASE
      END IF
   END FOREACH
   FREE cur_movimiento

   LET v_s_detalle = "NSS|importe_movimiento_92|importe_movimiento_97|saldo_92|saldo_97"
   
   --se escribe el detalle en el archivo
   CALL v_ch_arch_disposicion.writeLine(v_s_detalle)

   DECLARE cur_movimiento_2 CURSOR FOR
   SELECT nss, importe_movimiento_92, importe_movimiento_97,
          saldo_92, saldo_97
   FROM   tmp_det_arch_1671
   ORDER BY nss
   FOREACH cur_movimiento_2 INTO reg_detalle.*
      LET v_s_detalle = reg_detalle.nss,"|",
                        reg_detalle.importe_movimiento_92,"|",
                        reg_detalle.importe_movimiento_97,"|",
                        reg_detalle.saldo_92,"|",
                        reg_detalle.saldo_97
      --se escribe el detalle en el archivo
      CALL v_ch_arch_disposicion.writeLine(v_s_detalle)
   END FOREACH
  
   -- se cierra el archivo
   CALL v_ch_arch_disposicion.CLOSE()
   
END FUNCTION --fn_archivo_salida