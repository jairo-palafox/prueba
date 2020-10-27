--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETP426                                                                #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la preliquidacion #
#                de la carga de los cargos de SSV aplicados via SIAFF                   #
#Fecha inicio => Octubre 7, 2016                                                        #
#                                                                                       #
#                                                                                       #
#                                                                                       #
#########################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_pid                  LIKE bat_ctr_operacion.pid, -- PID del proceso
       p_proceso_cod          LIKE bat_ctr_operacion.proceso_cod, -- codigo del proceso
       p_opera_cod            LIKE bat_ctr_operacion.opera_cod, -- codigo de la operacion
       p_usuario_cod          LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_folio                LIKE deo_preliquida.folio_liquida,
       p_nombre_archivo       LIKE glo_ctr_archivo.nombre_archivo, 
       v_s_sql                STRING, -- cadena con una instruccion SQL
       v_i_resultado          INTEGER -- resultado del proceso
       ,r_bnd_fin_oper        SMALLINT
       ,v_si_correcto_integra SMALLINT
       ,p_titulo              STRING -- titulo del mensaje enviado en el correo
       ,p_mensaje             STRING -- cuerpo del mensaje enviado
       ,v_s_comando           STRING -- PARA EJECUTAR ARCHIVO DE RECHAZADOS
       ,p_programa_cod        VARCHAR(10)
       ,v_error_isam          INTEGER
       ,v_mensaje             VARCHAR(250)
       ,p_marca               SMALLINT 
       ,p_movimiento          SMALLINT -- movimiento de retiro por disposicion de recursos
       ,p_movimiento_sobregiro SMALLINT -- movimiento de sobregiro del retiro por disposicion de recursos
       ,p_max_aivs_sobregiro   DECIMAL(20,6) -- num maximo de aivs de diferencia entre saldo y monto solicitado para sobregiro
       ,v_id_solicitud_error    DECIMAL(9,0)

   
   -- se recuperan los parametros la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod          = ARG_VAL(1)
   LET p_pid                  = ARG_VAL(2)
   LET p_proceso_cod          = ARG_VAL(3)
   LET p_opera_cod            = ARG_VAL(4)
   LET p_folio                = ARG_VAL(5)
   LET p_nombre_archivo       = ARG_VAL(6)
   
   -- se asigna proceso y operacion
   LET g_pid         = p_pid      
   LET g_proceso_cod = p_proceso_cod -- retiros por disposicion de recursos
   LET g_opera_cod   = p_opera_cod -- preliquidacion
           
   -- se asume que el proceso termina correctamente
   LET v_i_resultado         = 0
   LET v_si_correcto_integra = 0

   -- se envia la cadena que indica el inicio de etapa
   CALL fn_display_proceso(0, "PRELIQUIDACION")

   DISPLAY "Realizando preliquidación de cargos de la SSV aplicados via SIAFF"
   
   -- se contruye el enuncionado SQL
   LET v_s_sql = "EXECUTE FUNCTION fn_ret_preliquida_ssv(?,?,?,?,?)"
   
   -- se prepara la ejecucion del stored procedure para la integracion
   PREPARE sid_integradeo FROM v_s_sql
   
   -- se ejecuta el stored procedure
   EXECUTE sid_integradeo USING p_folio, p_proceso_cod, p_opera_cod, p_usuario_cod, g_pid
      INTO v_i_resultado, v_error_isam, v_mensaje, v_id_solicitud_error

   DISPLAY "Estatus de preliquidación:",v_i_resultado

   
   -- si se termino correctamente 
   IF ( v_i_resultado = 0 )THEN
      DISPLAY "Preliquidación realizada con éxito"
      DISPLAY "Ya se puede continuar con la Liquidación."
      DISPLAY "Hace el llamado para exportar los escenarios 1.6"
      CALL fn_exporta_archivo(p_folio) RETURNING v_i_resultado
      IF v_i_resultado = 10 THEN 
         CALL fn_mensaje("Atención", "No se encontraron registros para exportar", "information")
      END IF 
      -- se obtiene el codigo de programa
      SELECT programa_cod
      INTO   p_programa_cod
      FROM   cat_operacion
      WHERE  proceso_cod = p_proceso_cod
      AND    opera_cod   = p_opera_cod

      CALL fn_reporte_liquidacion(p_folio, "ret_preliquida",
                                  p_usuario_cod, p_pid,
                                  p_proceso_cod, p_opera_cod,
                                  p_programa_cod, FALSE)
      
      -- se complementa el mensaje
      LET p_mensaje = "Preliquidación realizada con éxito.\nYa se puede continuar con la Liquidación."
      
      CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
                        RETURNING r_bnd_fin_oper
   ELSE
      -- se despliega el error
      DISPLAY "Error SQL : ", v_i_resultado
      DISPLAY "Error ISAM: ", v_error_isam
      DISPLAY "Mensaje   : ", v_mensaje
      DISPLAY "ID_Solicitud ERROR: ", v_id_solicitud_error
      -- se indica en el mensaje que la preliquidacion no termino correctamente
      LET p_mensaje = "El proceso de Preliquidación ha finalizado pero con errores.\nNo se puede continuar con el proceso de Liquidación. ", v_mensaje
   
      DISPLAY "El proceso de Preliquidación ha finalizado pero con errores.\nNo se puede continuar con el proceso de Liquidación."
      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
                          RETURNING v_i_resultado
   END IF

   -- se crea el titulo del mensaje
   LET p_titulo = "Finalización de operación - CARGOS DE LA SSV APLICADOS VIA SIAFF - PRELIQUIDACION"
   
   -- se invoca el envio de correo electronico de notificacion
   CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                          NULL, -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)

   -- se envia la cadena que indica el fin de la etapa
   CALL fn_display_proceso(1, "PRELIQUIDACIÓN")


END MAIN
--Función que exporta datos a un archivo
FUNCTION fn_exporta_archivo(p_l_folio)
DEFINE p_l_folio          DECIMAL(9,0)

DEFINE p_r_detalle RECORD 
         nss              CHAR(11),
         importe          DECIMAL(18,2),
         fch_contable     DATE,
         id_solicitud     DECIMAL(9,0)
END RECORD 
DEFINE v_pesos_tesofe     DECIMAL(18,2)
DEFINE v_acciones_viv97   DECIMAL(18,2)
DEFINE v_pesos_viv97      DECIMAL(18,2)
DEFINE v_acciones_viv92   DECIMAl(18,2)
DEFINE v_pesos_viv92      DECIMAl(18,2)
DEFINE v_abono_pesos      DECIMAL(18,2)
DEFINE v_abono_acciones   DECIMAL(18,2)
DEFINE v_num_credito      DECIMAL(10,0)
DEFINE v_cargo_cred_aivs  DECIMAL(12,2)
DEFINE v_cargo_cred_pesos DECIMAL(12,2)
DEFINE v_tpo_credito      SMALLINT 
DEFINE v_fecha_cargo_cred DATE
DEFINE v_estatus_adelanto CHAR(13)
DEFINE v_f_liq_cred       DATE 

DEFINE p_r_grupo   DYNAMIC ARRAY OF RECORD -- registro de despliegue del agrupador
         nss              CHAR(11),
         rfc              CHAR(13),
         doc_fico         CHAR(10),
         ejercicio        CHAR(4),
         fch_contable     DATE,
         fch_liquida      DATE,
         desc_tipo_sol    CHAR(25),
         folio            DECIMAL(9,0),
         subcta_47        SMALLINT,
         pesos_tesofe     DECIMAL(18,2),
         subcta_4         SMALLINT,
         acciones_viv97   DECIMAL(18,6),
         subcta_8         SMALLINT,
         acciones_viv92   DECIMAL(18,6),
         estado_solicitud SMALLINT
      END RECORD
DEFINE p_query             STRING 
DEFINE v_s_sql             STRING 
DEFINE p_detalle_exp      RECORD
         nss              CHAR(11),
         rfc              CHAR(13),
         doc_fico         CHAR(10),
         ejercicio        CHAR(4),
         fch_contable     DATE,
         fch_liquida      DATE,
         desc_tipo_sol    CHAR(25),
         folio            DECIMAL(9,0),
         subcta_47        SMALLINT,
         pesos_tesofe     DECIMAL(18,2),
         subcta_4         SMALLINT,
         acciones_viv97   DECIMAL(18,6),
         subcta_8         SMALLINT,
         acciones_viv92   DECIMAL(18,6),
         estado_solicitud SMALLINT      END RECORD
   DEFINE p_exporta_opcion       CHAR(15)
   DEFINE
      v_c_ruta_env            LIKE seg_modulo.ruta_envio,
      v_extension_txt         STRING,
      v_nom_archivo           STRING,
      v_archivo_txt           STRING,
      v_v_ruta_nomarch        STRING,
      v_v_ruta_nomarch2       STRING,
      v_mensaje_archivo       STRING,
      v_s_detalle             STRING,
      v_ch_arch_ret_generico  BASE.CHANNEL,  -- manejador de apuntador hacia archivo
      v_cuenta                INTEGER,
      v_solicitud             DECIMAL(9,0),
      v_tipo                  CHAR(1),
      v_hora                  CHAR(8),
      v_regresa               SMALLINT,
      v_contador              INTEGER,
      v_query                 STRING,
      v_nss_paso              CHAR(11),
      v_suma_aivs_97          DECIMAL(18,6),
      v_i                     INTEGER,
      v_regs_a_procesar       INTEGER,
      v_error                 SMALLINT 

   LET v_regresa = FALSE
   LET v_i       = 0
   LET p_r_detalle.nss = NULL
   LET p_r_detalle.fch_contable = NULL
   LET p_r_detalle.importe = NULL
   LET p_r_detalle.id_solicitud = NULL
   LET v_pesos_tesofe = NULL
   LET v_pesos_viv97 = NULL
   LET v_acciones_viv97 = NULL
   LET v_pesos_viv92 = NULL
   LET v_acciones_viv92 = NULL
   LET v_abono_pesos = NULL
   LET v_abono_acciones = NULL
   LET v_num_credito = NULL
   LET v_cargo_cred_aivs = NULL
   LET v_cargo_cred_pesos = NULL
   LET v_tpo_credito = NULL
   LET v_fecha_cargo_cred = NULL
   LET v_estatus_adelanto = NULL
   LET v_f_liq_cred = NULL   
   LET v_regs_a_procesar = 0


   DISPLAY "Se valida que existan registros para exportar"
   SELECT COUNT(*) 
   INTO   v_regs_a_procesar
   FROM   ret_cargos_ssv_siaff a 
   WHERE  a.folio =  p_l_folio
   AND    a.tipo_sol = '1.6'
   IF v_regs_a_procesar = 0 THEN 
      CALL fn_mensaje("Atención", "No existen registros para exportar", "information")
      RETURN 10
   END IF 


   -- se obtiene la ruta de envio y ejecutable
   SELECT ruta_envio
   INTO   v_c_ruta_env 
   FROM   seg_modulo
   WHERE  modulo_cod = "ret"

--   DISPLAY "el folio a procesar es >", p_folio, "<"
   
   -- las extensiones del archivo son csv para el detalle
   LET v_extension_txt = ".csv"
   LET v_hora = CURRENT HOUR TO SECOND

   -- Se genera el nombre del archivo de Rechazos
   LET v_nom_archivo = "Detalle_Cargos_SSV_Creditos", TODAY USING "yyyymmdd"
   LET v_archivo_txt = v_nom_archivo CLIPPED, v_extension_txt

   -- El archivo con ruta destino que contiene el detalle 
   LET v_v_ruta_nomarch = v_c_ruta_env CLIPPED , "/", v_archivo_txt

   -- Se muestra mensaje
   LET v_mensaje_archivo = "Se generará el archivo:\n\n\t", v_v_ruta_nomarch
   
   CALL fn_mensaje("Atención", v_mensaje_archivo, "information")
   -- nombre de archivo generado

   -- Se crea el manejador de archivo
   LET v_ch_arch_ret_generico = base.Channel.create()
   CALL v_ch_arch_ret_generico.setDelimiter(NULL)

   -- Se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_ret_generico.openFile(v_v_ruta_nomarch, "w" )
   -- Escribe el encabezado del archivo de Rechazados


   
   LET v_s_detalle = "NSS|IMPORTE FICO|FECHA PAGO|IMPORTE TESOFE|IMP SSV 97 APLIC PESOS|",
                     "IMP SSV 97 APLIC AIVS|IMP SSV 92 APLIC PESOS|IMP SSV 92 APLIC AIVS|",
                     "MOVTO ABONO DIF PESOS|MOVTO ABONO DIF AIVS|NUMERO CREDITO|IMP CARGO CREDITO AIVS|",
                     "IMP CARGO CREDITO PESOS|TIPO CREDITO|FECHA CARGO CREDITO|ESTATUS ADELANTO|",
                     "FECHA LIQ CREDITO|"

   CALL v_ch_arch_ret_generico.write(v_s_detalle)

   LET p_query = "   SELECT a.nss, a.importe, a.fch_contable, a.id_solicitud ", 
                 "\n FROM   ret_cargos_ssv_siaff a ",
                 "\n WHERE  a.folio = ", p_l_folio,
                 "\n AND    a.tipo_sol = '1.6'"

   DISPLAY "La consulta es >", p_query, "<"
   -- se llena el arreglo 
   PREPARE s_rch_query FROM p_query
   DECLARE cur_rch_query CURSOR FOR s_rch_query

   FOREACH cur_rch_query INTO p_r_detalle.*
      -- Busca los mivimientos de la subcuenta 47 TESOFE
      SELECT sum(monto_pesos)  --- movmientos tesofe
      INTO   v_pesos_tesofe
      FROM   ret_preliquida
      WHERE  folio_liquida = p_l_folio
      AND    id_referencia = p_r_detalle.id_solicitud
      AND    subcuenta     = 47
      AND    movimiento IN (1842,1862,1882,1902,1922,1942,1802,
                            1832,1852,1872,1892,1912,1932,1952)
      SELECT sum(monto_acciones), sum(monto_pesos)  --- movmientos viv97
      INTO   v_acciones_viv97, v_pesos_viv97
      FROM   ret_preliquida
      WHERE  folio_liquida = p_l_folio
      AND    id_referencia = p_r_detalle.id_solicitud
      AND    subcuenta     = 4
      AND    movimiento IN (1842,1862,1882,1902,1922,1942,1802,
                            1832,1852,1872,1892,1912,1932,1952)

      SELECT sum(monto_acciones), sum(monto_pesos)  --- movmientos viv92
      INTO   v_acciones_viv92, v_pesos_viv92
      FROM   ret_preliquida
      WHERE  folio_liquida = p_l_folio
      AND    id_referencia = p_r_detalle.id_solicitud
      AND    subcuenta     = 8
      AND    movimiento IN (1842,1862,1882,1902,1922,1942,1802,
                            1832,1852,1872,1892,1912,1932,1952)

      SELECT sum(monto_acciones), sum(monto_pesos)  --- movmientos abono por diferencia viv97
      INTO   v_abono_acciones, v_abono_pesos
      FROM   ret_preliquida
      WHERE  folio_liquida = p_l_folio
      AND    id_referencia = p_r_detalle.id_solicitud
      AND    subcuenta     = 4
      AND    movimiento IN (1701,1691,1711,1721,1731,1741,1751)  

      LET v_s_sql = "EXECUTE FUNCTION fn_cred_iniciativa(?)"
      
      -- se prepara la ejecucion del stored procedure para la integracion
      PREPARE sid_credito FROM v_s_sql
      
      -- se ejecuta el stored procedure
      EXECUTE sid_credito USING p_r_detalle.nss
         INTO v_error, v_num_credito, v_cargo_cred_aivs, v_cargo_cred_pesos,
              v_tpo_credito, v_fecha_cargo_cred, v_estatus_adelanto, v_f_liq_cred
    
      LET v_s_detalle = p_r_detalle.nss USING "&&&&&&&&&&&","|",
                        p_r_detalle.importe USING "################&.&&","|",
                        p_r_detalle.fch_contable USING "dd-mm-yyyy","|",
                        v_pesos_tesofe USING "################&.&&","|",
                        v_pesos_viv97 USING "################&.&&","|",
                        v_acciones_viv97 USING "################&.&&","|",
                        v_pesos_viv92 USING "################&.&&","|",
                        v_acciones_viv92 USING "################&.&&","|",
                        v_abono_pesos USING "################&.&&","|",
                        v_abono_acciones USING "################&.&&" ,"|",
                        v_num_credito, "|",
                        v_cargo_cred_aivs USING "################&.&&","|",
                        v_cargo_cred_pesos USING "################&.&&","|",
                        v_tpo_credito, "|",
                        v_fecha_cargo_cred USING "dd-mm-yyyy","|",
                        v_estatus_adelanto,"|",
                        v_f_liq_cred USING "dd-mm-yyyy","|"
      CALL v_ch_arch_ret_generico.write(v_s_detalle)
      LET p_r_detalle.nss = NULL
      LET p_r_detalle.fch_contable = NULL
      LET p_r_detalle.importe = NULL
      LET p_r_detalle.id_solicitud = NULL
      LET v_pesos_tesofe = NULL
      LET v_pesos_viv97 = NULL
      LET v_acciones_viv97 = NULL
      LET v_pesos_viv92 = NULL
      LET v_acciones_viv92 = NULL
      LET v_abono_pesos = NULL
      LET v_abono_acciones = NULL
      LET v_num_credito = NULL
      LET v_cargo_cred_aivs = NULL
      LET v_cargo_cred_pesos = NULL
      LET v_tpo_credito = NULL
      LET v_fecha_cargo_cred = NULL
      LET v_estatus_adelanto = NULL
      LET v_f_liq_cred = NULL
      
   END FOREACH
 
                           
   -- Se cierra el archivo de Rechazados
   CALL v_ch_arch_ret_generico.close()

   LET v_mensaje_archivo = "El archivo fue generado exitosamente:\n\n\t", v_v_ruta_nomarch

   CALL fn_mensaje("Atención", v_mensaje_archivo, "information")

   RETURN v_regresa
   
END FUNCTION
