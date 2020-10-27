--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
####################################################################
#Modulo            =>RET                                           #
#Programa          =>RETS02                                        #
#Objetivo          =>Programa que ejecuta el proceso de generacion #
#                    de archivo de salida de retiros por           #
#                    transferencia                                 #
####################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl" --Archivo que almacena las variables globales del modulo
GLOBALS
DEFINE g_pid            LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod    LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod      LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
END GLOBALS

MAIN
   DEFINE p_usuario_cod          LIKE seg_usuario.usuario_cod, -- nombre del usuario          
          p_folio                LIKE glo_folio.folio, -- numero de folio          
          v_d_inicial, -- Fecha inicial del periodo para el archivo de salida 
          v_d_final              DATE-- Fecha final del periodo para el archivo de salida 
          

   -- se reciben los parametros del programa
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET g_nombre_archivo = ARG_VAL(6)
   
   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".RETS02.log")
   
   -- Llamado a función que genera el archivo de salida
   CALL fn_archivo_salida(v_d_inicial, v_d_final, p_folio, p_usuario_cod)
END MAIN

-- Funcion que se encarga de crear el archivo de salida
-- recibe como parametro la fecha inicial y final para tomar el rango de salida
FUNCTION fn_archivo_salida(v_d_inicial, v_d_final, p_folio, p_usuario_cod)
DEFINE p_folio        LIKE glo_folio.folio,
       p_usuario_cod  LIKE seg_usuario.usuario_cod,

       v_r_detalle_transferencia        RECORD LIKE ret_transferencia.*, -- registro de transferencia       
       v_v_nom_archivo                  STRING, -- nombre del archivo de salida
       v_v_ruta_nomarch                 STRING, -- ruta y nombre del archivo de salida
       v_c_ruta_env_acr                 LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
       v_ch_arch_solTransf              BASE.CHANNEL, -- manejador de apuntador hacia archivo
       v_d_inicial                      DATE, -- Fecha inicial del periodo para el archivo de salida 
       v_d_final                        DATE, -- Fecha final del periodo para el archivo de salida 
       cont_cza_solicitud               SMALLINT, -- Contador de encabezado de solicitudes
       v_s_registro                     STRING, -- registro a insertar
       v_bandera                        SMALLINT,
       p_titulo                         STRING, -- titulo del mensaje enviado en el correo
       p_mensaje                        STRING,  -- cuerpo del mensaje enviado
       -- campos para la interfaz de salida
       v_importe_texto               CHAR(12),       -- el importe total en texo (sin punto decimal)
       v_i_secuencia                 INTEGER
       ,v_division 	                 CHAR(2)
       ,v_filler 	                   CHAR(2)     
       ,v_concepto_sop 	             CHAR(2)
       ,v_refer_pago 	               CHAR(12)
       ,v_filler1 	                 DECIMAL(6,2)
       ,v_imp_glob_pago 	           DECIMAL(22,2)
       ,v_f_pago                     CHAR(10)
       ,v_refer_pago_idem            DECIMAL(12,0)
       ,v_fller2                     DECIMAL(14,0) 
       ,v_s_sql                          STRING    

   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio 
     INTO v_c_ruta_env_acr
     FROM seg_modulo
    WHERE modulo_cod = 'ret'

   --se obtienela secuencia del archivo  
   SELECT seq_archivo_tesoreria.NEXTVAL
   INTO   v_i_secuencia
   FROM   systables
   WHERE  tabid = 1


   -- se envia la cadena que indica el fin de la etapa
   CALL fn_display_proceso(0, "INFORME PARA TESORERIA")

   --modificación realizada el día 21 de abril de 2012 por Rubén Haro Castro
   --se modifica el nombre del archivo que comprende el año-mes-dia-consecutivo.disposicion
   -- se crea el nombre del archivo y posteriormente se concatena con la ruta   
   --LET v_v_nom_archivo = "/transf_sol_liq_teso_" || p_folio || ".ret"
   LET v_v_nom_archivo = "/" ,YEAR(TODAY) USING "&&&&",MONTH(TODAY) USING "&&",DAY(TODAY) using "&&",v_i_secuencia USING "&&&",".Transferencia"      

   --DISPLAY " v_v_nom_archivo =  ",v_v_nom_archivo
   --DISPLAY " v_v_ruta_nomarch =  ",v_v_ruta_nomarch
   LET v_v_ruta_nomarch = v_c_ruta_env_acr CLIPPED || v_v_nom_archivo
   -- se crea el manejador de archivo
   LET v_ch_arch_solTransf = base.Channel.create()
  
   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_solTransf.openFile(v_v_ruta_nomarch, "w" )
   CALL v_ch_arch_solTransf.setDelimiter("")
  
   -- Se ingresan los registros del encabezado de la solicitud
   LET cont_cza_solicitud = 1
{
estructura de la interfaz de salida del retiros por transferencia

Id	Nombre del Campo	Tipo	Ent.	Dec.	Posición			Contenido
1	Referencia        	X	7	00	001	-	007	Consecutivo de operación a tres dígitos y cuatro para el año (xxxAAAA)
2	Acreedor 	        9	10	00	008	-	017	Campo fijo "2000001570"
3	Importe 	        9	14	02	018	-	033	Importe global en pesos, a transferir del archivo (solicitudes aceptadas)
4	División 	        9	2	00	034	-	035	Campo fijo "09"
5	Fecha de pago 	    F	8	00	036	-	043	Fecha de liquidación del archivo, formato "DDMMAAAA"
6	Concepto SOP 	    9	2	00	044	-	045	Campo fijo "23"}

   LET v_division     = "09"
   LET v_filler       = "  "
   LET v_concepto_sop = "0J"
   LET v_refer_pago   = v_i_secuencia USING "&&&"||YEAR(TODAY)
   LET v_filler1      = "        "      
   LET v_f_pago       = TODAY USING "dd/mm/yyyy"
   LET v_fller2       = "              "
   
   -- se obtiene el monto total en pesos de las solicitudes liquidadas
   SELECT SUM(monto_pesos)
     --INTO v_Importe
     INTO  v_imp_glob_pago
     FROM  cta_movimiento
    WHERE  movimiento = 222 -- cargo por retiro por transferencia
      AND  folio_liquida = p_folio
     
   LET v_refer_pago_idem = v_refer_pago 
   -- se cambia a positivo el importe
   LET v_imp_glob_pago         = v_imp_glob_pago  * (-1)
   LET v_importe_texto   = v_imp_glob_pago USING "&&&&&&&&&.&&"
                     
   LET v_s_registro =  v_division 
                      ,v_filler
                      ,v_concepto_sop
                      ,v_refer_pago
                      ,v_filler1
                      ,v_importe_texto
                      ,v_f_pago
                      ,v_refer_pago_idem USING "&&&&&&&&&&&&"
                      ,v_fller2 
   
   -- se escribe el registro                   
   CALL v_ch_arch_solTransf.write([v_s_registro])
                               

   -- se leen los datos de detalle que fueron preliquidados y liquidados
   DECLARE cur_salidadetalletrans CURSOR FOR
   SELECT *
   FROM
      ret_transferencia
   WHERE
         folio = p_folio
     AND estado_solicitud = 60 -- liquidada
   
   -- se escribe cada registro de detalle
   FOREACH cur_salidadetalletrans INTO v_r_detalle_transferencia.*    
   
      -- se cambia el estado de la solicitud a notificada
      UPDATE ret_transferencia
      SET
         estado_solicitud = 70 -- enviada a tesoreria
      WHERE
            id_derechohabiente = v_r_detalle_transferencia.id_derechohabiente
        AND folio              = v_r_detalle_transferencia.folio
             
   END FOREACH
   
   -- se actualizan las solicitudes que no tuvieron saldo suficiente
   UPDATE ret_transferencia
   SET
      estado_solicitud = 101 -- notificada a tesoreria por saldo insuficiente
   WHERE
         folio            = p_folio
     AND estado_solicitud = 100
     AND cod_rechazo      = 767

   -- se cierra el archivo
   CALL v_ch_arch_solTransf.close()
   
   -- se envia la cadena que indica el fin de la etapa
   CALL fn_display_proceso(1, "INFORME PARA TESORERIA")

   CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
                     RETURNING v_bandera

   LET v_s_sql = "\n UPDATE glo_folio",
                 "\n SET status = 3",         
                 "\n WHERE proceso_cod = ",g_proceso_cod, -- retiros por disposicion de recursos
                 "\n AND status = 2 ", --tesoreria
                 "\n AND folio  = ",p_folio

    PREPARE pr_folio  FROM v_s_sql
    EXECUTE pr_folio

   -- se complementa el mensaje
   LET p_mensaje = "Informe de solicitudes de retiro liquidadas para Tesorería realizado."

   -- se crea el titulo del mensaje
   LET p_titulo = "Finalización de operación - RETIROS TRANSFERENCIA - REPORTE PARA TESORERIA"
               
   -- se invoca el envio de correo electronico de notificacion
   CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                          NULL, -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)

   
END FUNCTION --fn_archivo_salida