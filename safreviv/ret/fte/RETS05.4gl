--=============================================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--=============================================================================
##############################################################################################
#MODULO            =>RET                                                                     #
#PROGRAMA          =>RETS05                                                                  #
#OBJETIVO          =>PROGRAMA QUE EJECUTA EL PROCESO DE GENERACION                           #
#                    DE ARCHIVO DE SALIDA DE RETIROS POR                                     #
#                    FONDO AHORRO                                                            #
##############################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"    --Archivo que almacena las variables globales del modulo
GLOBALS
DEFINE g_pid            LIKE bat_ctr_proceso.pid,           --  ID del proceso
       g_proceso_cod    LIKE cat_proceso.proceso_cod,       -- codigo del proceso
       g_opera_cod      LIKE cat_operacion.opera_cod,       -- codigo de operacion
       g_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
END GLOBALS

MAIN
   DEFINE p_usuario_cod          LIKE seg_usuario.usuario_cod, -- nombre del usuario
          p_folio                LIKE glo_folio.folio,         -- numero de folio
          v_d_inicial            DATE,                         -- Fecha inicial del periodo para el archivo de salida 
          v_d_final              DATE                          -- Fecha final del periodo para el archivo de salida 
          

   -- se reciben los parametros del programa
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET g_nombre_archivo = ARG_VAL(6)
   
   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".RETS05.log")
   
   -- Llamado a función que genera el archivo de salida
   CALL fn_archivo_salida(v_d_inicial, v_d_final, p_folio, p_usuario_cod)
END MAIN

-- Funcion que se encarga de crear el archivo de salida
-- recibe como parametro la fecha inicial y final para tomar el rango de salida
FUNCTION fn_archivo_salida(v_d_inicial, v_d_final, p_folio, p_usuario_cod)

DEFINE p_folio        LIKE glo_folio.folio,
       p_usuario_cod  LIKE seg_usuario.usuario_cod,
       v_id_solicitud LIKE ret_fondo_ahorro.id_solicitud, -- num de solicitud
       v_r_detalle_fondo_ahorro_recursos RECORD
         nss          LIKE afi_derechohabiente.nss, -- NSS del trabajador
         rfc          LIKE afi_derechohabiente.rfc, -- RFC del trabajador
         ap_paterno   LIKE afi_derechohabiente.ap_paterno_af, -- apellido paterno
         ap_materno   LIKE afi_derechohabiente.ap_materno_af, -- apellido materno
         nombre       LIKE afi_derechohabiente.nombre_af, -- nombre de pila
         clabe        CHAR(18), -- clabe interbancaria
         referencia   CHAR(18), -- referencia de pago
         monto        DECIMAL(22,2) -- importe por pagar
     END RECORD , -- registro de fondo ahorro
       v_v_nom_archivo                  STRING,        -- nombre del archivo de salida
       v_v_ruta_nomarch                 STRING,        -- ruta y nombre del archivo de salida
       v_c_ruta_env_acr                 LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
       v_ch_arch_fondoAhorro            BASE.CHANNEL,  -- manejador de apuntador hacia archivo
       v_d_inicial                      DATE,          -- Fecha inicial del periodo para el archivo de salida 
       v_d_final                        DATE,          -- Fecha final del periodo para el archivo de salida 
       cont_cza_solicitud               SMALLINT,      -- Contador de encabezado de solicitudes
       v_s_registro                     STRING,        -- registro a insertar
       v_bandera                        SMALLINT,
       p_titulo                         STRING,        -- titulo del mensaje enviado en el correo
       p_mensaje                        STRING,        -- cuerpo del mensaje enviado
       
       v_Referencia                     CHAR(7)      , -- X    7     00     001 - 007 Consecutivo de operación a tres dígitos y cuatro para el año (xxxAAAA)
       v_Acreedor                       CHAR(10)     , -- 9    10    00     008 - 017 Campo fijo "2000001570"
       v_Importe                        DECIMAL(16,2), -- 9    14    02     018 - 033 Importe global en pesos, a transferir del archivo (solicitudes aceptadas)
       v_División                       CHAR(2)      , -- 9    2     00     034 - 035 Campo fijo "09"
       v_Fecha_de_pago                  CHAR(8)      , -- F    8     00     036 - 043 Fecha de liquidación del archivo, formato "DDMMAAAA"
       v_Concepto_SOP                   CHAR(2)      , -- 9    2     00     044 - 045 Campo fijo "23"
       v_importe_texto                  CHAR(16)     ,  -- el importe total en texo (sin punto decimal)
       v_i_secuencia                    SMALLINT,
       v_comando                        STRING, -- cadena para ejecutar un comando
       v_sql               STRING

   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio 
   INTO   v_c_ruta_env_acr
   FROM   seg_modulo
   WHERE  modulo_cod = 'ret'

   DISPLAY "Generando archivo de salida de solicitudes liquidadas de Retiro de Fondo de Ahorro..."

   -- se actualiza el folio a enviado a tesoreria
   LET v_sql = "UPDATE glo_folio SET status = 3 WHERE folio = ", p_folio
   EXECUTE IMMEDIATE v_sql

   -- se envia la cadena que indica el fin de la etapa
   CALL fn_display_proceso(0, "INFORME PARA TESORERIA")

   SELECT seq_archivo_tesoreria.NEXTVAL
   INTO   v_i_secuencia
   FROM   systables
   WHERE  tabid = 1

   --se modifica el nombre del archivo que comprende el año-mes-dia-consecutivo.fondoAhorro
   --LET v_v_nom_archivo = "/" ||"solicitudes_liq_tesoreria_" || p_folio || ".ret"}
   LET v_v_nom_archivo = "/", TODAY USING "yyyymmdd" , ".rfa"
   LET v_v_ruta_nomarch = v_c_ruta_env_acr CLIPPED || v_v_nom_archivo
   
   -- nombre de archivo generado
   DISPLAY "Archivo de solicitud de pago FICO: ", v_v_ruta_nomarch

   -- se crea el manejador de archivo
   LET v_ch_arch_fondoAhorro = base.Channel.create()
   
   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_fondoAhorro.openFile(v_v_ruta_nomarch, "w" )
   CALL v_ch_arch_fondoAhorro.setDelimiter("|")
   
   -- Se ingresan los registros del encabezado de la solicitud
   LET cont_cza_solicitud = 1

   -- se leen las solicitudes liquidadas
   DECLARE cur_solpagfa CURSOR FOR
   SELECT b.id_solicitud   ,
          a.nss            ,
          a.rfc            ,
          "AP PATERNO"     ,
          "AP MATERNO"     ,
          "NOMBRE"         ,
          "CLABE"          ,
          "REFBANCO"       ,
          NVL(b.saldo_viv72,0) + NVL(b.tanto_adicional,0) -- importe pagado
       FROM ret_solicitud_generico a,
            ret_fondo_ahorro b
       WHERE b.folio            = p_folio
       AND   b.estado_solicitud = 60 -- liquidadas
       AND   a.folio            = b.folio
       AND   a.id_solicitud     = b.id_solicitud
         
   -- se cambian de estatus las solicitudes liquidadas a informadas
   FOREACH cur_solpagfa INTO v_id_solicitud, v_r_detalle_fondo_ahorro_recursos.*    
   
      -- se cambia el estado de la solicitud a notificada
      UPDATE ret_fondo_ahorro
      SET    estado_solicitud = 70 -- enviada a tesoreria
      WHERE  folio            = p_folio
      AND    id_solicitud     = v_id_solicitud
          
      -- se escribe el registro en archivo   
      CALL v_ch_arch_fondoAhorro.write([v_r_detalle_fondo_ahorro_recursos.*])
             
   END FOREACH
   
   -- se cierra el archivo
   CALL v_ch_arch_fondoAhorro.close()

   -- se crea el SHA1 del archivo
   LET v_comando = "sha1sum ", v_v_ruta_nomarch
   DISPLAY "Generando hash del archivo..."
   DISPLAY v_comando
   RUN v_comando 
   
   -- se envia la cadena que indica el fin de la etapa
   CALL fn_display_proceso(1, "INFORME PARA TESORERIA")

   -- se finaliza la operacion
   CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
                     RETURNING v_bandera

   -- se complementa el mensaje
   LET p_mensaje = "Informe de solicitudes de retiro liquidadas para Tesorería realizado."
                        
   -- se crea el titulo del mensaje
   LET p_titulo = "Finalización de operación - RETIROS FONDO AHORRO - REPORTE PARA TESORERIA"
               
   -- se invoca el envio de correo electronico de notificacion
   CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                          NULL, -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)

END FUNCTION 