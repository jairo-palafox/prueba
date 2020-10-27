--=============================================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--=============================================================================
##############################################################################################
#MODULO            =>RET                                                                     #
#PROGRAMA          =>RETS04                                                                  #
#OBJETIVO          =>PROGRAMA QUE EJECUTA EL PROCESO DE GENERACION                           #
#                    DE ARCHIVO DE SALIDA DE RETIROS POR                                     #
#                    SOLO INFONAVIT                                                          #
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
   DEFINE p_usuario_cod          LIKE seg_usuario.usuario_cod,        -- nombre del usuario
          p_d_pid                LIKE bat_ctr_proceso.pid,            -- pid
          p_tipo_proceso         SMALLINT,                            -- Tipo de proceso
          p_nom_prog             VARCHAR(30),                         --Nombre del programa
          p_folio                LIKE glo_folio.folio,                -- numero de folio
          p_nombre_archivo       LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
          p_v_arch_proceso       VARCHAR(100),                        -- nombre del archivo
          v_d_inicial,                                                -- Fecha inicial del periodo para el archivo de salida 
          v_d_final              DATE,                                -- Fecha final del periodo para el archivo de salida 
          v_i_sol_patronales     SMALLINT,                            -- Contador de numero de solicitudes por periodo
          v_i_reg_trabajadores   SMALLINT,                            -- Contador de numero de registro de trabajadores
          v_i_tot_registros      SMALLINT,                            -- Contador de numero de registro de registros
          v_d_tot_vivienda       DECIMAL(16,2),                       -- Contador de numero de vivienda
          v_d_tot_avis           DECIMAL(16,2)                        -- Contador de numero de avis

   -- se reciben los parametros del programa
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET g_nombre_archivo = ARG_VAL(6)
   
   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".RETS04.log")
   
   -- Llamado a función que genera el archivo de salida
   CALL fn_archivo_salida(v_d_inicial, v_d_final, p_folio, p_usuario_cod)
END MAIN

-- Funcion que se encarga de crear el archivo de salida
-- recibe como parametro la fecha inicial y final para tomar el rango de salida
FUNCTION fn_archivo_salida(v_d_inicial, v_d_final, p_folio, p_usuario_cod)

DEFINE p_folio        LIKE glo_folio.folio,
       p_usuario_cod  LIKE seg_usuario.usuario_cod,
       v_r_detalle_solo_info_recursos RECORD LIKE ret_solo_infonavit.*, -- registro de disposicion       
       v_v_nom_archivo                  STRING,        -- nombre del archivo de salida
       v_v_ruta_nomarch                 STRING,        -- ruta y nombre del archivo de salida
       v_c_ruta_env_acr                 LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
       v_ch_arch_soloInfonavit              BASE.CHANNEL,  -- manejador de apuntador hacia archivo
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
       v_i_secuencia                    SMALLINT 

    -- se obtienen la ruta envio del modulo
    SELECT ruta_envio 
      INTO v_c_ruta_env_acr
      FROM seg_modulo
     WHERE modulo_cod = 'ret'

     --DISPLAY g_pid," ",g_proceso_cod," ",g_opera_cod," ",p_folio," ","RETS04"," ",
                              --g_nombre_archivo," ",p_usuario_cod
   -- se inicia la operacion
   IF (fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,p_folio,"RETS04",
                              g_nombre_archivo,p_usuario_cod) = 0) THEN

      -- se envia la cadena que indica el fin de la etapa
      CALL fn_display_proceso(0, "INFORME PARA TESORERIA")
      
     SELECT seq_archivo_tesoreria.NEXTVAL
       INTO v_i_secuencia
       FROM systables
      WHERE tabname = "seq_archivo_tesoreria"
        
      --PREPARE prp_secuenca_liquidacion FROM "seq_archivo_tesoreria.NEXTVAL"
      --EXECUTE prp_secuenca_liquidacion INTO v_i_secuencia
     
      -- se crea el nombre del archivo y posteriormente se concatena con la ruta   
      --se modifica el nombre del archivo que comprende el año-mes-dia-consecutivo.soloInfonavit
      --LET v_v_nom_archivo = "/" ||"solicitudes_liq_tesoreria_" || p_folio || ".ret"}
      LET v_v_nom_archivo = "/",YEAR(TODAY) USING "&&&&",MONTH(TODAY) USING "&&",DAY(TODAY) USING "&&",v_i_secuencia USING "&&&" ,".soloInfonavit"
       --DISPLAY  v_v_nom_archivo
      LET v_v_ruta_nomarch = v_c_ruta_env_acr CLIPPED || v_v_nom_archivo
      -- se crea el manejador de archivo
      LET v_ch_arch_soloInfonavit = base.Channel.create()
      
      -- se crea archivo y se indica que se escribira en el mismo
      CALL v_ch_arch_soloInfonavit.openFile(v_v_ruta_nomarch, "w" )
      CALL v_ch_arch_soloInfonavit.setDelimiter("")
   
      -- Se ingresan los registros del encabezado de la solicitud
      LET cont_cza_solicitud = 1

{

estructura del archivo de salida

Id	Nombre del Campo	Tipo	Ent.	Dec.	Posición			Contenido
1	Referencia 	        X       7       00       001 - 007 Consecutivo de operación a tres dígitos y cuatro para el año (xxxAAAA)
2	Acreedor 	        9       10      00       008 - 017 Campo fijo "2000001570"
3	Importe 	        9       14      02       018 - 033 Importe global en pesos, a transferir del archivo (solicitudes aceptadas)
4	División 	        9       2       00       034 - 035 Campo fijo "09"
5	Fecha de pago 	    F       8       00       036 - 043 Fecha de liquidación del archivo, formato "DDMMAAAA"
6	Concepto SOP 	    9       2       00       044 - 045 Campo fijo "21"

}

      -- se obtiene el monto total en pesos de las solicitudes liquidadas
      SELECT SUM(monto_pesos)
      INTO   v_Importe
      FROM   cta_movimiento
      WHERE  movimiento = 172
        AND  folio_liquida = p_folio
        
      LET v_Referencia      = "001" || YEAR(TODAY)
      LET v_Acreedor        = "2000001570"
      LET v_División        = "09"
      LET v_Fecha_de_pago   = TODAY USING "ddmmyyyy"
      LET v_Concepto_SOP    = "21"

      -- se cambia a positivo el importe
      LET v_Importe         = (v_Importe * 100) * (-1)
      LET v_importe_texto   = v_Importe USING "&&&&&&&&&&&&&&&&"

      LET v_s_registro =  v_Referencia
                         ,v_Acreedor
                         ,v_importe_texto
                         ,v_División
                         ,v_Fecha_de_pago
                         ,v_Concepto_SOP
      
      -- se escribe el registro
      CALL v_ch_arch_soloInfonavit.write([v_s_registro])


      -- se leen los datos de detalle que fueron preliquidados y liquidados
      DECLARE cur_salidadetallesoloInfonavit CURSOR FOR
      SELECT *
      FROM
         ret_solo_infonavit
      WHERE
            folio = p_folio
        AND estado_solicitud = 60 -- liquidada
      
      -- se escribe cada registro de detalle
      FOREACH cur_salidadetallesoloInfonavit INTO v_r_detalle_solo_info_recursos.*    
      
         -- se cambia el estado de la solicitud a notificada
         UPDATE ret_solo_infonavit
         SET
            estado_solicitud = 70 -- enviada a tesoreria
         WHERE
               id_derechohabiente = v_r_detalle_solo_info_recursos.id_derechohabiente
           AND folio              = v_r_detalle_solo_info_recursos.folio
                
      END FOREACH
      
      -- se actualizan las solicitudes que no tuvieron saldo suficiente
      UPDATE ret_solo_infonavit
      SET
         estado_solicitud = 101 -- notificada a tesoreria por saldo insuficiente
      WHERE
            folio            = p_folio
        AND estado_solicitud = 100
        AND cod_rechazo      = 10

      -- se cierra el archivo
      CALL v_ch_arch_soloInfonavit.close()
      
      -- se envia la cadena que indica el fin de la etapa
      CALL fn_display_proceso(1, "INFORME PARA TESORERIA")

      CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
                        RETURNING v_bandera

      -- se complementa el mensaje
      LET p_mensaje = "Informe de solicitudes de retiro liquidadas para Tesorería realizado."
                        
   ELSE
      -- se complementa el mensaje para el correo
      LET p_mensaje = "Ocurrió un error al generar el informe de solicitudes liquidadas para Tesorería."
   
      DISPLAY p_mensaje
      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
                          RETURNING v_bandera
   END IF

   -- se crea el titulo del mensaje
   LET p_titulo = "Finalización de operación - RETIROS SOLO INFONAVIT - REPORTE PARA TESORERIA"
               
   -- se invoca el envio de correo electronico de notificacion
   CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                          NULL, -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)

   --fn_archivo_salida
END FUNCTION 