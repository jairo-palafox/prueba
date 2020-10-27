--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
####################################################################
#Modulo            =>RET                                           #
#Programa          =>RETS74                                        #
#Objetivo          =>Programa que ejecuta el proceso de generacion #
#                    de archivo de salida de retiros por fortale-  #
#                    cimiento al credito para la Tesoreria         #
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
          p_d_pid                LIKE bat_ctr_proceso.pid, -- pid
          p_tipo_proceso         SMALLINT, -- Tipo de proceso
          p_nom_prog             VARCHAR(30), --Nombre del programa
          p_folio                LIKE glo_folio.folio, -- numero de folio
          p_nombre_archivo       LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
          p_v_arch_proceso       VARCHAR(100), -- nombre del archivo
          v_d_inicial            DATE, -- Fecha inicial del periodo para el archivo de salida 
          v_d_final              DATE, -- Fecha final del periodo para el archivo de salida 
          v_i_sol_patronales     SMALLINT, -- Contador de numero de solicitudes por periodo
          v_i_reg_trabajadores   SMALLINT, -- Contador de numero de registro de trabajadores
          v_i_tot_registros      SMALLINT, -- Contador de numero de registro de registros
          v_d_tot_vivienda       DECIMAL(16,2), -- Contador de numero de vivienda
          v_d_tot_avis           DECIMAL(16,2) -- Contador de numero de avis

   -- se reciben los parametros del programa
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET g_nombre_archivo = ARG_VAL(6)
   
   -- se crear el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".RETS74.log")
   
   -- Llamado a función que genera el archivo de salida
   CALL fn_archivo_salida(v_d_inicial, v_d_final, p_folio, p_usuario_cod)
END MAIN

-- Funcion que se encarga de crear el archivo de salida
-- recibe como parametro la fecha inicial y final para tomar el rango de salida
FUNCTION fn_archivo_salida(v_d_inicial, v_d_final, p_folio, p_usuario_cod)
DEFINE p_folio                       LIKE glo_folio.folio,
       p_usuario_cod                 LIKE seg_usuario.usuario_cod,
       v_id_derechohabiente          LIKE afi_derechohabiente.id_derechohabiente,
       v_r_detalle_tipo_n            RECORD LIKE ret_tipo_n.*, -- registro de retiro tipo N
       v_c_nss_trabajador            CHAR(11),   
       v_v_nom_archivo               STRING, -- nombre del archivo de salida
       v_v_ruta_nomarch              STRING, -- ruta y nombre del archivo de salida
       v_c_ruta_env_acr              LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
       v_ch_arch_solTransf           BASE.CHANNEL, -- manejador de apuntador hacia archivo
       v_d_inicial                   DATE, -- Fecha inicial del periodo para el archivo de salida 
       v_d_final                     DATE, -- Fecha final del periodo para el archivo de salida 
       v_cont_cza_solicitud          INTEGER, -- Contador de encabezado de solicitudes
       v_s_registro                  STRING, -- registro a insertar
       v_bandera                     SMALLINT,
       p_titulo                      STRING, -- titulo del mensaje enviado en el correo
       p_mensaje                     STRING,  -- cuerpo del mensaje enviado
       v_s_qry                       STRING, -- cadena con una instruccion SQL
       v_i_secuencia                 INTEGER,
       v_monto_pesos                 DECIMAL(12,2), -- monto en pesos
       -- campos del encabezado del archivo
       tmp_ret_cza_fc_tpo_registro   CHAR(2),
       tmp_ret_cza_fc_id_servicio    CHAR(2),
       tmp_ret_cza_fc_id_operacion   CHAR(2),
       tmp_ret_cza_fc_res_operacion  CHAR(1),
       tmp_ret_cza_fc_motivo_rch     CHAR(3),
       
       -- campos de detalle del archivo
       tmp_ret_det_fc_tpo_registro  CHAR(2) ,
       tmp_ret_det_fc_id_servicio   CHAR(2) ,
       tmp_ret_det_fc_id_operacion  CHAR(2) ,
       tmp_ret_det_fc_nss           CHAR(11),
       tmp_ret_det_fc_monto_pesos   CHAR(10),
       tmp_ret_det_fc_res_operacion CHAR(1),
       tmp_ret_det_fc_mtivo_rch     CHAR(3),
       
       -- campos del sumario del archivo
       tmp_ret_sum_fc_tpo_registro    CHAR(2) ,
       tmp_ret_sum_fc_id_servicio     CHAR(2) ,
       tmp_ret_sum_fc_id_operacion    CHAR(2) ,
       tmp_ret_sum_fc_total_registros CHAR(6)



   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio 
     INTO v_c_ruta_env_acr
     FROM seg_modulo
    WHERE modulo_cod = 'ret'

   --se obtienela secuencia del archivo  
   SELECT seq_archivo_tesoreria.NEXTVAL
     INTO v_i_secuencia
     FROM systables
    WHERE tabid = 1
     
   -- se envia la cadena que indica el fin de la etapa
   CALL fn_display_proceso(0, "INFORME PARA TESORERÍA")
   
   
   --modificación realizada el día 21 de abril de 2012 por Rubén Haro Castro
   --se modifica el nombre del archivo que comprende el año-mes-dia-consecutivo.disposicion
   -- se crea el nombre del archivo y posteriormente se concatena con la ruta   
   --LET v_v_nom_archivo = "/tipoN_sol_liq_teso_" || p_folio || ".ret"
   LET v_v_nom_archivo = "/" ,YEAR(TODAY) USING "&&&&",MONTH(TODAY) USING "&&",DAY(TODAY) USING "&&",v_i_secuencia USING "&&&",".retfc"      
   
   LET v_v_ruta_nomarch = v_c_ruta_env_acr CLIPPED || v_v_nom_archivo
   -- se crea el manejador de archivo
   LET v_ch_arch_solTransf = base.Channel.create()
   
   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_solTransf.openFile(v_v_ruta_nomarch, "w" )
   CALL v_ch_arch_solTransf.setDelimiter("")
   
   -- Se ingresan los registros del encabezado de la solicitud
   LET v_cont_cza_solicitud = 0

   -- se asignan los datos al archivo encabezado del archivo de salida
   LET tmp_ret_cza_fc_tpo_registro  = "01"
   LET tmp_ret_cza_fc_id_servicio   = "04"
   LET tmp_ret_cza_fc_id_operacion  = "11"
   LET tmp_ret_cza_fc_res_operacion = "1"
   LET tmp_ret_cza_fc_motivo_rch    = "000" -- no se rechaza
  
   -- se escribe el encabezado
   CALL v_ch_arch_solTransf.write([
        tmp_ret_cza_fc_tpo_registro ,
        tmp_ret_cza_fc_id_servicio  ,
        tmp_ret_cza_fc_id_operacion ,
        tmp_ret_cza_fc_res_operacion,
        tmp_ret_cza_fc_motivo_rch   ])

   -- se leen los datos de detalle
   DECLARE cur_salidaretfc CURSOR FOR
   SELECT 
    a.nss,
    a.id_derechohabiente,
    b.importe_viv
     FROM ret_fortalecimiento_credito b,
          afi_derechohabiente a
    WHERE b.folio = p_folio
    AND   a.id_derechohabiente = b.id_derechohabiente
--      AND estado_solicitud = 60 -- liquidada
   
   -- se escribe cada registro de detalle
   FOREACH cur_salidaretfc INTO tmp_ret_det_fc_nss, v_id_derechohabiente, v_monto_pesos
      -- se asignan los datos comunes de los registros de detalle
      LET tmp_ret_det_fc_tpo_registro  = "03"
      LET tmp_ret_det_fc_id_servicio   = "04"
      LET tmp_ret_det_fc_id_operacion  = "11"
      LET tmp_ret_det_fc_res_operacion = "1"
      LET tmp_ret_det_fc_mtivo_rch     = "000"
      -- se quitan los 2 decimales al monto en pesos
      LET tmp_ret_det_fc_monto_pesos   = (v_monto_pesos * 100) USING "&&&&&&&&&&"
      -- se cuenta un registro de detalle
      LET v_cont_cza_solicitud = v_cont_cza_solicitud + 1

      -- se cambia el estado de la solicitud a notificada
      UPDATE ret_fortalecimiento_credito
      SET    estado_solicitud = 70 -- enviada a tesoreria
      WHERE  id_derechohabiente = v_id_derechohabiente
        AND  folio              = p_folio
        
      -- se escribe el registro en archivo
      CALL v_ch_arch_solTransf.write([
              tmp_ret_det_fc_tpo_registro  ,
              tmp_ret_det_fc_id_servicio   ,
              tmp_ret_det_fc_id_operacion  ,
              tmp_ret_det_fc_nss           ,
              tmp_ret_det_fc_monto_pesos   ,
              tmp_ret_det_fc_res_operacion ,
              tmp_ret_det_fc_mtivo_rch     ])
   END FOREACH


   -- se escribe el sumario del archivo
   LET tmp_ret_sum_fc_tpo_registro    = "09"
   LET tmp_ret_sum_fc_id_servicio     = "04"
   LET tmp_ret_sum_fc_id_operacion    = "11"
   LET tmp_ret_sum_fc_total_registros = v_cont_cza_solicitud USING "&&&&&&"


   CALL v_ch_arch_solTransf.write([
           tmp_ret_sum_fc_tpo_registro    ,
           tmp_ret_sum_fc_id_servicio     ,
           tmp_ret_sum_fc_id_operacion    ,
           tmp_ret_sum_fc_total_registros ])


   -- se cierra el archivo
   CALL v_ch_arch_solTransf.close()
   
   
   -- se envia la cadena que indica el fin de la etapa
   CALL fn_display_proceso(1, "INFORME PARA TESORERÍA")

   CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
                     RETURNING v_bandera

   -- se complementa el mensaje
   LET p_mensaje = "Informe de solicitudes de retiro liquidadas para Tesorería realizado."
                        
   -- se crea el titulo del mensaje
   LET p_titulo = "Finalización de operación - RETIROS FORTALECIMIENTO AL CRÉTIDO - INFORME PARA TESORERÍA"
               
   -- se invoca el envio de correo electronico de notificacion
   CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                          NULL, -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)

   
END FUNCTION --fn_archivo_salida