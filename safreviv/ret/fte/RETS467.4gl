--=============================================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--=============================================================================
##############################################################################################
#MODULO            =>RET                                                                     #
#PROGRAMA          =>RETS467                                                                 #
#OBJETIVO          =>PROGRAMA QUE EJECUTA EL PROCESO DE GENERACION DE ARCHIVO DE SALIDA      #
#                    PARA LA NOTIFICACIÓN DE PAGO DE GRUPOS 2, 3 Y 4                         #
##############################################################################################
IMPORT os
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
          p_folio                LIKE glo_folio.folio         -- numero de folio         

   -- se reciben los parametros del programa
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET g_nombre_archivo = ARG_VAL(6)
   
   -- se crea el archivo log
   DISPLAY "Parámetros recibidos :"
   DISPLAY "Usuario        : ", p_usuario_cod
   DISPLAY "Pid            : ", g_pid
   DISPLAY "Proceso cod    : ", g_proceso_cod
   DISPLAY "Opera cod      : ", g_opera_cod
   DISPLAY "Folio          : ", p_folio
   DISPLAY "Nombre archivo : ", g_nombre_archivo
   
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".RETS467.log")
   
   -- Llamado a función que genera el archivo de salida
   CALL fn_archivo_salida(p_folio, p_usuario_cod)
   
   
END MAIN

{
======================================================================
Clave: 
Nombre: fn_archivo_salida
Fecha creacion: Noviembre 28, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Funcion que se encarga de crear el archivo de salida de notificaciones
de pago de grupos 2, 3 y 4
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_archivo_salida(p_folio, p_usuario_cod)
DEFINE p_folio              LIKE glo_folio.folio,
       p_usuario_cod        LIKE seg_usuario.usuario_cod,
       v_id_solicitud       LIKE ret_fondo_ahorro_generico.id_solicitud, -- num de solicitud
       v_id_solicitud_envio DECIMAL(9,0),
       v_encabezado         STRING,
       v_sumario            STRING,
       v_detalle            STRING,
       v_estado_solicitud   SMALLINT,
       v_espacios_cza       CHAR(278),
       v_espacios_det       CHAR(127),
       v_espacios_sum       CHAR(238),
       i                    INTEGER,
       v_nss                CHAR(11),
       v_indicador          CHAR(1),
       v_imp_total          DECIMAL(11,2),
       v_fch_pago           DATE,
       v_fch_pago_teso      DATE,
       v_fecha_mes_pago     DATE,
       v_grupo              CHAR(4),
       v_gpo_ley73          SMALLINT,
       v_precio_aivs        DECIMAL(15,6),
       v_imp_transfer_gf    DECIMAL(11,2),
       v_monto_pagado       DECIMAL(11,2),
       v_aivs_pago          DECIMAL(15,6),
       v_nom_archivo                    STRING, -- nombre del archivo de salida
       v_v_ruta_nomarch                 STRING, -- ruta y nombre del archivo de salida
       v_c_ruta_env_acr                 LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
       v_ch_arch_ret_generico           BASE.CHANNEL,  -- manejador de apuntador hacia archivo
       cont_cza_solicitud               SMALLINT,      -- Contador de encabezado de solicitudes
       v_comando                        STRING,
       v_s_registro                     STRING,        -- registro a insertar
       v_bandera                        SMALLINT,
       p_titulo                         STRING,        -- titulo del mensaje enviado en el correo
       p_mensaje                        STRING,        -- cuerpo del mensaje enviado     
       v_sql                            STRING,
       v_id_derechohabiente             LIKE afi_derechohabiente.id_derechohabiente, -- id del derechohabiente
       v_nombre_af                      LIKE afi_derechohabiente.nombre_af, -- nombre del titular
       v_ap_paterno_af                  LIKE afi_derechohabiente.ap_paterno_af, -- apellido paterno del titular
       v_ap_materno_af                  LIKE afi_derechohabiente.ap_materno_af, -- apellido materno del titular
       v_nombre_fondo72                 LIKE afi_fondo72.nombre, -- nombre completo del titula en fondo de ahorro
       v_nom_completo                   STRING,
       v_nom_beneficiario               CHAR (40),
       v_s_nom_beneficiario             STRING,
       v_ap_beneficiario                CHAR (40),
       v_s_ap_beneficiario              STRING,
       v_am_beneficiario                CHAR (40),
       v_s_am_beneficiario              STRING,
       v_s_detalle                      STRING,
       v_modalidad_retiro               LIKE ret_solicitud_generico.modalidad_retiro, -- modalidad de retiro
       -- VARIABLES PARA CALCULAR EL HASH DEL ARCHIVO GENERADO
       v_chpipe                         base.Channel, -- channel para leer la salida standard
       v_r_ret_ctr_archivo_fico         RECORD LIKE ret_ctr_archivo_fico.*, -- registro de control de archivo
       v_hash                           STRING, -- hash calculado
       v_tokenizer                      base.StringTokenizer, -- para obtener el hash
       v_conteo                         INTEGER, -- contador de registros
       v_monto_pesos                    DECIMAL(12,2),
       v_ruta_bin                       LIKE seg_modulo.ruta_bin, -- ruta donde estan los ejecutables
       v_cambio_directorio              SMALLINT,
       v_cadena                         STRING, -- cadena para formatear texto
       v_id_entidad                     LIKE ret_cat_entidad_federativa.id_entidad, -- clave de la entidad
       v_des_entidad                    LIKE ret_cat_entidad_federativa.des_entidad,-- descripcion
       v_ejecuta_sh                     STRING,
       v_tipo_retiro                    SMALLINT 

   -- se envia la cadena que indica el fin de la etapa
   CALL fn_display_proceso(0, "NOTIFICACIÓN PAGO GRUPOS 2 3 Y 4")
   LET v_estado_solicitud = 80
   
   -- se obtiene la ruta de envio y ejecutable
   SELECT ruta_envio, ruta_bin
   INTO   v_c_ruta_env_acr, v_ruta_bin
   FROM   seg_modulo
   WHERE  modulo_cod = "ret"

   DISPLAY "___________________________________________________________________________"
   DISPLAY " Generando archivo de salida de notificación de pago de grupos 2, 3 y 4"
   
     -- se genera el folio
   CALL fn_genera_folio(g_proceso_cod, g_opera_cod, p_usuario_cod) RETURNING p_folio 

   -- el archivo con ruta destino que contiene el detalle
   LET v_v_ruta_nomarch = v_c_ruta_env_acr CLIPPED , "/", g_nombre_archivo
   
   -- nombre de archivo generado
   DISPLAY "~~~~~~~~~~~"
   DISPLAY "Archivo de Notificación de pago de grupos 2, 3 y 4 generado: ", v_v_ruta_nomarch

   -- se crea el manejador de archivo
   LET v_ch_arch_ret_generico = base.Channel.create()
   CALL v_ch_arch_ret_generico.setDelimiter(NULL)
   
   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_ret_generico.openFile(v_v_ruta_nomarch, "w" )

   -- Se ingresan los registros del encabezado de la solicitud
   LET cont_cza_solicitud = 1

   -- se inicia el contador de registros
   LET v_conteo = 0

   LET v_espacios_cza = 278 SPACES 
   LET v_encabezado = "01040400203001", TODAY USING "YYYY", TODAY USING "MM", TODAY USING "DD", v_espacios_cza 
   CALL v_ch_arch_ret_generico.write(v_encabezado)

   
   -- se buscan las solicitudes de ret-ly73_generico

--   LET v_sql    =   " SELECT DISTINCT b.id_solicitud, c.nss, b.id_derechohabiente, b.gpo_ley73, 1 as tipo_retiro \n ",
--                    " FROM   ret_preliquida a, \n ",
--                    "        ret_ley73_generico b,  \n ",
--                    "        afi_derechohabiente c, \n ",
--                    "        ret_solicitud_generico d  \n ",
--                    " WHERE  a.id_derechohabiente = b.id_derechohabiente \n ",
--                    " AND    b.id_derechohabiente = c.id_derechohabiente \n ",
--                    " AND    b.id_derechohabiente = d.id_derechohabiente \n ",
--                    " AND    a.id_referencia = b.id_solicitud \n ",
--                    " AND    b.id_solicitud = d.id_solicitud \n ",
--                    " AND    b.gpo_ley73 IN (2,3,4) \n ",
--                    " AND    b.estado_solicitud NOT IN (80,81,82) \n ";
   LET v_sql    =   " SELECT DISTINCT b.id_solicitud, c.nss, b.id_derechohabiente, b.gpo_ley73, 1 AS tipo_retiro \n ",
                    " FROM   ret_preliquida a, \n ",
                    "        ret_ley73_generico b,  \n ",
                    "        afi_derechohabiente c, \n ",
                    "        ret_solicitud_generico d  \n ",
                    " WHERE  a.id_derechohabiente = b.id_derechohabiente \n ",
                    " AND    b.id_derechohabiente = c.id_derechohabiente \n ",
                    " AND    b.id_derechohabiente = d.id_derechohabiente \n ",
                    " AND    a.id_referencia = b.id_solicitud \n ",
                    " AND    b.id_solicitud = d.id_solicitud \n ",
                    " AND    b.gpo_ley73 IN (2,3,4) \n ",
                    " AND    b.estado_solicitud IN (71,72) \n ",
                    " UNION ALL \n ",
                    " SELECT DISTINCT b.id_solicitud, c.nss, b.id_derechohabiente, 4 as gpo_ley73, 2 AS tipo_retiro \n ",
                    " FROM   ret_preliquida a, \n ",
                    "        ret_solo_infonavit b,  \n ",
                    "        afi_derechohabiente c \n ",
                    " WHERE  a.id_derechohabiente = b.id_derechohabiente \n ",
                    " AND    b.id_derechohabiente = c.id_derechohabiente \n ",
                    " AND    a.id_referencia = b.id_solicitud \n ",
                    " AND    b.estado_solicitud IN (60) \n ",
                    " AND    a.subcuenta = 4 \n " -- Se comenta la siguiente seccion hasta liberar el de excepciones,
--                    " UNION ALL \n ",
--                    " SELECT DISTINCT b.id_solicitud, c.nss, c.id_derechohabiente, 2 as gpo_ley73, 3 AS tipo_retiro \n ",
--                    " FROM   ret_preliquida a, \n ",
--                    "        ret_excep_devol_ssv b, \n ", 
--                    "        afi_derechohabiente c \n ",
--                    " WHERE  a.id_derechohabiente = c.id_derechohabiente \n ",
--                    " AND    a.id_referencia = b.id_solicitud \n ",
--                    " AND    a.folio_liquida = b.folio \n ",
--                    " AND    b.estado_solicitud IN (71,72) \n ";

   PREPARE stm_ret_notifica_generico FROM v_sql
   DECLARE cur_ret_notifica_generico CURSOR FOR stm_ret_notifica_generico

   -- se cambian de estatus las solicitudes liquidadas a informadas
   FOREACH cur_ret_notifica_generico INTO v_id_solicitud          ,
                                          v_nss                   ,
                                          v_id_derechohabiente    ,
                                          v_gpo_ley73,
                                          v_tipo_retiro
      LET v_indicador = 1
      LET v_imp_total = 0
      LET v_monto_pagado = 0
      LET v_aivs_pago = 0
      LET v_imp_transfer_gf = 0
      LET v_precio_aivs = 0
      LET v_fch_pago = NULL
      
      SELECT SUM(monto_pesos * (-1)), SUM(monto_acciones * (-1)), f_liquida
      INTO   v_monto_pagado, v_aivs_pago, v_fch_pago
      FROM   ret_preliquida 
      WHERE  id_derechohabiente = v_id_derechohabiente
      AND    id_referencia = v_id_solicitud
      AND    monto_pesos < 0
      AND    subcuenta IN (4,8)
      GROUP BY f_liquida
      IF v_monto_pagado IS NULL THEN 
         LET v_monto_pagado = 0
         LET v_aivs_pago = 0
      END IF 

      IF v_fch_pago IS NOT NULL THEN 
         LET v_imp_total = v_monto_pagado
         LET v_fecha_mes_pago = MDY(MONTH(v_fch_pago),1,YEAR(v_fch_pago))
         SELECT precio_fondo
         INTO   v_precio_aivs
         FROM   glo_valor_fondo
         WHERE  fondo = 11
         AND    f_valuacion = v_fecha_mes_pago
         LET v_indicador = 2
      END IF 
      SELECT SUM(monto_pesos * (-1)), f_liquida
      INTO   v_imp_transfer_gf, v_fch_pago_teso
      FROM   ret_preliquida 
      WHERE  id_derechohabiente = v_id_derechohabiente
      AND    id_referencia = v_id_solicitud
      AND    monto_pesos < 0
      AND    subcuenta IN (47)
      GROUP BY f_liquida
      IF v_imp_transfer_gf IS NULL THEN 
         LET v_imp_transfer_gf = 0
      END IF 
      IF v_precio_aivs IS NULL THEN 
         LET v_precio_aivs = 0
      END IF 
      LET v_imp_total = v_imp_total + v_imp_transfer_gf 
      IF v_fch_pago IS NULL AND v_fch_pago_teso IS NOT NULL THEN 
         LET v_fch_pago = v_fch_pago_teso
         LET v_indicador = 1
      END IF 

      IF v_gpo_ley73 = 2 THEN 
         LET v_grupo = '0102'
      END IF 

      IF v_gpo_ley73 = 3 THEN 
         LET v_grupo = '0103'
      END IF 
      IF v_gpo_ley73 = 4 THEN
         LET v_grupo = '0114'
         IF v_imp_transfer_gf > 10000  AND v_monto_pagado = 0 THEN 
            LET v_grupo = '0104'
         END IF 
         IF  v_imp_transfer_gf <= 10000  AND v_monto_pagado = 0 THEN 
            LET v_grupo = '0504'
         END IF 
         IF v_monto_pagado > 0 AND v_imp_transfer_gf > 0 THEN 
            LET v_grupo = '0124'
         END IF 
      END IF 
      LET v_espacios_det = 127 SPACES 
      LET v_detalle = "030476", v_nss, "           ",v_indicador,v_imp_total*100 USING "&&&&&&&&&&&",v_fch_pago USING "YYYYMMDD",
                      v_grupo, v_precio_aivs * 1000000 USING "&&&&&&&&&&&&&&&",v_imp_transfer_gf * 100 USING "&&&&&&&&&&&",
                      v_monto_pagado * 100 USING "&&&&&&&&&&&", v_aivs_pago * 1000000 USING "&&&&&&&&&&&&&&&", 
                      "   ", -- Afore Notificada
                      "000000000000000", -- Número de AIV Aplicado de Vivienda 97 
                      "000000000000000", -- Número de AIV Aplicado de Vivienda 92 
                      "000000000000000", -- Número de AIV Existente de Vivienda 97 del Derechohabiente
                      "000000000000000", -- Número de AIV Existente de Vivienda 92 del Derechohabiente
                      "   ", -- Diagnóstico PROCESAR
                      "   ", -- Diagnóstico AFORE
                      v_espacios_det
      DISPLAY "El detalle :", v_detalle
      
      -- se cambia el estado de la solicitud a notificada

      CASE v_tipo_retiro 
         WHEN 1 -- UPDATE ret_solicitud_generico
      
            UPDATE   ret_solicitud_generico
            SET      estado_solicitud = v_estado_solicitud -- enviada a Procesar
            WHERE    id_solicitud     = v_id_solicitud
            
            -- la modalidad es fondo de ahorro, por tanto los datos salen de ret_fondo_ahorro
            UPDATE   ret_ley73_generico
            SET      estado_solicitud = v_estado_solicitud -- enviada a Procesar
            WHERE    id_solicitud     = v_id_solicitud
            
         WHEN 2  -- UPDATE ret_solo_infonavit

            UPDATE   ret_solo_infonavit
            SET      estado_solicitud = v_estado_solicitud -- enviada a Procesar
            WHERE    id_solicitud     = v_id_solicitud
         
         WHEN 3  -- UPDATE ret_excep_devol_ssv

            UPDATE   ret_excep_devol_ssv
            SET      estado_solicitud = v_estado_solicitud -- enviada a Procesar
            WHERE    id_solicitud     = v_id_solicitud
         OTHERWISE
            DISPLAY "Se procesa una solicitud sin tipo de retiro válido NSS:", v_nss, " tipo retiro:", v_tipo_retiro
         
      END CASE 
      SELECT seq_ret_notifica_gpo.nextval
      INTO   v_id_solicitud_envio
      FROM   systables
      WHERE  tabid = 1;
      

      INSERT INTO ret_notifica_gpo 
           VALUES (v_id_solicitud_envio, v_id_solicitud, v_estado_solicitud, TODAY, NULL,
                   v_nss, v_indicador, v_imp_total, v_fch_pago, v_grupo, v_precio_aivs,
                   v_imp_transfer_gf,v_monto_pagado, v_aivs_pago,0,v_tipo_retiro);
                 
      CALL v_ch_arch_ret_generico.write(v_detalle)
      
      -- se cuenta un registro escrito
      LET v_conteo = v_conteo + 1
      LET v_indicador = 1
             
   END FOREACH


   
   -- se buscan las solicitudes a reenviar
   -- del titular de la cuenta
   LET v_sql    =   " SELECT  a.id_solicitud_retiro, a.nss,                         \n ",
                 "         a.indicador_cargo, a.importe_total,                        \n",
                 "         a.fch_pago, a.grupo,                                     \n ",
                 "         a.valor_aiv, a.imp_gob_fed, a.monto_pagado, a.aivs_pagadas \n ",
                 "   FROM  ret_notifica_gpo_reenvio a                                         \n "
   
   PREPARE stm_ret_notifica FROM v_sql
   DECLARE cur_ret_notifica CURSOR FOR stm_ret_notifica

   -- se cambian de estatus las solicitudes liquidadas a informadas
   FOREACH cur_ret_notifica INTO v_id_solicitud         ,
                                 v_nss                ,
                                 v_indicador              ,
                                 v_imp_total               ,
                                 v_fch_pago               ,
                                 v_grupo           ,
                                 v_precio_aivs           ,
                                 v_imp_transfer_gf,
                                 v_monto_pagado              ,
                                 v_aivs_pago
      LET v_espacios_det = 127 SPACES 
      LET v_detalle = "030476", v_nss, "           ",v_indicador,v_imp_total*100 USING "&&&&&&&&&&&",v_fch_pago USING "YYYYMMDD",
                      v_grupo, v_precio_aivs * 1000000 USING "&&&&&&&&&&&&&&&",v_imp_transfer_gf * 100 USING "&&&&&&&&&&&",
                      v_monto_pagado * 100 USING "&&&&&&&&&&&", v_aivs_pago * 1000000 USING "&&&&&&&&&&&&&&&",
                      "   ", -- Afore Notificada
                      "000000000000000", -- Número de AIV Aplicado de Vivienda 97 
                      "000000000000000", -- Número de AIV Aplicado de Vivienda 92 
                      "000000000000000", -- Número de AIV Existente de Vivienda 97 del Derechohabiente
                      "000000000000000", -- Número de AIV Existente de Vivienda 92 del Derechohabiente
                      "   ", -- Diagnóstico PROCESAR
                      "   ", -- Diagnóstico AFORE
                      v_espacios_det
      DISPLAY "El detalle :", v_detalle
      
      -- se cambia el estado de la solicitud a notificada
      -- UPDATE ret_fondo_ahorro
      UPDATE   ret_solicitud_generico
      SET      estado_solicitud = v_estado_solicitud -- enviada a Procesar
      WHERE    id_solicitud     = v_id_solicitud
      
      -- la modalidad es fondo de ahorro, por tanto los datos salen de ret_fondo_ahorro
      UPDATE   ret_ley73_generico
      SET      estado_solicitud = v_estado_solicitud -- enviada a Procesar
      WHERE    id_solicitud     = v_id_solicitud

      SELECT seq_ret_notifica_gpo.nextval
      INTO   v_id_solicitud_envio
      FROM   systables
      WHERE  tabid = 1;
      

      INSERT INTO ret_notifica_gpo 
           VALUES (v_id_solicitud_envio, v_id_solicitud, v_estado_solicitud, TODAY, NULL,
                   v_nss, v_indicador, v_imp_total, v_fch_pago, v_grupo, v_precio_aivs,
                   v_imp_transfer_gf,v_monto_pagado, v_aivs_pago,0,1);
                 
      CALL v_ch_arch_ret_generico.write(v_detalle)
      
      -- se cuenta un registro escrito
      LET v_conteo = v_conteo + 1
             
   END FOREACH
   LET v_espacios_sum = 238 SPACES 
   LET v_sumario = "09040400203001",v_conteo USING "&&&&&&",
                   "000000",   --Número de Registros Únicos
                   "000000000000", --Número de AIV Aplicativas
                   "000000000000", --Número de AIV Aplicadas 
                   "000000", -- Número de Rechazos por Procesar
                   "000000", -- Número de Rechazos por Afore
                   v_espacios_sum
   CALL v_ch_arch_ret_generico.write(v_sumario)
   -- se cierra el archivo
   CALL v_ch_arch_ret_generico.close()

   DISPLAY "Finalizado..."

   -- Elimina los registros de la tabla ret_notifica_gpo_reenvio
   DELETE 
   FROM   ret_notifica_gpo_reenvio
   WHERE  1 = 1;
   
   -- se envia la cadena que indica el fin de la etapa
   CALL fn_display_proceso(1, "Notificación de Pago de Grupos 2, 3 y 4 finalizado")

   -- se finaliza la operacion
   CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
                     RETURNING v_bandera

   -- se complementa el mensaje
   LET p_mensaje = "Generación de archivo de Notificación de Pago de Grupos 2, 3 y 4 realizado."
                        
   -- se crea el titulo del mensaje
   LET p_titulo = "Finalización de operación - Notificación de Pago de Grupos 2, 3 y 4"
   
    CALL fn_actualiza_opera_fin(g_pid, g_proceso_cod, g_opera_cod)
                                  RETURNING v_bandera
               
   -- se invoca el envio de correo electronico de notificacion
   CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                          NULL, -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)
   

END FUNCTION 
