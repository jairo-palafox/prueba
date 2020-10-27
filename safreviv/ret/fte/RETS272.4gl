--=============================================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--=============================================================================
##############################################################################################
#MODULO            =>RET                                                                     #
#PROGRAMA          =>RETS272                                                                 #
#OBJETIVO          =>Programa que genera el archivo de salida con las cuentas por pagar      #
#                    de las solicitudes restituidas que sera enviado a FICO para que las     #
#                    cancele                                                                 #
# Autor           Fecha      Modificación                                                    #
# Eneas Armas     20140122   Se cambia la tabla ret_fondo_ahorro por ret_fondo_ahorro_generico
#                 20140122   Se cambia la tabla ret_ley73 por ret_ley73_generico
##############################################################################################
IMPORT os
DATABASE safre_viv
GLOBALS "RETG01.4gl" --Archivo que almacena las variables globales del modulo

GLOBALS
DEFINE g_pid            LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod    LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod      LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- nombre del usuario
       p_folio          LIKE glo_folio.folio, -- numero de folio
       v_bandera        SMALLINT,
       v_conteo         INTEGER, -- contador de registros
       p_titulo         STRING, -- titulo del mensaje enviado en el correo
       p_mensaje        STRING, -- cuerpo del mensaje enviado
       p_ruta_envio     LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
       p_ruta_bin       LIKE seg_modulo.ruta_bin

   -- se reciben los parametros del programa
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET g_nombre_archivo = ARG_VAL(6)
   
   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".RETS272.log")   


   -- se obtiene la ruta de envio y ejecutable
   SELECT ruta_envio, ruta_bin
   INTO   p_ruta_envio, p_ruta_bin
   FROM   seg_modulo
   WHERE  modulo_cod = "ret"

     -- se genera el folio
   CALL fn_genera_folio(g_proceso_cod, g_opera_cod, p_usuario_cod) RETURNING p_folio 

   -- 16Dic2013. Se verifica si hay datos para generación de archivo de cancelación
   SELECT COUNT(*)
   INTO   v_conteo
   FROM   ret_solicitud_generico
   WHERE  estado_solicitud = 210 AND cod_rechazo = 65 AND modalidad_retiro = 9 -- listos para generación de archivo de cancelación
   
   -- si no hay registros para generación de archivo de cancelación
   IF ( v_conteo < 1 ) THEN
      -- se crea el titulo del mensaje que se enviara por correo
      LET p_titulo = "Generación de archivo de cancelación"
   
      -- se construye el mensaje
      LET p_mensaje = "ID Proceso  : ", g_pid, "\n", 
                      "Proceso      : RETIRO AMORTIZACIONES EXCEDENTES\n",
                      "Operación    : GENERACIÓN ARCHIVO DE CANCELACIÓN\n",
                      "Fecha Inicio : ", TODAY, "\n",
                      "Fecha Fin    : ", TODAY, "\n\n",
                      "\n__________________________________________________________________",
                      "\nNo se tienen solicitudes para la generación de archivo de cancelación.\nNo es necesario ejecutar esta etapa.",
                      "\nProceso Vacio"
      
      -- se despliega para que aparezca en el log
      DISPLAY p_mensaje

      -- se envia el correo de notificacion
      CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                             NULL, --no lleva archivo adjunto
                             p_titulo,
                             p_mensaje)

                       
   ELSE -- se ejecuta la generación de archivo de cancelación
   
      --  invoca la creación de los archivos
      -- se invoca anulación normal
      CALL fn_genera_archivos_fico(p_ruta_envio,p_ruta_bin)
      
      -- se invoca anulación rechazos
      --CALL fn_genera_archivos_fico(2,v_c_ruta_env_acr,v_ruta_bin)
      
      -- se finaliza la operacion
      CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
                        RETURNING v_bandera

      -- se complementa el mensaje
      LET p_mensaje = "Generación de archivo para cancelación de cuentas por pagar para FICO realizado."
                           
      -- se crea el titulo del mensaje
      LET p_titulo = "Finalización de operación - ARCHIVO PARA CIERRE DE CXP SACI-FICO"
      
       CALL fn_actualiza_opera_fin(g_pid, g_proceso_cod, g_opera_cod)
                                     RETURNING v_bandera
                  
      -- se invoca el envio de correo electronico de notificacion
      CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                             NULL, -- no lleva archivo adjunto
                             p_titulo,
                             p_mensaje)
   END IF

END MAIN

-- Función que genera los archivos de respuesta a FICO
FUNCTION fn_genera_archivos_fico(p_ruta_envio, p_ruta_bin)
DEFINE p_tipo_archivo   SMALLINT, -- parámetro para definir la creación del tipo de archivo
       p_ruta_envio     LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
       p_ruta_bin       LIKE seg_modulo.ruta_bin,
       v_sql                         STRING, -- cadena con enunciado SQL
       v_id_solicitud                LIKE ret_voluntaria.id_solicitud,
       v_sociedad_fico               LIKE ret_respuesta_fico.sociedad,
       v_num_documento_fico          LIKE ret_respuesta_fico.cta_x_pagar,
       v_ejercicio_fico              LIKE ret_respuesta_fico.anho,
       r_det_arch_fico RECORD
       	sociedad         CHAR(4),                      --Control del archivo
       	num_documento    CHAR(10),
       	ejercicio        CHAR(4)
     END RECORD , -- registro de fondo ahorro
       v_nom_archivo                    STRING, -- nombre del archivo de salida
       v_extension_txt                  STRING, -- extension del archivo de salida
       v_extension_key                  STRING, -- extension KEY del archivo con el HASH
       v_archivo_txt                    STRING, -- nombre y extension del archivo con el detalle
       v_archivo_key                    STRING, -- nombre y extension del archivo con el HASH
       v_v_ruta_nomarch                 STRING, -- ruta y nombre del archivo de salida
       v_c_ruta_env_acr                 LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
       v_ch_arch_ret_generico           BASE.CHANNEL,  -- manejador de apuntador hacia archivo
       v_comando                        STRING,

       v_id_derechohabiente             LIKE afi_derechohabiente.id_derechohabiente, -- id del derechohabiente
       v_nombre_af                      LIKE afi_derechohabiente.nombre_af, -- nombre del titular
       v_ap_paterno_af                  LIKE afi_derechohabiente.ap_paterno_af, -- apellido paterno del titular
       v_ap_materno_af                  LIKE afi_derechohabiente.ap_materno_af, -- apellido materno del titular
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

       v_id_entidad                     LIKE ret_cat_entidad_federativa.id_entidad, -- clave de la entidad
       v_des_entidad                    LIKE ret_cat_entidad_federativa.des_entidad, -- descripcion  
       v_des_etapa                      STRING, --descripción de la etapa
       v_des_complemento                STRING  --complemento de la leyenda
      
       	LET v_des_etapa = "ARCHIVO PARA CIERRE DE CXP SACI-FICO"
       	-- los nombres son todo en mayusculas con la siguiente mascara
         -- SACI_ANULAAAAAMMDD.TXT
         -- SACI_TRANSFAAAAMMDD.KEY
         LET v_nom_archivo = "SACI_ANULACXP", TODAY USING "yyyymmdd"
         
         -- asinga nulo a complemento en leyendas
         LET v_des_complemento =""
         
         -- crea query de consulta
         LET v_sql="   SELECT rf.sociedad   ,                ",
                   "\n        rf.cta_x_pagar,                 ",
                   "\n        rf.anho       ,                 ",
                   "\n        rs.id_solicitud                 ",
                   "\n FROM   ret_solicitud_generico rs,      ",
                   "\n        ret_respuesta_fico     rf       ",
                   "\n WHERE  rs.estado_solicitud = 210       ",
                   "\n AND    rs.id_solicitud = rf.referencia ",
                   "\n AND    rf.bandera      = 0             ",
                   "\n AND    rs.modalidad_retiro = 9         "
    
   -- se envia la cadena que indica el inicio de la etapa
   CALL fn_display_proceso(0,v_des_etapa)


   -- las extensiones del archivo son TXT para el detalle y KEY para el hash
   LET v_extension_txt = ".TXT"
   LET v_extension_key = ".KEY"

   LET v_archivo_txt = v_nom_archivo, v_extension_txt
   LET v_archivo_key = v_nom_archivo, v_extension_key
   
   -- el archivo con ruta destino que contiene el detalle
   LET v_v_ruta_nomarch = p_ruta_envio CLIPPED , "/", v_archivo_txt
   
   -- nombre de archivo generado
   DISPLAY "~~~~~~~~~~~"
   DISPLAY "Archivo de cierre de cuentas por pagar ",v_des_complemento,": ", v_v_ruta_nomarch

   -- =====================================================================
   -- se registra el archivo y su hash en la tabla de control
   -- se obtiene el id de archivo
   SELECT NVL(MAX(id_archivo),0) + 1
   INTO   v_r_ret_ctr_archivo_fico.id_archivo
   FROM   ret_ctr_archivo_fico
   
   LET v_r_ret_ctr_archivo_fico.nombre_archivo = v_archivo_txt
   LET v_r_ret_ctr_archivo_fico.num_registros  = 0
   LET v_r_ret_ctr_archivo_fico.tpo_archivo    = gi_tipo_archivo_envio -- envio
   LET v_r_ret_ctr_archivo_fico.f_actualiza    = TODAY
   LET v_r_ret_ctr_archivo_fico.h_actualiza    = CURRENT HOUR TO SECOND
   
   -- se inserta el registro en la tabla de control
   INSERT INTO ret_ctr_archivo_fico VALUES ( v_r_ret_ctr_archivo_fico.* )

   -- se crea el manejador de archivo
   LET v_ch_arch_ret_generico = base.Channel.create()
   CALL v_ch_arch_ret_generico.setDelimiter(NULL)
   
   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_ret_generico.openFile(v_v_ruta_nomarch, "w" )

   DISPLAY "_______________________________________________"
   DISPLAY "Generando archivo con instruccion de cierre de cuentas por pagar para SAP-FICO",v_des_complemento

   -- se inicia el contador de registros
   LET v_conteo = 0

   -- prepara statement de consulta
   PREPARE stm_consulta_retiro FROM v_sql
   DECLARE cur_cuentas CURSOR FOR stm_consulta_retiro
   	
   -- se leen las solicitudes que fueron restituidas y que tuvieron respuesta positiva de FICO
   -- para cada solicitud encontrada
   FOREACH cur_cuentas INTO v_sociedad_fico, v_num_documento_fico, v_ejercicio_fico, v_id_solicitud
      -- se asignan los valores al registro de detalle
      LET r_det_arch_fico.sociedad       = v_sociedad_fico
      LET r_det_arch_fico.num_documento  = v_num_documento_fico
      LET r_det_arch_fico.ejercicio      = v_ejercicio_fico

      -- se cambia el estado de la solicitud a notificada
      UPDATE   ret_solicitud_generico
      SET      estado_solicitud       = 211, -- enviada a cancelacion de cuenta por pagar
               id_archivo_cancela_cxp = v_r_ret_ctr_archivo_fico.id_archivo -- archivo en el que fue enviada la solicitud a pago
      WHERE    id_solicitud           = v_id_solicitud
      
      -- se verifica la modalidad para actualizar la tabla de solicitudes correspondiente
      -- 20140122   Se cambia la tabla ret_fondo_ahorro por ret_fondo_ahorro_generico
      -- 20140122   Se cambia la tabla ret_ley73 por ret_ley73_generico
      CASE v_modalidad_retiro
         WHEN 2 -- fondo de ahorro
            UPDATE   ret_fondo_ahorro_generico
            SET      estado_solicitud = 211 -- enviada a tesoreria
            WHERE    id_solicitud     = v_id_solicitud
         WHEN 3 -- ley 73
            UPDATE   ret_ley73_generico
            SET      estado_solicitud = 211 -- enviada a tesoreria
            WHERE    id_solicitud     = v_id_solicitud
         WHEN 9 -- amortizaciones excedentes
            UPDATE   ret_amort_excedente
            SET      estado_solicitud = 211 -- enviada a tesoreria
            WHERE    id_solicitud     = v_id_solicitud
         WHEN 10 -- aportaciones voluntarias
            UPDATE   ret_voluntaria
            SET      estado_solicitud = 211 -- enviada a tesoreria
            WHERE    id_solicitud     = v_id_solicitud
      END CASE
      
      -- Asigna el string
      LET v_s_detalle = r_det_arch_fico.sociedad     ,
                        r_det_arch_fico.num_documento,
                        r_det_arch_fico.ejercicio    
                       
      -- se escribe el registro en archivo   
      CALL v_ch_arch_ret_generico.write(v_s_detalle)
      
      -- se cuenta un registro escrito
      LET v_conteo = v_conteo + 1
             
   END FOREACH
   
   -- se cierra el archivo
   CALL v_ch_arch_ret_generico.close()

   -- se cambia el formato de archivo a DOS
   LET v_comando = p_ruta_envio CLIPPED
   CALL os.Path.chdir(v_comando) RETURNING v_cambio_directorio
   RUN "pwd"

   LET v_comando = "unix2dos . " , v_archivo_txt
   DISPLAY "Codificando el archivo a formato DOS..."
   DISPLAY v_comando
   RUN v_comando
   DISPLAY "Finalizado..."

   -- se crea el SHA1 del archivo
   LET v_comando = "shasum ", v_v_ruta_nomarch, " > ", p_ruta_envio CLIPPED, "/", v_archivo_key
   DISPLAY "\n\n ==== Generando hash del archivo ==="
   DISPLAY v_comando
   RUN v_comando 
   DISPLAY "Finalizado..."
   
   --Genera copia del archivo
   LET v_comando = "cp ",v_v_ruta_nomarch," ",p_ruta_envio CLIPPED,"/SACI_ANULACXP.TXT"
   DISPLAY "\n\n === Generando archivo a carpeta de transferencia para Axway === "
   DISPLAY v_comando
   RUN v_comando
   DISPLAY "Archivo TXT... Correcto"
   
   --Genera copia del archivo KEY
   LET v_comando = "shasum ", p_ruta_envio CLIPPED, "/SACI_ANULACXP.TXT > ", p_ruta_envio CLIPPED, "/SACI_ANULACXP.KEY"
   DISPLAY "\n\n ==== Generando hash del archivo ==="
   DISPLAY v_comando
   RUN v_comando 
   DISPLAY "Finalizado..."
   
   LET v_comando = v_ruta_bin CLIPPED
   CALL os.Path.chdir(v_comando) RETURNING v_cambio_directorio
   RUN "pwd"
   
   -- se obtiene el hash y se registra el archivo en la tabla de control
   LET v_chpipe = base.Channel.create()

   -- se conforma el comando que calculara el hash
   LET v_comando = "shasum ", v_v_ruta_nomarch
   
   -- se abre comunicacion para leer la salida standard
   CALL v_chpipe.openPipe( v_comando, "u")

   CALL v_chpipe.setDelimiter(" ")
   
   DISPLAY "Obteniendo HASH para base de datos: ", v_comando
   
   -- se lee el resultado del hash
   WHILE v_chpipe.read([v_hash])
      -- se crea un tokenizer para obtener el hash y el nombre del archivo
      LET v_tokenizer = base.StringTokenizer.create(v_hash," ")
      
      -- si hay tokens
      IF ( v_tokenizer.hasMoreTokens() ) THEN
        
         -- =====================================================================
         -- se actualiza el hash y el numero de registros en el registro de control
         LET v_r_ret_ctr_archivo_fico.hash           = v_tokenizer.nextToken()         
         DISPLAY "Hash calculado: ", v_r_ret_ctr_archivo_fico.hash
         LET v_r_ret_ctr_archivo_fico.num_registros  = v_conteo
         LET v_r_ret_ctr_archivo_fico.f_actualiza    = TODAY
         LET v_r_ret_ctr_archivo_fico.h_actualiza    = CURRENT HOUR TO SECOND
         
         UPDATE ret_ctr_archivo_fico
         SET    hash          = v_r_ret_ctr_archivo_fico.hash         ,
                num_registros = v_r_ret_ctr_archivo_fico.num_registros,
                f_actualiza   = v_r_ret_ctr_archivo_fico.f_actualiza  ,
                h_actualiza   = v_r_ret_ctr_archivo_fico.h_actualiza  
         WHERE  id_archivo    = v_r_ret_ctr_archivo_fico.id_archivo
      END IF
   END WHILE

   LET v_comando = "sh ", p_ruta_envio CLIPPED, "/SACI_ANULACXP.sh"
   RUN v_comando
   DISPLAY "Finalizado... \n\n"
      
   -- se envia la cadena que indica el fin de la etapa
   CALL fn_display_proceso(1,v_des_etapa)
END FUNCTION