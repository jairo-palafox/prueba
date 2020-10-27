--=============================================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--=============================================================================
##############################################################################################
#MODULO            =>RET                                                                     #
#PROGRAMA          =>RETS452                                                                 #
#OBJETIVO          =>PROGRAMA QUE EJECUTA EL PROCESO DE GENERACION                           #
#                    DE ARCHIVO DE SALIDA DE LOS PAGOS PARA FICO                             #
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
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".RETS452.log")
   
   -- Llamado a función que genera el archivo de salida
   CALL fn_archivo_salida(p_folio, p_usuario_cod)
   
   
END MAIN

{
======================================================================
Clave: 
Nombre: fn_archivo_salida
Fecha creacion: Agosto 2017
Narrativa del proceso que realiza:
Funcion que se encarga de crear el archivo de salida para FICO
Registro de modificaciones:
======================================================================
}
FUNCTION fn_archivo_salida(p_folio, p_usuario_cod)
DEFINE p_folio        LIKE glo_folio.folio,
       p_usuario_cod  LIKE seg_usuario.usuario_cod,
       v_id_solicitud LIKE ret_fondo_ahorro_generico.id_solicitud, -- num de solicitud
       v_c_id_sol     CHAR(9),
       r_det_arch_fico RECORD
       	v_delegacion       CHAR  (2),
       	v_concepto         CHAR  (2),
       	v_nss              CHAR (11), -- NSS del trabajador
         v_beneficiario     CHAR (40),
       	v_monto            CHAR (12),
       	v_cve_dap          CHAR  (4),
       	v_fec_contable     CHAR (10),
         v_fecha_vence      CHAR (10),
       	v_ref_dap          CHAR (20)
     END RECORD , -- registro de fondo ahorro
       v_nom_archivo                    STRING, -- nombre del archivo de salida
       v_extension_txt                  STRING, -- extension del archivo de salida
       v_extension_key                  STRING, -- extension KEY del archivo con el HASH
       v_archivo_txt                    STRING, -- nombre y extension del archivo con el detalle
       v_archivo_key                    STRING, -- nombre y extension del archivo con el HASH
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
       -- para evaluar el grupo y subgrupo de ley73 y poner el concepto y cuenta correctos
       v_gpo_ley73                      LIKE ret_ley73_generico.gpo_ley73,
       v_subgrupo                       LIKE ret_ley73_generico.subgrupo,
       v_importe_viv92                  LIKE ret_ley73_generico.importe_viv92,
       v_importe_viv97                  LIKE ret_ley73_generico.importe_viv97,
       v_importe_viv97_anexo1           LIKE ret_ley73_generico.importe_viv97_anexo1,
       v_monto                          DECIMAL(14,2),
       v_fch_contabiliza                DATE,
       v_fch_vencimiento                DATE

       DEFINE v_num_pago                SMALLINT
       DEFINE v_num_parcialidades       SMALLINT

     
   --Asigna valores genéricos o desconocidos al momento
   LET r_det_arch_fico.v_concepto        = 4  SPACES -- cambia segun lo que se vaya a pagar
   LET r_det_arch_fico.v_monto           = 12 SPACES
   LET r_det_arch_fico.v_fec_contable    = TODAY USING "dd/mm/yyyy"
   
   -- se envia la cadena que indica el fin de la etapa
   CALL fn_display_proceso(0, "GENERACION ARCHIVO DE ENTRADA SAP-FICO")
   
   -- se obtiene la ruta de envio y ejecutable
   SELECT ruta_envio, ruta_bin
   INTO   v_c_ruta_env_acr, v_ruta_bin
   FROM   seg_modulo
   WHERE  modulo_cod = "ret"

   DISPLAY "___________________________________________________________________________"
   DISPLAY " Generando archivo de salida para creación de cuentas por pagar a SAP-FICO"
   
     -- se genera el folio
   CALL fn_genera_folio(g_proceso_cod, g_opera_cod, p_usuario_cod) RETURNING p_folio 

   -- 16Dic2013. Se verifica si hay datos para generación de archivo FICO
   SELECT COUNT(*)
   INTO   v_conteo
   FROM   ret_excep_devol_ssv 
   WHERE  estado_solicitud = 60 -- listos para generación de archivo FICO
   
   -- si no hay registros para la generación de archivo
   IF ( v_conteo < 1 ) THEN
      -- se crea el titulo del mensaje que se enviara por correo
      LET p_titulo = "Generación de archivo FICO"
   
      -- se construye el mensaje
      LET p_mensaje = "ID Proceso  : ", g_pid, "\n", 
                      "Proceso      : RETIRO POR EXCEPCIONES DE DEVOLUCION DE LA SSV\n",
                      "Operación    : GENERACIÓN DE ARCHIVO FICO\n",
                      "Fecha Inicio : ", TODAY, "\n",
                      "Fecha Fin    : ", TODAY, "\n\n",
                      "\n__________________________________________________________________",
                      "\nNo se tienen solicitudes para generar archivo. No es necesario ejecutar la etapa de generación de archivo FICO.",
                      "\nProceso Vacio"
      
      -- se despliega para que aparezca en el log
      DISPLAY p_mensaje

      -- se envia el correo de notificacion
      CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                             NULL, --no lleva archivo adjunto
                             p_titulo,
                             p_mensaje)
   ELSE -- se ejecuta la generación de archivo
      -- las extensiones del archivo son TXT para el detalle y KEY para el hash
      LET v_extension_txt = ".TXT"
      LET v_extension_key = ".KEY"

      -- los nombres son todo en mayusculas con la siguiente mascara
      -- SACI_TRANSFAAAAMMDD.TXT
      -- SACI_TRANSFAAAAMMDD.KEY
      LET v_nom_archivo = "SACI_EXCEPCIONES_DEVOL_SSV_", TODAY USING "yyyymmdd" --- se quita la fecha del nombre del archivo
      LET v_archivo_txt = v_nom_archivo, v_extension_txt
      LET v_archivo_key = v_nom_archivo, v_extension_key
      
      -- el archivo con ruta destino que contiene el detalle
      LET v_v_ruta_nomarch = v_c_ruta_env_acr CLIPPED , "/", v_archivo_txt
      
      -- nombre de archivo generado
      DISPLAY "~~~~~~~~~~~"
      DISPLAY "Archivo de solicitudes de excepciones de devolución del SSV: ", v_v_ruta_nomarch

      -- =====================================================================
      -- se registra el archivo y su hash en la tabla de control
      -- se obtiene el id de archivo
      SELECT NVL(MAX(id_archivo),0) + 1
      INTO   v_r_ret_ctr_archivo_fico.id_archivo
      FROM   ret_ctr_archivo_fico
      
      LET v_r_ret_ctr_archivo_fico.nombre_archivo = v_archivo_txt
      LET v_r_ret_ctr_archivo_fico.num_registros  = 0
      LET v_r_ret_ctr_archivo_fico.tpo_archivo    = gi_tipo_archivo_envio  
      LET v_r_ret_ctr_archivo_fico.f_actualiza    = TODAY
      LET v_r_ret_ctr_archivo_fico.h_actualiza    = CURRENT HOUR TO SECOND
      
      -- se inserta el registro en la tabla de control
      INSERT INTO ret_ctr_archivo_fico VALUES ( v_r_ret_ctr_archivo_fico.* )

      -- se crea el manejador de archivo
      LET v_ch_arch_ret_generico = base.Channel.create()
      CALL v_ch_arch_ret_generico.setDelimiter(NULL)
      
      -- se crea archivo y se indica que se escribira en el mismo
      CALL v_ch_arch_ret_generico.openFile(v_v_ruta_nomarch, "w" )

      -- Se ingresan los registros del encabezado de la solicitud
      LET cont_cza_solicitud = 1

      
      -- =========================================================================================
      -- =========================================================================================
      -- SOLICITUDES QUE OBTIENEN LOS DATOS DEL TITULAR DE afi_derechohabiente
      
      -- =========================================================================================

      -- se leen las solicitudes liquidadas que usan la tabla de afiliacion para obtener el nombre
      -- del titular de la cuenta
      LET v_sql="\n SELECT a.num_delega                ,                     ",
                "\n        a.nss                       ,                     ",
                "\n        a.beneficiario              ,                     ",
                "\n        a.importe_cuenta            ,                     ",
                "\n        a.fch_contabiliza           ,                     ",
                "\n        a.fch_vencimiento           ,                     ",
                "\n        a.id_solicitud                                    ",
                "\n FROM   ret_excep_devol_ssv     a                         ",
                "\n WHERE  a.estado_solicitud      = 60                      "
      
      PREPARE stm_ret_generico FROM v_sql
      DECLARE cur_ret_generico CURSOR FOR stm_ret_generico       

      -- se inicia el contador de registros
      LET v_conteo = 0

      -- se cambian de estatus las solicitudes liquidadas a informadas
      FOREACH cur_ret_generico INTO v_id_entidad                       ,
                                    r_det_arch_fico.v_nss              ,
                                    r_det_arch_fico.v_beneficiario     ,
                                    v_monto                            ,
                                    v_fch_contabiliza                  ,
                                    v_fch_vencimiento                  ,
                                    v_id_solicitud          
                                    
         -- la entidad, delegacion y su clave son las de la solicitud
         LET v_c_id_sol = v_id_solicitud USING "&&&&&&&&&"
         LET r_det_arch_fico.v_delegacion      = v_id_entidad using "&&"
         LET r_det_arch_fico.v_monto           = v_monto USING "&&&&&&&&&.&&"
         LET r_det_arch_fico.v_fec_contable    = v_fch_contabiliza USING "dd/mm/yyyy"
         LET r_det_arch_fico.v_fecha_vence     = v_fch_vencimiento USING "dd/mm/yyyy"
         -- se solicita cambiar la referencia para dejarlo a 20 posiciones
         LET r_det_arch_fico.v_ref_dap = r_det_arch_fico.v_nss, v_c_id_sol[5,9]

			-- se actualiza la tabla de excepciones de la devolucion de la SSV
			UPDATE ret_excep_devol_ssv
			SET    estado_solicitud = 70,
			       id_archivo_envio = v_r_ret_ctr_archivo_fico.id_archivo -- archivo en el que fue enviada la solicitud a pago       
			WHERE  id_solicitud     = v_id_solicitud;
         
         -- Asigna el string
         LET v_s_detalle = r_det_arch_fico.v_delegacion     ,
                           "0F"                             ,
                           "  "                             ,
                           r_det_arch_fico.v_ref_dap        ,
                           r_det_arch_fico.v_beneficiario   ,
                           r_det_arch_fico.v_monto          ,
                           "006"                            ,
                           " "                              ,
                           r_det_arch_fico.v_fec_contable   ,
                           r_det_arch_fico.v_fecha_vence    ,
                           r_det_arch_fico.v_nss            ,
                           v_c_id_sol                       ,
                           "   "
                          
         -- se escribe el registro en archivo   
         CALL v_ch_arch_ret_generico.write(v_s_detalle)
         
         -- se cuenta un registro escrito
         LET v_conteo = v_conteo + 1

      END FOREACH

      -- se cierra el archivo
      CALL v_ch_arch_ret_generico.close()

      -- se cambia el formato de archivo a DOS
      LET v_comando = v_c_ruta_env_acr CLIPPED
      CALL os.Path.chdir(v_comando) RETURNING v_cambio_directorio
      RUN "pwd"

      LET v_comando = "unix2dos . " , v_archivo_txt
      DISPLAY "Codificando el archivo a formato DOS..."
      DISPLAY v_comando
      RUN v_comando
      DISPLAY "Finalizado..."

      -- se crea el SHA1 del archivo
      LET v_comando = "shasum ", v_v_ruta_nomarch, " > ", v_c_ruta_env_acr CLIPPED, "/", v_archivo_key
      DISPLAY "\n\n ==== Generando hash del archivo ==="
      DISPLAY v_comando
      RUN v_comando 
      DISPLAY "Finalizado..."
      
      --Genera copia del archivo
      LET v_comando = "cp ",v_v_ruta_nomarch," ",v_c_ruta_env_acr CLIPPED,"/SACI_EXCEPCIONES_DEVOL_SSV.TXT"
      DISPLAY "\n\n === Generando archivo de transferencia para Axway === "
      DISPLAY v_comando
      RUN v_comando
      DISPLAY "Archivo TXT... Correcto"
      
      -- Genera SHA1 del archivo copia
      LET v_comando = "shasum ", v_c_ruta_env_acr CLIPPED, "/SACI_EXCEPCIONES_DEVOL_SSV.TXT > ", v_c_ruta_env_acr CLIPPED, "/SACI_EXCEPCIONES_DEVOL_SSV.KEY"
      DISPLAY "\n\n ==== Generando hash del archivo SACI_PARCIAL.TXT ==="
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

      DISPLAY "\n\nEjecutando scripts de transferencia de archivos"
      --LET v_ejecuta_sh = " sh /opt/Interpel/Scripts/SACI_TRANSF.sh"
      LET v_ejecuta_sh = "sh /safreviv_int/ret/envio/SACI_TRANS_EXC.sh"
      RUN v_ejecuta_sh
      -- se envia la cadena que indica el fin de la etapa
      CALL fn_display_proceso(1, "GENERACION ARCHIVO DE ENTRADA SAP-FICO")

      -- se finaliza la operacion
      CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
                        RETURNING v_bandera

      -- se complementa el mensaje
      LET p_mensaje = "Generación de archivo de excepciones de devolucion de la SSV para FICO realizado."
                           
      -- se crea el titulo del mensaje
      LET p_titulo = "Finalización de operación - GENERACIÓN ARCHIVO FICO"
      
       CALL fn_actualiza_opera_fin(g_pid, g_proceso_cod, g_opera_cod)
                                     RETURNING v_bandera
                  
      -- se invoca el envio de correo electronico de notificacion
      CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                             NULL, -- no lleva archivo adjunto
                             p_titulo,
                             p_mensaje)
   END IF 

END FUNCTION 
