--=============================================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--=============================================================================
##############################################################################################
#MODULO            =>RET                                                                     #
#PROGRAMA          =>RETS2250                                                                #
#OBJETIVO          =>PROGRAMA QUE EJECUTA EL PROCESO DE GENERACION                           #
#                    DE ARCHIVO DE SALIDA DEL FONDO DE AHORRO                                #
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
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".RETS2250.log")
   
   -- Llamado a función que genera el archivo de salida
   CALL fn_archivo_salida(p_folio, p_usuario_cod)
   
   
END MAIN

{
======================================================================
Clave: 
Nombre: fn_archivo_salida
Fecha creacion: Octubre 02, 2013
Autor: Esteban Sanchez, EFP
Narrativa del proceso que realiza:
Funcion que se encarga de crear el archivo de salida para FICO
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega     31 Oct 2013             - INFONAVIT solicito por correo cambio en la
                                        cuenta contable asociada por instruccion
                                        de FICO.
                                        De: 2203120004
                                        A : 2203120019
                                        
                                        Correo de Alejandra N. Gonzalez
                                        31Oct2013 1206pm
Ivan Vega     18 Dic 2013             - Concepto FICO para pago por:
                                        Fondo Ahorro: 17
Ivan Vega     12Mar2014               - Se cambian los conceptos de FICO segun
                                        documento enviado por Benjamin Rodriguez (Infonavit)
                                        el 17 de dic de 2013
======================================================================
}
FUNCTION fn_archivo_salida(p_folio, p_usuario_cod)
DEFINE p_folio        LIKE glo_folio.folio,
       p_usuario_cod  LIKE seg_usuario.usuario_cod,
       v_id_solicitud LIKE ret_fondo_ahorro_generico.id_solicitud, -- num de solicitud
       r_det_arch_fico RECORD
       	v_control          CHAR(2),                      --Control del archivo
       	v_delegacion       CHAR(2),
       	v_concepto         CHAR (4),
       	v_referencia       CHAR(20),
       	v_monto            CHAR(12),
       	v_banco            CHAR(4),
       	v_fec_pago         CHAR(10),
       	v_ref_definitiva   CHAR(25),
       	v_acreedor         CHAR(10),
       	V_sociedad         CHAR(4),
       	v_gpo_ctas_acrdr   CHAR(4),
       	v_nom_acreedor_1   CHAR(35),
       	v_nom_acreedor_2   CHAR(35),
        v_beneficiario     CHAR(60),
       	v_cve_pais         CHAR(3),
       	v_estado           CHAR(3),
       	v_rfc              CHAR(16),
        v_nss              CHAR(11), -- NSS del trabajador
        v_lista_vias_pago  CHAR(10),
        v_deleg_mpo        CHAR(35),
        v_cta_asoc_conta   CHAR(10),
        v_cta_clabe        CHAR(18)
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
       v_nombre_af                      CHAR (40), --LIKE afi_derechohabiente.nombre_af, -- nombre del titular
       v_ap_paterno_af                  CHAR (40), --LIKE afi_derechohabiente.ap_paterno_af, -- apellido paterno del titular
       v_ap_materno_af                  CHAR (40), --LIKE afi_derechohabiente.ap_materno_af, -- apellido materno del titular
       v_nombre_fondo72                 CHAR (40), --LIKE afi_fondo72.nombre, -- nombre completo del titula en fondo de ahorro
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
       v_importe_viv97_anexo1           LIKE ret_ley73_generico.importe_viv97_anexo1

     
   --Asigna valores genéricos o desconocidos al momento
   LET r_det_arch_fico.v_control         = "18"  
   LET r_det_arch_fico.v_concepto        = 4  SPACES -- cambia segun lo que se vaya a pagar
   LET r_det_arch_fico.v_monto           = 12 SPACES
   LET r_det_arch_fico.v_banco           = 4  SPACES
   LET r_det_arch_fico.v_fec_pago        = TODAY USING "dd/mm/yyyy"
   LET r_det_arch_fico.v_ref_definitiva  = 25 SPACES
   LET r_det_arch_fico.v_acreedor        = 10 SPACES
   LET r_det_arch_fico.v_sociedad        = "INFO"
   LET r_det_arch_fico.v_gpo_ctas_acrdr  = "PFON"
   LET r_det_arch_fico.v_cve_pais        = "MX "

   LET r_det_arch_fico.v_lista_vias_pago = "         T"
   
   LET r_det_arch_fico.v_cta_asoc_conta  = "2203120019"
   
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
   FROM   ret_solicitud_generico a,
          ret_pago_spei b
   WHERE  a.estado_solicitud = 60 -- listos para generación de archivo FICO
   AND    a.modalidad_retiro = 2   -- Solo Fondo de Ahorro
   AND    a.id_solicitud = b.id_solicitud -- que se pagan por SPEI
   
   -- si no hay registros para la generación de archivo
   IF ( v_conteo < 1 ) THEN
      -- se crea el titulo del mensaje que se enviara por correo
      LET p_titulo = "Generación de archivo FICO"
   
      -- se construye el mensaje
      LET p_mensaje = "ID Proceso  : ", g_pid, "\n", 
                      "Proceso      : RETIRO FONDO DE AHORRO\n",
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
      LET v_nom_archivo = "SACI_TRANSFA", TODAY USING "yyyymmdd"
      LET v_archivo_txt = v_nom_archivo, v_extension_txt
      LET v_archivo_key = v_nom_archivo, v_extension_key
      
      -- el archivo con ruta destino que contiene el detalle
      LET v_v_ruta_nomarch = v_c_ruta_env_acr CLIPPED , "/", v_archivo_txt
      
      -- nombre de archivo generado
      DISPLAY "~~~~~~~~~~~"
      DISPLAY "Archivo de solicitudes de retiro genérico: ", v_v_ruta_nomarch

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
      -- SOLICITUDES QUE OBTIENEN LOS DATOS DEL TITULAR DE afi_fondo72. RETIRO FONDO DE AHORRO
      
      -- =========================================================================================

      -- se leen las solicitudes liquidadas que usan la tabla de afiliacion para obtener el nombre
      -- del titular de la cuenta
      LET v_sql="\n SELECT rs.id_derechohabiente      ,                 ",
                "\n        afi.nombre                 ,                 ",
                "\n        rs.id_solicitud            ,                 ",
                "\n        rb.nombre                  ,                 ",
                "\n        rb.ap_paterno              ,                 ",
                "\n        rb.ap_materno              ,                 ",
                "\n        rs.nss                     ,                 ",
                "\n        rs.rfc                     ,                 ",
                "\n        rp.cuenta_clabe            ,                 ",
                "\n        rs.modalidad_retiro        ,                 ",
                "\n        rb.id_entidad_federativa   ,                 ",
                "\n        ce.des_entidad                               ",
                "\n FROM ret_solicitud_generico rs    ,                 ",
                "\n      ret_beneficiario_generico rb                   ",
                "\n      LEFT OUTER JOIN ret_cat_entidad_federativa ce  ",
                "\n                   ON rb.id_entidad_federativa = ce.id_entidad, ",                
                "\n      ret_pago_spei rp             ,                 ",
                "\n      afi_fondo72         afi                        ",
                "\n WHERE rs.estado_solicitud = 60                      ",
                "\n AND rs.modalidad_retiro = 2                         ", -- solo fondo de ahorro
                "\n AND rb.id_solicitud = rs.id_solicitud               ",
                "\n AND rp.id_solicitud = rb.id_solicitud               ",
                "\n AND rp.consec_beneficiario = rb.consec_beneficiario ",
                "\n AND afi.id_derechohabiente = rs.id_derechohabiente  ",
                "\n AND afi.nss = rs.nss                                ",
                "\n AND afi.ind_estado_cuenta  = 0                      "
      
      PREPARE stm_ret_genericof72 FROM v_sql
      DECLARE cur_ret_genericof72 CURSOR FOR stm_ret_genericof72

      -- se cambian de estatus las solicitudes liquidadas a informadas
      FOREACH cur_ret_genericof72 INTO v_id_derechohabiente       ,
                                    v_nombre_fondo72           ,
                                    v_id_solicitud             ,
                                    v_nom_beneficiario         ,
                                    v_ap_beneficiario          ,
                                    v_am_beneficiario          ,
                                    r_det_arch_fico.v_nss      ,
                                    r_det_arch_fico.v_rfc      ,
                                    r_det_arch_fico.v_cta_clabe,
                                    v_modalidad_retiro         ,
                                    v_id_entidad               ,
                                    v_des_entidad
                                    
         -- la entidad, delegacion y su clave son las de la solicitud
         LET r_det_arch_fico.v_delegacion      = v_id_entidad using "&&"
         LET r_det_arch_fico.v_estado          = v_id_entidad using "&&&"
         LET r_det_arch_fico.v_deleg_mpo       = v_des_entidad CLIPPED


         -- Beneficiario
         -- Formatea el nombre
         LET v_s_nom_beneficiario = v_nom_beneficiario                            
         LET v_s_ap_beneficiario  = v_ap_beneficiario
         LET v_s_am_beneficiario  = v_am_beneficiario
         
         --Concatena el nombre completo
         LET v_nom_completo = v_s_nom_beneficiario.trim()," ",v_s_ap_beneficiario.trim()," ",v_s_am_beneficiario.trim()
         
         -- se asigna el nombre del beneficiario
         LET r_det_arch_fico.v_beneficiario = UPSHIFT(v_nom_completo.trim())

         {-- Formatea el nombre del titular (se usa la variable del de beneficiario)
         --LET v_s_nom_beneficiario = v_nombre_fondo72

         -- Concatena el nombre completo
         --LET v_nom_completo = v_s_nom_beneficiario.trim()}

         -- Se pidio usar el nombre obtenidio de CRM
         LET v_nom_completo = v_s_nom_beneficiario.trim()," ",v_s_ap_beneficiario.trim()," ",v_s_am_beneficiario.trim()         
         
         
         -- Se pasa la identificación de la solicitud a la referencia y referencia definitiva, deben ir
         -- alineados a la izquierda
         LET v_cadena = v_id_solicitud USING "####################"
         LET r_det_arch_fico.v_referencia     = v_cadena.trim()
         LET r_det_arch_fico.v_ref_definitiva = v_cadena.trim()

         -- Elimina espacios
         LET v_nom_completo = UPSHIFT(v_nom_completo.trim())
         
         -- Valida la longitud de la cadena
         IF v_nom_completo.getLength() > 35 THEN
             -- Se formatea el nombre conforme lo solicitado
            LET r_det_arch_fico.v_nom_acreedor_1 = v_nom_completo.subString(1,35)
            LET r_det_arch_fico.v_nom_acreedor_2 = v_nom_completo.subString(36,v_nom_completo.getLength())
         ELSE
             -- Formatea el nombre
            LET r_det_arch_fico.v_nom_acreedor_1 = v_nom_completo.subString(1,v_nom_completo.getLength())
            LET r_det_arch_fico.v_nom_acreedor_2 = 35 SPACES
         END IF

                                    
         -- se cambia el estado de la solicitud a notificada
         -- UPDATE ret_fondo_ahorro
         UPDATE   ret_solicitud_generico
         SET      estado_solicitud = 70, -- enviada a tesoreria
                  id_archivo_envio = v_r_ret_ctr_archivo_fico.id_archivo -- archivo en el que fue enviada la solicitud a pago
         WHERE    id_solicitud     = v_id_solicitud
         
         -- la modalidad es fondo de ahorro, por tanto los datos salen de ret_fondo_ahorro
         UPDATE   ret_fondo_ahorro_generico
         SET      estado_solicitud = 70 -- enviada a tesoreria
         WHERE    id_solicitud     = v_id_solicitud
         
         -- el concepto de amortizaciones excedentes es 18. Alineado a la derecha
         LET r_det_arch_fico.v_concepto = "  17"
         
         -- monto sale de la tabla de solicitudes del retiro
         SELECT (saldo_viv72 + tanto_adicional)
         INTO   v_monto_pesos
         FROM   ret_fondo_ahorro_generico
         WHERE  id_solicitud = v_id_solicitud
         
         -- se formatea el monto
         LET r_det_arch_fico.v_monto           = v_monto_pesos USING "&&&&&&&&&.&&"
            
         
         -- Asigna el string
         LET v_s_detalle = r_det_arch_fico.v_control        ,
                           r_det_arch_fico.v_delegacion     ,
                           r_det_arch_fico.v_concepto       ,
                           r_det_arch_fico.v_referencia     ,
                           r_det_arch_fico.v_monto          ,
                           r_det_arch_fico.v_banco          ,
                           r_det_arch_fico.v_fec_pago       ,
                           r_det_arch_fico.v_ref_definitiva ,
                           r_det_arch_fico.v_acreedor       ,
                           r_det_arch_fico.V_sociedad       ,
                           r_det_arch_fico.v_gpo_ctas_acrdr ,
                           r_det_arch_fico.v_nom_acreedor_1 ,
                           r_det_arch_fico.v_nom_acreedor_2 ,
                           r_det_arch_fico.v_beneficiario   ,
                           r_det_arch_fico.v_cve_pais       ,
                           r_det_arch_fico.v_estado         ,
                           r_det_arch_fico.v_rfc            ,
                           r_det_arch_fico.v_nss            ,
                           r_det_arch_fico.v_lista_vias_pago,
                           r_det_arch_fico.v_deleg_mpo      ,
                           r_det_arch_fico.v_cta_asoc_conta ,
                           r_det_arch_fico.v_cta_clabe      
                          
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
      LET v_comando = "cp ",v_v_ruta_nomarch," ",v_c_ruta_env_acr CLIPPED,"/SACI_TRANSFA.TXT"
      DISPLAY "\n\n === Generando archivo de transferencia para Axway === "
      DISPLAY v_comando
      RUN v_comando
      DISPLAY "Archivo TXT... Correcto"
      
      -- Genera SHA1 del archivo copia
      LET v_comando = "shasum ", v_c_ruta_env_acr CLIPPED, "/SACI_TRANSFA.TXT > ", v_c_ruta_env_acr CLIPPED, "/SACI_TRANSFA.KEY"
      DISPLAY "\n\n ==== Generando hash del archivo SACI_TRANSFA.TXT ==="
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
      LET v_ejecuta_sh = "sh /safreviv_int/ret/envio/SACI_TRANSFA.sh"
      RUN v_ejecuta_sh

      --LET v_ejecuta_sh = " sh /opt/Interpel/Scripts/SACI_TRANSF_KEY.sh"
      LET v_ejecuta_sh = "sh /safreviv_int/ret/envio/SACI_TRANSFA_KEY.sh"
      RUN v_ejecuta_sh

      -- se envia la cadena que indica el fin de la etapa
      CALL fn_display_proceso(1, "GENERACION ARCHIVO DE ENTRADA SAP-FICO")

      -- se finaliza la operacion
      CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
                        RETURNING v_bandera

      -- se complementa el mensaje
      LET p_mensaje = "Generación de archivo de retiro genérico para FICO realizado."
                           
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
