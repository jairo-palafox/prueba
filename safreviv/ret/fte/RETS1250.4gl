--=============================================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--=============================================================================
##############################################################################################
#MODULO            =>RET                                                                     #
#PROGRAMA          =>RETS1250                                                                #
#OBJETIVO          =>PROGRAMA QUE EJECUTA EL PROCESO DE GENERACION                           #
#                    DE ARCHIVO DE SALIDA DE RETIROS LEY 73 PARA FICO                        #
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
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".RETS1250.log")
   
   -- Llamado a funci�n que genera el archivo de salida
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
       v_importe_viv97_anexo1           LIKE ret_ley73_generico.importe_viv97_anexo1,
       v_porcentaje                     DECIMAL(5,2),
       v_consec_beneficiario            SMALLINT,
       v_medio_entrega                  SMALLINT,
       v_caso_crm                       CHAR(10),
       v_caso_crm_num                   DECIMAL(10,0),
       v_casos_num                      DECIMAL(10,0),
       v_tpo_beneficiario               SMALLINT

   DEFINE v_regreso       SMALLINT 

   DEFINE arr_casos_crm DYNAMIC ARRAY OF RECORD
         casos           STRING, 
         fecha_creacion  CHAR(10),
         status          CHAR(5),
         fecha_modifica  CHAR(10),
         clase_operacion STRING,
         tipificacion    CHAR(4),
         texto_status    STRING,
         permite_adoc    CHAR(05),
         marca_origen    STRING
   END RECORD 

   DEFINE v_indice       INTEGER 
   DEFINE v_con_caso     INTEGER 
   DEFINE v_existe_juridico SMALLINT
     
   --Asigna valores gen�ricos o desconocidos al momento
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
   DISPLAY " Generando archivo de salida para creaci�n de cuentas por pagar a SAP-FICO"
   
     -- se genera el folio
   CALL fn_genera_folio(g_proceso_cod, g_opera_cod, p_usuario_cod) RETURNING p_folio 

   -- 16Dic2013. Se verifica si hay datos para generaci�n de archivo FICO
   SELECT COUNT(*)
   INTO   v_conteo
   FROM   ret_solicitud_generico a,
          ret_pago_spei b
   WHERE  a.estado_solicitud = 60 -- listos para generaci�n de archivo FICO
   AND    a.modalidad_retiro = 3  -- Ley 73 
   AND    a.id_solicitud = b.id_solicitud -- que se pagan por SPEI
   
   -- si no hay registros para la generaci�n de archivo
   IF ( v_conteo < 1 ) THEN
      -- se crea el titulo del mensaje que se enviara por correo
      LET p_titulo = "Generaci�n de archivo FICO"
   
      -- se construye el mensaje
      LET p_mensaje = "ID Proceso  : ", g_pid, "\n", 
                      "Proceso      : RETIRO LEY 73\n",
                      "Operaci�n    : GENERACI�N DE ARCHIVO FICO\n",
                      "Fecha Inicio : ", TODAY, "\n",
                      "Fecha Fin    : ", TODAY, "\n\n",
                      "\n__________________________________________________________________",
                      "\nNo se tienen solicitudes para generar archivo. No es necesario ejecutar la etapa de generaci�n de archivo FICO.",
                      "\nProceso Vacio"
      
      -- se despliega para que aparezca en el log
      DISPLAY p_mensaje

      -- se envia el correo de notificacion
      CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                             NULL, --no lleva archivo adjunto
                             p_titulo,
                             p_mensaje)
   ELSE -- se ejecuta la generaci�n de archivo
      -- las extensiones del archivo son TXT para el detalle y KEY para el hash
      LET v_extension_txt = ".TXT"
      LET v_extension_key = ".KEY"

      -- los nombres son todo en mayusculas con la siguiente mascara
      -- SACI_TRANSFAAAAMMDD.TXT
      -- SACI_TRANSFAAAAMMDD.KEY
      LET v_nom_archivo = "SACI_TRANS_SSV", TODAY USING "yyyymmdd"
      LET v_archivo_txt = v_nom_archivo, v_extension_txt
      LET v_archivo_key = v_nom_archivo, v_extension_key
      
      -- el archivo con ruta destino que contiene el detalle
      LET v_v_ruta_nomarch = v_c_ruta_env_acr CLIPPED , "/", v_archivo_txt
      
      -- nombre de archivo generado
      DISPLAY "~~~~~~~~~~~"
      DISPLAY "Archivo de solicitudes de retiro gen�rico: ", v_v_ruta_nomarch

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
      LET v_sql="\n SELECT rs.id_derechohabiente      ,                 ",
                -- Se pidio llenar ambos nombres con la informacion de CRM
                "\n        rb.nombre                  ,                 ",
                "\n        rb.ap_paterno              ,                 ",
                "\n        rb.ap_materno              ,                 ",
                --"\n        afi.nombre_af              ,                 ",
                --"\n        afi.ap_paterno_af          ,                 ",
                --"\n        afi.ap_materno_af          ,                 ",
                "\n        rs.id_solicitud            ,                 ",
                "\n        rb.nombre                  ,                 ",
                "\n        rb.ap_paterno              ,                 ",
                "\n        rb.ap_materno              ,                 ",
                "\n        rs.nss                     ,                 ",
                "\n        rs.rfc                     ,                 ",
                "\n        NVL(rp.cuenta_clabe,rsiaf.cuenta_clabe),     ",
                "\n        rs.modalidad_retiro        ,                 ",
                "\n        rb.id_entidad_federativa   ,                 ",
                "\n        ce.des_entidad             ,                 ",
                "\n        rb.porcentaje              ,                 ",
                "\n        rb.consec_beneficiario     ,                 ",
                "\n        rsme.medio_entrega         ,                 ",
                "\n        rs.caso_adai               ,                 ",
                "\n        rb.tpo_beneficiario                          ",
                "\n FROM ret_solicitud_generico rs    ,                 ",
                "\n      ret_sol_medio_entrega rsme   ,                 ",
                "\n      ret_beneficiario_generico rb                   ",
                "\n      LEFT OUTER JOIN ret_pago_spei rp               ",
                "\n                   ON rp.id_solicitud = rb.id_solicitud ",
                "\n                  AND rp.consec_beneficiario = rb.consec_beneficiario      ",
                "\n      LEFT OUTER JOIN ret_pago_siaf rsiaf            ",
                "\n                   ON rsiaf.id_solicitud = rb.id_solicitud ",
                "\n                  AND rsiaf.consec_beneficiario = rb.consec_beneficiario , ",
                "\n      afi_derechohabiente afi      ,                 ",
                "\n      ret_cat_entidad_federativa ce                  ",
                "\n WHERE rs.estado_solicitud = 60                      ",
                "\n AND rs.modalidad_retiro = 3                         ", -- Ley 73
                "\n AND rs.id_solicitud = rsme.id_solicitud             ", 
                "\n AND rb.id_solicitud = rs.id_solicitud               ",
                "\n AND rb.importe > 0                                  ",
                "\n AND rs.id_derechohabiente  = afi.id_derechohabiente ",
                "\n AND ce.id_entidad  = rb.id_entidad_federativa "
                --"\n < "
      
      PREPARE stm_ret_generico FROM v_sql
      DECLARE cur_ret_generico CURSOR FOR stm_ret_generico       

      -- se inicia el contador de registros
      LET v_conteo = 0

      -- se cambian de estatus las solicitudes liquidadas a informadas
      FOREACH cur_ret_generico INTO v_id_derechohabiente       ,
                                    v_nombre_af                ,
                                    v_ap_paterno_af            ,
                                    v_ap_materno_af            ,
                                    v_id_solicitud             ,
                                    v_nom_beneficiario         ,
                                    v_ap_beneficiario          ,
                                    v_am_beneficiario          ,
                                    r_det_arch_fico.v_nss      ,
                                    r_det_arch_fico.v_rfc      ,
                                    r_det_arch_fico.v_cta_clabe,
                                    v_modalidad_retiro         ,
                                    v_id_entidad               ,
                                    v_des_entidad              ,
                                    v_porcentaje               ,
                                    v_consec_beneficiario      ,
                                    v_medio_entrega            ,
                                    v_caso_crm                 ,
                                    v_tpo_beneficiario
                                    
         -- la entidad, delegacion y su clave son las de la solicitud
         LET v_con_caso = 0
         LET r_det_arch_fico.v_delegacion      = v_id_entidad using "&&"
         LET r_det_arch_fico.v_estado          = v_id_entidad using "&&&"
         LET r_det_arch_fico.v_deleg_mpo       = v_des_entidad CLIPPED

         -- Formatea el nombre del titular
         LET v_s_nom_beneficiario = v_nombre_af                            
         LET v_s_ap_beneficiario  = v_ap_paterno_af
         LET v_s_am_beneficiario  = v_ap_materno_af
         
         -- Concatena el nombre completo
         LET v_nom_completo = v_s_nom_beneficiario.trim()," ",v_s_ap_beneficiario.trim()," ",v_s_am_beneficiario.trim()
         
         -- Se pasa la identificaci�n de la solicitud a la referencia y referencia definitiva, deben ir
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

         -- Formatea el nombre
         LET v_s_nom_beneficiario = v_nom_beneficiario                            
         LET v_s_ap_beneficiario  = v_ap_beneficiario
         LET v_s_am_beneficiario  = v_am_beneficiario
         
         --Concatena el nombre completo
         LET v_nom_completo = v_s_nom_beneficiario.trim()," ",v_s_ap_beneficiario.trim()," ",v_s_am_beneficiario.trim()
         
         -- se asigna el nombre del beneficiario
         LET r_det_arch_fico.v_beneficiario = UPSHIFT(v_nom_completo.trim())
                                    
         --Reviso si es grupo 4 con solo importe en TESOFE, estas se envian por SIAFF
         -- se obtiene el grupo y subgrupo de la solicitud de retiro
         SELECT gpo_ley73           ,
                subgrupo            ,
                importe_viv92       ,
                importe_viv97       ,
                importe_viv97_anexo1
         INTO   v_gpo_ley73           ,
                v_subgrupo            ,
                v_importe_viv92       ,
                v_importe_viv97       ,
                v_importe_viv97_anexo1
                
         FROM ret_ley73_generico
         WHERE id_solicitud = v_id_solicitud

         -- Se busca el tipo de beneficiario para saber si entro por Jur�dico
         -- Se busca con el servicio expuesto por CRM de BuscaCaso, si entro por Jur�dico solo del medio de entrega 3 (CRM)

         IF v_medio_entrega = 3  AND v_tpo_beneficiario = 2 THEN
            LET v_caso_crm_num = v_caso_crm 
--            CALL fn_busca_caso_crm(r_det_arch_fico.v_nss) RETURNING v_regreso, arr_casos_crm
--            LET v_con_caso = 0 
--            IF v_regreso = 0 THEN 
--               FOR v_indice = 1 TO arr_casos_crm.getLength()
--                  LET v_casos_num = arr_casos_crm[v_indice].casos
--                  IF v_casos_num = v_caso_crm_num AND 
--                     arr_casos_crm[v_indice].clase_operacion = "ZG1B" THEN 
                     LET v_con_caso = v_con_caso + 1
--                     EXIT FOR 
--                  END IF 
--               END FOR
--            END IF 
         END IF 

         IF v_gpo_ley73 = 4 AND v_importe_viv97 = 0 AND v_importe_viv97_anexo1 > 0 AND v_con_caso = 0 THEN
            --Esta se env�a por SIAFF
            DISPLAY "Se fue por el SIAFF",v_id_solicitud
            DISPLAY "Importe 92:",v_importe_viv92
            DISPLAY "Importe 97:",v_importe_viv97
            DISPLAY "Importe A1:",v_importe_viv97_anexo1
            CONTINUE FOREACH
         END IF

         -- se cambia el estado de la solicitud a notificada
         -- UPDATE ret_fondo_ahorro
         UPDATE   ret_solicitud_generico
         SET      estado_solicitud = 70, -- enviada a tesoreria
                  id_archivo_envio = v_r_ret_ctr_archivo_fico.id_archivo -- archivo en el que fue enviada la solicitud a pago
         WHERE    id_solicitud     = v_id_solicitud

         
         UPDATE   ret_ley73_generico
         SET      estado_solicitud = 70 -- enviada a tesoreria
         WHERE    id_solicitud     = v_id_solicitud

         -- monto sale de la tabla de solicitudes del retiro

         IF v_con_caso = 0 THEN 
            SELECT SUM(importe_viv92 + importe_viv97 + importe_viv97_anexo1)
            INTO   v_monto_pesos
            FROM   ret_ley73_generico
            WHERE  id_solicitud = v_id_solicitud
         ELSE 
            -- busca el registro en ret_beneficiario_juridico, si no lo encuentra lo inserta, de
            LET v_existe_juridico = 0
            SELECT COUNT(*)
            INTO   v_existe_juridico
            FROM   ret_beneficiario_juridico
            WHERE  id_solicitud = v_id_solicitud
            AND    consec_beneficiario = v_consec_beneficiario
            IF v_existe_juridico = 0 THEN 
               INSERT INTO ret_beneficiario_juridico (id_solicitud, consec_beneficiario, estado_solicitud, cod_rechazo)
                    VALUES (v_id_solicitud, v_consec_beneficiario, 70, 0);
            ELSE 
               UPDATE ret_beneficiario_juridico
               SET    estado_solicitud = 70
               WHERE  id_solicitud = v_id_solicitud
               AND    consec_beneficiario = v_consec_beneficiario
            END IF
            SELECT importe
            INTO   v_monto_pesos
            FROM   ret_beneficiario_generico
            WHERE  id_solicitud = v_id_solicitud
            AND    consec_beneficiario = v_consec_beneficiario
            LET r_det_arch_fico.v_referencia = r_det_arch_fico.v_referencia CLIPPED || v_consec_beneficiario CLIPPED || "DSSV"
--            LET r_det_arch_fico.v_ref_definitiva = r_det_arch_fico.v_ref_definitiva CLIPPED || v_consec_beneficiario
            LET r_det_arch_fico.v_ref_definitiva = r_det_arch_fico.v_nss
         END IF 
      
         -- se formatea el monto
         LET r_det_arch_fico.v_monto = v_monto_pesos USING "&&&&&&&&&.&&"

         -- se verifica el grupo para indicar el concepto correspondiente
         CASE v_gpo_ley73
            WHEN 1 -- grupo 1
               LET r_det_arch_fico.v_concepto = "  0C" -- viv 92 y viv 97

            WHEN 2 -- grupo 2
               -- si se tiene viv97 y anexo 1
               LET r_det_arch_fico.v_concepto = "  0B" -- viv 97 y anexo 1

            WHEN 3 -- grupo 3
               -- si se tiene viv97 y anexo 1
               LET r_det_arch_fico.v_concepto = "  0B" -- viv 97 y anexo 1

            WHEN 4 -- grupo 1
               -- si se paga viv92 y viv97 y anexo
               IF ( v_importe_viv92 > 0 OR v_importe_viv97 > 0 AND v_importe_viv97_anexo1 > 0 ) THEN
                  LET r_det_arch_fico.v_concepto = "  0B" -- viv 92 y viv97 y anexo 1
               ELSE
                  -- se paga alguno de los fondos de vivienda pero Anexo 1 NO
                  LET r_det_arch_fico.v_concepto = "  0C" -- viv 92, viv97 y anexo 1
               END IF
         
         END CASE
        
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
         DISPLAY "En el archivo:",v_s_detalle,"<"
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
      LET v_comando = "cp ",v_v_ruta_nomarch," ",v_c_ruta_env_acr CLIPPED,"/SACI_TRANS_SSV.TXT"
      DISPLAY "\n\n === Generando archivo de transferencia para Axway === "
      DISPLAY v_comando
      RUN v_comando
      DISPLAY "Archivo TXT... Correcto"
      
      -- Genera SHA1 del archivo copia
      LET v_comando = "shasum ", v_c_ruta_env_acr CLIPPED, "/SACI_TRANS_SSV.TXT > ", v_c_ruta_env_acr CLIPPED, "/SACI_TRANS_SSV.KEY"
      DISPLAY "\n\n ==== Generando hash del archivo SACI_TRANS_SSV.TXT ==="
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
      LET v_ejecuta_sh = "sh /safreviv_int/ret/envio/SACI_TRANS_SSV.sh"
      RUN v_ejecuta_sh

      --LET v_ejecuta_sh = " sh /opt/Interpel/Scripts/SACI_TRANSF_KEY.sh"
--      LET v_ejecuta_sh = "sh /safreviv_int/ret/envio/SACI_TRANS_SSV_KEY.sh"
--      RUN v_ejecuta_sh

      -- se envia la cadena que indica el fin de la etapa
      CALL fn_display_proceso(1, "GENERACION ARCHIVO DE ENTRADA SAP-FICO")

      -- se finaliza la operacion
      CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
                        RETURNING v_bandera

      -- se complementa el mensaje
      LET p_mensaje = "Generaci�n de archivo de retiro gen�rico para FICO realizado."
                           
      -- se crea el titulo del mensaje
      LET p_titulo = "Finalizaci�n de operaci�n - GENERACI�N ARCHIVO FICO"
      
       CALL fn_actualiza_opera_fin(g_pid, g_proceso_cod, g_opera_cod)
                                     RETURNING v_bandera
                  
      -- se invoca el envio de correo electronico de notificacion
      CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                             NULL, -- no lleva archivo adjunto
                             p_titulo,
                             p_mensaje)
   END IF 

END FUNCTION 
