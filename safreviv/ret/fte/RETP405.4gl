################################################################################
#Modulo       => RET                                                           #
#Programa     => RETP405                                                       #
#Objetivo     => Programa que ejecuta consultas para generación de archivo     #
#                delimitado por Pipes                                          #
#Fecha inicio => Febrero 22, 2012                                              #
################################################################################

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
       v_i_resultado          INTEGER, -- resultado del proceso
       r_bnd_fin_oper         SMALLINT,
       v_si_correcto_integra  SMALLINT,
       p_mensaje              STRING, -- cuerpo del mensaje enviado
       v_nss                  LIKE afi_derechohabiente.nss

   -- se recuperan los parametros la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   
   -- se asigna proceso y operacion
   LET g_pid         = p_pid      
   LET g_proceso_cod = p_proceso_cod -- retiros de LEY 73
   LET g_opera_cod   = p_opera_cod -- integracion

   -- se asume que el proceso termina correctamente
   LET v_i_resultado         = 0
   LET v_si_correcto_integra = 0
   LET v_nss                 = ""

   -- se envia la cadena que indica el inicio de etapa
   CALL fn_display_proceso(0, "INTEGRACION")

   -- se genera el folio
   CALL fn_genera_folio(g_proceso_cod, g_opera_cod,p_usuario_cod) RETURNING p_folio 
   -- se manda a llamar funcionalidad
   CALL fn_arma_archivo(p_folio)
   DISPLAY "\nLa integración se terminó completamente."
   DISPLAY "Ya puede revisar el archivo generado"

   -- se complementa el mensaje
   LET p_mensaje = p_mensaje || "Integración realizada con éxito \n.Ya se puede continuar con la Preliquidación"
      
   CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
   RETURNING r_bnd_fin_oper

   UPDATE glo_ctr_archivo
   SET estado = 2, -- integrado
       folio = p_folio
   WHERE proceso_cod = p_proceso_cod
   AND nombre_archivo = p_nombre_archivo;

   #Se actualiza el folio del proceso               
   UPDATE bat_ctr_proceso SET folio = p_folio WHERE pid = p_pid
   UPDATE bat_ctr_operacion SET folio = p_folio WHERE pid = p_pid
      
   -- se envia la cadena que indica el fin de la etapa
   CALL fn_display_proceso(1, "INTEGRACION")
   
END MAIN

FUNCTION fn_arma_archivo(v_folio)
   DEFINE
       v_folio                      LIKE deo_preliquida.folio_liquida,
       v_indice                     INTEGER, -- contador       
       v_sql                        STRING, -- cadena con instruccion sql
       v_sqlb                       STRING, -- cadena con instruccion sql beneficiarios
       v_nom_archivo                STRING, -- nombre del archivo de salida
       v_extension_txt              STRING, -- extension del archivo de salida
       v_archivo_txt                STRING, -- nombre y extension del archivo con el detalle
       v_mensaje_archivo            STRING, -- mensaje de generacion de archivo
       v_v_ruta_nomarch             STRING, -- ruta y nombre del archivo de salida
       v_ch_arch_ret_generico       BASE.CHANNEL,  -- manejador de apuntador hacia archivo
       v_s_detalle                  STRING,
       v_r_despliegue         RECORD
         id_solicitud               LIKE ret_solicitud_generico.id_solicitud    ,
         modalidad_retiro           LIKE ret_solicitud_generico.modalidad_retiro,
         desc_modalidad             VARCHAR(200)                                ,
         nss                        LIKE afi_derechohabiente.nss                ,
         nombre                     VARCHAR(120)                                ,
         rfc                        LIKE afi_derechohabiente.rfc                ,
         caso_adai                  LIKE ret_solicitud_generico.caso_adai       ,
         folio                      LIKE glo_folio.folio                        ,
         f_solicitud                LIKE ret_disposicion.f_solicitud            ,
         aivs                       LIKE ret_disposicion.aivs_viv92             ,
         pesos                      DECIMAL(22,2)                               ,
         monto                      DECIMAL(22,2)                               ,
         tanto_adicional            DECIMAL(22,2)                               ,
         ref_dap                    DECIMAL(15,0)                               ,
         estado_solicitud           VARCHAR(100)                                ,
         cod_rechazo                VARCHAR(100)                                ,
         nombre_archivo_e           LIKE ret_ctr_archivo_fico.nombre_archivo    ,
         fecha_archivo_e            LIKE ret_ctr_archivo_fico.f_actualiza       ,
         nombre_archivo_r           LIKE ret_ctr_archivo_fico.nombre_archivo    ,
         fecha_archivo_r            LIKE ret_ctr_archivo_fico.f_actualiza       ,
         nombre_archivo_cc          LIKE ret_ctr_archivo_fico.nombre_archivo    ,
         fecha_archivo_cc           LIKE ret_ctr_archivo_fico.f_actualiza       ,
         nombre_archivo_rc          LIKE ret_ctr_archivo_fico.nombre_archivo    ,
         fecha_archivo_rc           LIKE ret_ctr_archivo_fico.f_actualiza       ,
         id_derechohabiente         LIKE ret_solicitud_generico.id_derechohabiente,
         folio_restitucion          LIKE ret_solicitud_generico.folio_restitucion,
         des_estado                 LIKE ret_cat_edo_pago_fico.des_estado,
         rsp_referencia             LIKE ret_ws_consulta_pago_fico.rsp_referencia,
         rsp_f_pago                 LIKE ret_ws_consulta_pago_fico.rsp_f_pago,
         cta_x_pagar                LIKE ret_ws_consulta_pago_fico.documento,
         gpo_ley73                  VARCHAR(20),
         cve_afore                  SMALLINT,
         curp                       CHAR(18),
         curp_afi                   CHAR(18),
         grupo_ventanilla           SMALLINT,
         aivs_92                    LIKE ret_disposicion.aivs_viv92             ,
         aivs_97                    LIKE ret_disposicion.aivs_viv92             ,
         pesos_92                   DECIMAL(22,2)                               ,
         pesos_97                   DECIMAL(22,2)                               ,
         f_autorizacion             LIKE ret_ws_peticion_marca.f_peticion
       END RECORD,
       v_r_beneficiario       RECORD --arreglo para los beneficiarios de la solicitud
         consec_beneficiario 			LIKE ret_beneficiario_generico.consec_beneficiario,
         beneficiario        			LIKE ret_beneficiario_generico.tpo_beneficiario   ,
         tpo_pago            			LIKE ret_beneficiario_generico.tpo_pago           ,
         cod_parentesco      			LIKE ret_beneficiario_generico.cod_parentesco     ,
         ap_paterno          			LIKE ret_beneficiario_generico.ap_paterno         ,
         ap_materno          			LIKE ret_beneficiario_generico.ap_materno         ,
         nombre              			LIKE ret_beneficiario_generico.nombre             ,
         telefono            			LIKE ret_beneficiario_generico.telefono           ,
         correo              			LIKE ret_beneficiario_generico.correo             ,
         porcentaje          			LIKE ret_beneficiario_generico.porcentaje         ,
         aivs                			LIKE ret_beneficiario_generico.aivs               ,
         importe             			LIKE ret_beneficiario_generico.importe            ,
         aivs_92             			LIKE ret_beneficiario_generico.aivs               ,
         aivs_97             			LIKE ret_beneficiario_generico.aivs               ,
         pesos_92            			LIKE ret_beneficiario_generico.importe            ,
         pesos_97            			LIKE ret_beneficiario_generico.importe            ,
         rfc_benef           			CHAR(13)                                          ,
         curp_benef          			CHAR(18)
       END RECORD,
       v_r_respuesta          RECORD --arreglo para las repuestas de la solicitud
         acreedor            			LIKE ret_respuesta_fico.acreedor                  ,
         cta_clabe           			LIKE ret_respuesta_fico.cta_clabe                 ,
         cta_x_pagar         			LIKE ret_respuesta_fico.cta_x_pagar               ,
         anho                			LIKE ret_respuesta_fico.anho                      ,
         bandera             			LIKE ret_respuesta_fico.bandera                   ,
         acreedor_res        			LIKE ret_respuesta_fico.acreedor_res              ,
         banco_inter         			LIKE ret_respuesta_fico.banco_inter               ,
         des_error           			LIKE ret_respuesta_fico.des_error
       END RECORD,
       v_curp                			CHAR(18),
       v_c_ruta_env          			LIKE seg_modulo.ruta_envio,
       v_num_credito         			DECIMAL(10,0),
       v_resultado           			SMALLINT,
       v_tpo_originacion     			SMALLINT,
       v_tpo_credito         			SMALLINT,
       v_f_otorga            			DATE,
       v_f_liquida           			DATE,
       v_desc_tipo_credito   			CHAR(30),
       v_tipo_credito_desc   			CHAR(34),
       v_estado_credito      			CHAR(10),
       v_id_archivo_envio        	LIKE ret_solicitud_generico.id_archivo_envio,
       v_id_archivo_respuesta    	LIKE ret_solicitud_generico.id_archivo_respuesta,
       v_id_archivo_cancela_cxp  	LIKE ret_solicitud_generico.id_archivo_cancela_cxp,
       v_id_archivo_resp_cxp     	LIKE ret_solicitud_generico.id_archivo_resp_cxp,
       v_gpo_ley73               	LIKE ret_ley73_generico.gpo_ley73,
       v_max_fec_pago            	DATE

   -- se obtienen las solicitudes de los folios elegidos y con las condiciones dadas en la captura de consulta  
   LET v_sql = "\n SELECT                                         ",
               "\n a.id_solicitud                                ,",
               "\n a.modalidad_retiro                            ,",
               "\n a.modalidad_retiro || ' - ' || e.des_corta    ,",
               "\n a.nss                                         ,",
               "\n ''                                            ,",
               "\n a.rfc                                         ,",
               "\n a.caso_adai                                   ,",
               "\n a.folio                                       ,",
               "\n a.f_solicitud                                 ,",
               "\n 0                                             ,",
               "\n 0                                             ,",
               "\n 0                                             ,",
               "\n 0                                             ,",
               "\n ''                                            ,",
               "\n a.estado_solicitud || '-' || es.des_corta     ,",
               "\n a.cod_rechazo || '-' || cr.des_corta          ,",
               "\n ''                                            ,",
               "\n ''                                            ,",
               "\n ''                                            ,",
               "\n ''                                            ,",
               "\n ''                                            ,",
               "\n ''                                            ,",
               "\n ''                                            ,",
               "\n ''                                            ,",
               "\n a.id_derechohabiente                          ,",
               "\n a.folio_restitucion                           ,",
               "\n ''                                            ,",
               "\n ''                                            ,",
               "\n ''                                            ,",
               "\n ''                                            ,",
               "\n ''                                            ,",
               "\n ''                                            ,",
               "\n ''                                            ,",
               "\n ''                                            ,",
               "\n a.grupo_ventanilla                            ,",
               "\n ''                                            ,",
               "\n ''                                            ,",
               "\n ''                                            ,",
               "\n ''                                            ,",
               "\n ''                                            ,",
               "\n a.id_archivo_envio                            ,",
               "\n a.id_archivo_respuesta                        ,",
               "\n a.id_archivo_cancela_cxp                      ,",
               "\n a.id_archivo_resp_cxp                          ",
               "\n FROM ret_solicitud_generico  a ,               ",
               "\n ret_modalidad_retiro         e ,               ",
               "\n ret_estado_solicitud         es,               ",
               "\n ret_rechazo_generico         cr,               ",
               "\n safre_tmp:tmp_ret_multinss   trm               ",
               "\n WHERE a.modalidad_retiro = e.modalidad_retiro  ",
               "\n AND   a.estado_solicitud = es.estado_solicitud ",
               "\n AND   a.cod_rechazo      = cr.cod_rechazo      ",
               "\n AND   a.nss              = trm.nss             "


   #DISPLAY "Buscando solicitudes por tipo de retiro elegido:"
   #DISPLAY v_sql
   LET v_indice = 1

   -- Se obtiene la ruta de envio y ejecutable
   SELECT ruta_envio
   INTO   v_c_ruta_env 
   FROM   seg_modulo
   WHERE  modulo_cod = "ret"

   -- las extensiones del archivo son TXT para el detalle
   LET v_extension_txt = ".congen_res"

   -- los nombres son todo en mayusculas con la siguiente mascara
   -- SG_USUARIO_AAAAMMDD.TXT
   LET v_nom_archivo = "Consulta_MultiNSS_",v_folio USING "<<<<<<<<"
   LET v_archivo_txt = v_nom_archivo, v_extension_txt

   -- el archivo con ruta destino que contiene el detalle
   LET v_v_ruta_nomarch = v_c_ruta_env CLIPPED , "/", v_archivo_txt
   LET v_mensaje_archivo = "Se generara el archivo:", v_v_ruta_nomarch
   #CALL fn_mensaje("Atención", v_mensaje_archivo, "information")
   -- nombre de archivo generado
   DISPLAY "~~~~~~~~~~~"
   DISPLAY "Archivo generado: ", v_v_ruta_nomarch

   -- se crea el manejador de archivo
   LET v_ch_arch_ret_generico = base.Channel.create()
   CALL v_ch_arch_ret_generico.setDelimiter(NULL)

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_ret_generico.openFile(v_v_ruta_nomarch, "w" )

   -- Se imprime encabezado
   LET v_s_detalle = "Numero de Solicitud|",
                     "Descripcion Modalidad|",
                     "NSS|",
                     "Tipo de Grupo|",
                     "Clave de Afore|",
                     "Fecha de Pago|",
                     "AIVs 92|",
                     "AIVs 97|",
                     "AIVs Total|",
                     "Pesos 92|",
                     "Pesos 97|",
                     "Pesos Total|",
                     "Caso CRM|",
                     "Fecha Solicitud|",
                     "Fecha Autorizacion|",
                     "Monto|",
                     "Adicional|",
                     "Estado Solicitud|",
                     "Codigo de Retorno|",   --Solicitudes encontradas
                     "Nombre del titular|",
                     "Folio liquidación|",
                     "RFC|",
                     "Folio restitución|",
                     "CURP|",                --Detalle del registro
                     "Envio|",
                     "Fecha de Envio|",
                     "Respuesta|",
                     "Fecha de Respuesta|",
                     "Cancelación cuentas por pagar|",
                     "Fecha cancelación CxP|",
                     "Respuesta cuentas por pagar|",
                     "Fecha respuesta CxP|", --Archivos
                     "Estado|",
                     "Referencia|",
                     "Fecha|",               --Pago
                     "Parentesco|",
                     "Ap. Paterno|",
                     "Ap. Materno|",
                     "Nombre|",
                     "AIVs 92|",
                     "AIVs 97|",
                     "AIVs Total|",
                     "Pesos 92|",
                     "Pesos 97|",
                     "Pesos Total|",
                     "Telefono|",
                     "Correo|",
                     "Porcentaje|",
                     "RFC|",
                     "CURP|",                --Beneficiarios
                     "Acreedor|",
                     "CLABE|",
                     "Cta. X Pagar|",
                     "Anio|",
                     "Bandera|",
                     "Acreedor|",
                     "Banco Inter|",
                     "Estado de pago|",      --Respuesta
                     "No. de credito|",
                     "Tipo de credito|",
                     "Fecha inicio de credito|",
                     "Estatus de credito|",
                     "Descripcion de estatus de credito|"   --Crédito
                     
   CALL v_ch_arch_ret_generico.write(v_s_detalle)

   
   -- se transfieren los datos al arreglo de despligue
   PREPARE sid_solicitudesdet FROM v_sql
   DECLARE cur_solicitudesdet CURSOR FOR sid_solicitudesdet
   FOREACH cur_solicitudesdet INTO v_r_despliegue.*,v_id_archivo_envio,v_id_archivo_respuesta,
   v_id_archivo_cancela_cxp,v_id_archivo_resp_cxp
      #DISPLAY v_indice

      SELECT nvl(cve_referencia,0)
      INTO v_r_despliegue.ref_dap
      FROM ret_pago_dap
      WHERE id_solicitud = v_r_despliegue.id_solicitud

      SELECT nombre_archivo, f_actualiza
      INTO v_r_despliegue.nombre_archivo_e,v_r_despliegue.fecha_archivo_e
      FROM ret_ctr_archivo_fico
      WHERE id_archivo = v_id_archivo_envio

      SELECT nombre_archivo, f_actualiza
      INTO v_r_despliegue.nombre_archivo_r,v_r_despliegue.fecha_archivo_r
      FROM ret_ctr_archivo_fico
      WHERE id_archivo = v_id_archivo_respuesta

      SELECT nombre_archivo, f_actualiza
      INTO v_r_despliegue.nombre_archivo_cc,v_r_despliegue.fecha_archivo_cc
      FROM ret_ctr_archivo_fico
      WHERE id_archivo = v_id_archivo_cancela_cxp

      SELECT nombre_archivo, f_actualiza
      INTO v_r_despliegue.nombre_archivo_rc,v_r_despliegue.fecha_archivo_rc
      FROM ret_ctr_archivo_fico
      WHERE id_archivo = v_id_archivo_resp_cxp

      SELECT gpo_ley73 
      INTO v_gpo_ley73
      FROM ret_ley73_generico
      WHERE id_solicitud = v_r_despliegue.id_solicitud

      CASE v_gpo_ley73
         WHEN 1 LET v_r_despliegue.gpo_ley73 = "GRUPO 1" EXIT CASE
         WHEN 2 LET v_r_despliegue.gpo_ley73 = "GRUPO 2" EXIT CASE
         WHEN 3 LET v_r_despliegue.gpo_ley73 = "GRUPO 3" EXIT CASE
         WHEN 4 LET v_r_despliegue.gpo_ley73 = "GRUPO 4" EXIT CASE
         OTHERWISE LET v_r_despliegue.gpo_ley73 = ""
      END CASE

      SELECT cve_afore,curp
      INTO v_r_despliegue.cve_afore,v_r_despliegue.curp
      FROM ret_ws_sol_retiro_vent_afore
      WHERE id_solicitud = v_r_despliegue.id_solicitud

      SELECT curp
      INTO v_r_despliegue.curp_afi
      FROM afi_derechohabiente
      WHERE id_derechohabiente = v_r_despliegue.id_derechohabiente

      SELECT ef.des_estado, rw.rsp_referencia, rw.rsp_f_pago, rw.documento
      INTO v_r_despliegue.des_estado, v_r_despliegue.rsp_referencia,
      v_r_despliegue.rsp_f_pago, v_r_despliegue.cta_x_pagar
      FROM ret_ws_consulta_pago_fico AS rw, (SELECT rrr.id_solicitud, MAX(rrr.f_consulta) AS fecha_cons
                                             FROM ret_ws_consulta_pago_fico rrr
                                             WHERE rrr.id_solicitud = v_r_despliegue.id_solicitud
                                             GROUP BY rrr.id_solicitud) AS rw_max, ret_cat_edo_pago_fico ef
      WHERE rw.id_solicitud = rw_max.id_solicitud
      AND rw.f_consulta = rw_max.fecha_cons
      AND rw.rsp_estatus IN (2,3,4,20,21,22,23,24)
      AND rw.rsp_estatus = ef.estado_pago
      AND rw.id_solicitud = v_r_despliegue.id_solicitud
      
      IF v_r_despliegue.modalidad_retiro = 3 AND v_r_despliegue.gpo_ley73 = 1
      AND v_r_despliegue.grupo_ventanilla = 101 THEN
         --Corresponde a Ventanilla infonavit del Grupo 1, modalidad 3 (Ley 73)
         LET v_curp            = v_r_despliegue.curp
      ELSE
         --Cualquier otro toma del maestro de afiliados de INFONAVIT
         LET v_curp            = v_r_despliegue.curp_afi
      END IF

      --obteción de datos extras, nombre de la persona
      ## Se dejo el query de esta manera, porque de la forma anterior consumia mucho tiempo de ejecución,
      ## y se pidió revisar porque tardaban las consultas.
      
      SELECT TRIM(ap_paterno_af) || ' ' || TRIM(ap_materno_af)|| ' ' || TRIM(nombre_af)
      INTO v_r_despliegue.nombre
      FROM afi_derechohabiente
      WHERE id_derechohabiente = v_r_despliegue.id_derechohabiente
      IF v_r_despliegue.modalidad_retiro = 2 AND length(v_r_despliegue.nombre) < 1  THEN
        SELECT nombre
        INTO   v_r_despliegue.nombre
        FROM   afi_fondo72
        WHERE  id_derechohabiente = v_r_despliegue.id_derechohabiente
        AND    ind_estado_cuenta  = 0
      END IF
      
      --Se obtiene la fecha de actualizacion
      SELECT MAX(f_peticion)
      INTO v_r_despliegue.f_autorizacion
      FROM ret_ws_peticion_marca rp, ret_ws_det_peticion_marca rd
      WHERE rp.id_peticion = rd.id_peticion
      AND ind_marca = 3 
      AND caso_adai = v_r_despliegue.caso_adai

      IF v_r_despliegue.f_autorizacion IS NULL
      OR v_r_despliegue.f_autorizacion = "" THEN
          LET v_r_despliegue.f_autorizacion = v_r_despliegue.f_solicitud
      END IF

      --DISPLAY "obteción de datos extras de acuerdo a laa tabla que depende de la modalidad"
      --obteción de datos extras de acuerdo a laa tabla que depende de la modalidad
      --total de aivs y pesos
      CASE v_r_despliegue.modalidad_retiro
         WHEN 10
            SELECT total_aivs,total_importe,0,0,0,0,0,0
            INTO v_r_despliegue.aivs,v_r_despliegue.pesos,
                 v_r_despliegue.monto,v_r_despliegue.tanto_adicional,
                 v_r_despliegue.aivs_92,v_r_despliegue.pesos_92,
                 v_r_despliegue.aivs_97,v_r_despliegue.pesos_97
            FROM ret_voluntaria
            WHERE id_solicitud = v_r_despliegue.id_solicitud
         WHEN 3
            SELECT aivs_viv92+aivs_viv97+importe_viv97_anexo1,importe_viv92+importe_viv97+importe_viv97_anexo1,0,0,
                   aivs_viv92,importe_viv92,
                   aivs_viv97+importe_viv97_anexo1,importe_viv97+importe_viv97_anexo1
            INTO v_r_despliegue.aivs,v_r_despliegue.pesos,
                 v_r_despliegue.monto,v_r_despliegue.tanto_adicional,
                 v_r_despliegue.aivs_92,v_r_despliegue.pesos_92,
                 v_r_despliegue.aivs_97,v_r_despliegue.pesos_97
            FROM ret_ley73_generico
            WHERE id_solicitud = v_r_despliegue.id_solicitud
         WHEN 2
            SELECT 0,saldo_viv72+tanto_adicional, saldo_viv72, tanto_adicional,0,0,0,0
            INTO v_r_despliegue.aivs,v_r_despliegue.pesos,
                 v_r_despliegue.monto,v_r_despliegue.tanto_adicional,
                 v_r_despliegue.aivs_92,v_r_despliegue.pesos_92,
                 v_r_despliegue.aivs_97,v_r_despliegue.pesos_97
            FROM ret_fondo_ahorro_generico
            WHERE id_solicitud = v_r_despliegue.id_solicitud
         OTHERWISE--Para la 9
            SELECT total_aivs,total_importe,0,0,0,0,0,0
            INTO v_r_despliegue.aivs,v_r_despliegue.pesos,
                 v_r_despliegue.monto,v_r_despliegue.tanto_adicional,
                 v_r_despliegue.aivs_92,v_r_despliegue.pesos_92,
                 v_r_despliegue.aivs_97,v_r_despliegue.pesos_97
            FROM ret_amort_excedente
            WHERE id_solicitud = v_r_despliegue.id_solicitud
      END CASE


      INITIALIZE v_max_fec_pago,v_r_respuesta.* TO NULL
      
      SELECT MAX(fecha_pago)
      INTO v_max_fec_pago
      FROM ret_respuesta_fico
      WHERE referencia = v_r_despliegue.id_solicitud

      IF v_max_fec_pago IS NOT NULL THEN
         SELECT  FIRST 1 acreedor_res,cta_clabe,cta_x_pagar,anho,bandera,acreedor_res,banco_inter,
         des_error
         INTO v_r_respuesta.*
         FROM ret_respuesta_fico
         WHERE referencia = v_r_despliegue.id_solicitud
         AND fecha_pago = v_max_fec_pago
      END IF

      --Obtengo información del Crédito
      CALL fn_credito_vivienda(v_r_despliegue.id_derechohabiente, 1)
      RETURNING v_resultado,
                v_tpo_originacion,
                v_tpo_credito,
                v_num_credito,
                v_f_otorga,
                v_f_liquida

      INITIALIZE v_estado_credito TO NULL
      IF v_resultado = 0 OR v_resultado = 2 THEN
         --ejecuta consulta de creditos
         SELECT desc_credito                     
         INTO   v_desc_tipo_credito 
         FROM   cat_tipo_credito
         WHERE  tpo_originacion = v_tpo_originacion
         AND    tpo_credito = v_tpo_credito

         LET v_tipo_credito_desc = v_desc_tipo_credito

         --Determina si es crédito Vigente o Liquidado
         CASE
            WHEN v_resultado = 2
               LET v_estado_credito = "LIQUIDADO"
            WHEN v_resultado = 0
               LET v_estado_credito = "VIGENTE"
         END CASE

      ELSE 
         LET v_tipo_credito_desc = "SIN CREDITO"    
      END IF

      -- Se obtienen los beneficiarios
      LET v_sqlb ="\n SELECT                            ",
                  "\n a.consec_beneficiario            ,",
                  "\n a.tpo_beneficiario               ,",
                  "\n a.tpo_pago                       ,",
                  "\n a.cod_parentesco                 ,",
                  "\n a.ap_paterno                     ,",
                  "\n a.ap_materno                     ,",
                  "\n a.nombre                         ,",
                  "\n a.telefono                       ,",
                  "\n a.correo                         ,",
                  "\n a.porcentaje                     ,",
                  "\n a.aivs                           ,",
                  "\n a.importe                        ,",
                  "\n 0                                ,",
                  "\n 0                                ,",
                  "\n 0                                ,",
                  "\n 0                                ,",
                  "\n ''                               ,",
                  "\n ''                                ",
                  "\n FROM ret_beneficiario_generico  a ",
                  "\n WHERE a.id_solicitud =", v_r_despliegue.id_solicitud -- beneficiarios de la solicitud

      PREPARE sid_benefiriariosdet FROM v_sqlb
      DECLARE cur_benefiriariosdet CURSOR FOR sid_benefiriariosdet
   
      -- se transfieren los datos al arreglo de beneficiarios
      FOREACH cur_benefiriariosdet INTO v_r_beneficiario.*
         --Recalculo valores para 92 y 97 en base a lo encontrado, multiplicado por su factor
         LET v_r_beneficiario.aivs_92  = v_r_despliegue.aivs_92  * (v_r_beneficiario.porcentaje / 100)
         LET v_r_beneficiario.pesos_92 = v_r_despliegue.pesos_92 * (v_r_beneficiario.porcentaje / 100)
         LET v_r_beneficiario.aivs_97  = v_r_despliegue.aivs_97  * (v_r_beneficiario.porcentaje / 100)
         LET v_r_beneficiario.pesos_97 = v_r_despliegue.pesos_97 * (v_r_beneficiario.porcentaje / 100)
         --
         SELECT rfc_beneficiario,curp_beneficiario
         INTO v_r_beneficiario.rfc_benef, v_r_beneficiario.curp_benef
         FROM ret_ws_sol_retiro_vent_afore
         WHERE id_solicitud = v_r_despliegue.id_solicitud
         AND ind_beneficiario = v_r_beneficiario.consec_beneficiario
         --

         --Imprimo renglón con el detalle
         LET v_s_detalle = v_r_despliegue.id_solicitud,"|",
                           v_r_despliegue.desc_modalidad,"|",
                           v_r_despliegue.nss,"|",
                           v_r_despliegue.gpo_ley73,"|",
                           v_r_despliegue.cve_afore,"|",
                           fn_fecha(v_r_despliegue.rsp_f_pago),"|",
                           v_r_despliegue.aivs_92,"|",
                           v_r_despliegue.aivs_97,"|",
                           v_r_despliegue.aivs,"|",
                           v_r_despliegue.pesos_92,"|",
                           v_r_despliegue.pesos_97,"|",
                           v_r_despliegue.pesos,"|",
                           v_r_despliegue.caso_adai,"|",
                           fn_fecha(v_r_despliegue.f_solicitud),"|",
                           fn_fecha(v_r_despliegue.f_autorizacion),"|",
                           v_r_despliegue.monto,"|",
                           v_r_despliegue.tanto_adicional,"|",
                           v_r_despliegue.estado_solicitud,"|",
                           v_r_despliegue.cod_rechazo,"|",                       -- Solicitudes encontradas
                           v_r_despliegue.nombre,"|",
                           v_r_despliegue.folio,"|",
                           v_r_despliegue.rfc,"|",
                           v_r_despliegue.folio_restitucion,"|",
                           v_curp,"|",
                           v_r_despliegue.nombre_archivo_e,"|",
                           fn_fecha(v_r_despliegue.fecha_archivo_e),"|",
                           v_r_despliegue.nombre_archivo_r,"|",
                           fn_fecha(v_r_despliegue.fecha_archivo_r),"|",
                           v_r_despliegue.nombre_archivo_cc,"|",
                           fn_fecha(v_r_despliegue.fecha_archivo_cc),"|",
                           v_r_despliegue.nombre_archivo_rc,"|",
                           fn_fecha(v_r_despliegue.fecha_archivo_rc),"|",
                           v_r_despliegue.des_estado,"|",
                           v_r_despliegue.rsp_referencia,"|",
                           fn_fecha(v_r_despliegue.rsp_f_pago),"|",
                           v_r_beneficiario.cod_parentesco,"|",
                           v_r_beneficiario.ap_paterno,"|",
                           v_r_beneficiario.ap_materno,"|",
                           v_r_beneficiario.nombre,"|",
                           v_r_beneficiario.aivs_92,"|",
                           v_r_beneficiario.aivs_97,"|",
                           v_r_beneficiario.aivs,"|",
                           v_r_beneficiario.pesos_92,"|",
                           v_r_beneficiario.pesos_97,"|",
                           v_r_beneficiario.importe,"|",
                           v_r_beneficiario.telefono,"|",
                           v_r_beneficiario.correo,"|",
                           v_r_beneficiario.porcentaje,"|",
                           v_r_beneficiario.rfc_benef,"|",
                           v_r_beneficiario.curp_benef,"|",
                           v_r_respuesta.acreedor,"|",
                           v_r_respuesta.cta_clabe,"|",
                           v_r_respuesta.cta_x_pagar,"|",
                           v_r_respuesta.anho,"|",
                           v_r_respuesta.bandera,"|",
                           v_r_respuesta.acreedor_res,"|",
                           v_r_respuesta.banco_inter,"|",
                           v_r_respuesta.des_error,"|",
                           v_num_credito,"|",
                           v_tipo_credito_desc,"|",
                           v_f_otorga,"|",
                           v_tpo_credito,"|",
                           v_estado_credito,"|"
                           
         CALL v_ch_arch_ret_generico.write(v_s_detalle)
                           
      END FOREACH
      
      LET v_indice = v_indice + 1
                        
   END FOREACH
   
END FUNCTION

#Da formato a una fecha, el campo original es una cadena en fomato internacional aaaammdd y se regresa en formato México dd-mm-aaaa
FUNCTION fn_fecha(p_fecha_text)
   DEFINE
      p_fecha_text   VARCHAR(8),
      v_fecha        VARCHAR(20)
   IF LENGTH(p_fecha_text) > 0 THEN
      LET v_fecha = p_fecha_text[7,8],"-",p_fecha_text[5,6],"-",p_fecha_text[1,4]
   ELSE
      LET v_fecha = ""
   END IF
   RETURN v_fecha
END FUNCTION
