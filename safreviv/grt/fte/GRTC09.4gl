--====================================================================
-- Versión: 1.0.0
-- Fecha última modificación:
--====================================================================

######################################################################
#Módulo             => GRT                                           #
#Programa           => GRTC09                                        #
#Objetivo           => Programa que realiza la consulta de crédito   #
#                      Apoyo Infonavit (Crédito en Garantía 43bis)   #
#                      para usuarios de entidades financieras        #
#Autor              => Mauro Muñiz Caballero, EFP                    #
#Fecha inicio       => 21 de noviembre de 2018                       #
#Autor modifica     => Emilio Abarca, EFP.                           #
#Fecha modifica     => 13/Diciembre/2018.                            #
######################################################################

IMPORT OS

DATABASE safre_viv

GLOBALS

   DEFINE p_usuario_cod             CHAR(20)
   DEFINE p_tipo_ejecucion          SMALLINT 
   DEFINE p_s_titulo                STRING
   DEFINE p_rfc_usr_ef              CHAR(13)
   DEFINE p_cve_ent_financiera      SMALLINT
   DEFINE g_aviso                   CHAR(35)
   DEFINE v_sqlqry                  STRING
   DEFINE g_nss                     CHAR(11)
   DEFINE g_nss_unificado           CHAR(11)
   DEFINE g_indicador_tj            SMALLINT
   DEFINE g_accion_tj               CHAR(550)

   -- Arreglo global para marcas UG
   DEFINE g_arr_marca_ug DYNAMIC ARRAY OF RECORD
      codigo_resp                   SMALLINT,
      marca                         SMALLINT,
      marca_desc                    CHAR(40),
      f_marca                       DATE
   END RECORD

   -- Arreglo global para Pagos Usos de Garantía
   DEFINE g_arr_pago_ug DYNAMIC ARRAY OF RECORD
      codigo_resp                   SMALLINT,
      periodo_pago                  CHAR(6),
      importe                       DECIMAL(12,2),
      f_liquida                     DATE,
      estado_desc                   CHAR(60),
      accion                        CHAR(330)
   END RECORD

   -- Variables para salida del reporte
   DEFINE v_ruta_binaria            CHAR(40)
   DEFINE v_ruta_envio              CHAR(40)

END GLOBALS

GLOBALS "../../cta/fte/CTAW15.inc"    #archivo de variables globales del WS de consulta de saldo Procesar

MAIN

   -- Parámetros recibidos
   LET p_usuario_cod        = ARG_VAL(1)
   LET p_tipo_ejecucion     = ARG_VAL(2)
   LET p_s_titulo           = ARG_VAL(3)
   LET p_rfc_usr_ef         = ARG_VAL(4)
   LET p_cve_ent_financiera = ARG_VAL(5)

   #PRUEBA
   --LET  p_rfc_usr_ef = "OEAC8505141Y9"
   --LET  p_cve_ent_financiera = 12

   -- Inicializa variables
   LET g_aviso = "Consulta crédito apoyo infonavit"

   CLOSE WINDOW SCREEN

   -- se crea el archivo log
   CALL STARTLOG('/safreviv_log/grt/'||p_usuario_cod CLIPPED||'.GRTC09.log')

   -- si se obtuvo el título, se pone como título de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   SELECT ruta_bin,
          ruta_envio
     INTO v_ruta_binaria,
          v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'grt'

    -- Ejecuta función principal
    CALL fn_consulta_derechohabiente()

END MAIN

#Objetivo: Función que realiza la consulta por derechohabiente
FUNCTION fn_consulta_derechohabiente()

   DEFINE v_ax_error                SMALLINT
   DEFINE v_cod_rch_cons            SMALLINT
   DEFINE v_desc_resp_cons          CHAR(255)
   DEFINE v_nombre_dh               CHAR(123)
   DEFINE v_curp                    LIKE afi_derechohabiente.curp
   DEFINE v_rfc                     LIKE afi_derechohabiente.rfc
   DEFINE v_fallecido               SMALLINT
   DEFINE v_rl                      SMALLINT
   DEFINE v_sdo                     DECIMAL(12,2)
   DEFINE v_nss_unificador          CHAR(11)
   DEFINE v_desp_msj                STRING
   DEFINE v_ind_numero              SMALLINT
   DEFINE v_usuario_saci            SMALLINT
   DEFINE v_usuario_ef              SMALLINT
   DEFINE v_cve_ent_financiera      SMALLINT
   DEFINE v_estado_usuario          SMALLINT
   DEFINE v_msj_confirma            STRING
   DEFINE bnd_confirma              SMALLINT
   DEFINE v_msj_ventana             STRING
   DEFINE v_id_dh_unificador        DECIMAL(9,0);
   DEFINE v_diag_uni                SMALLINT;
   DEFINE v_ind_excepcion           SMALLINT

   OPEN WINDOW vtn1 WITH FORM "GRTC091"

      INPUT BY NAME g_nss ATTRIBUTES(UNBUFFERED,WITHOUT DEFAULTS)

         BEFORE INPUT
            LET g_nss  = NULL
            LET v_msj_confirma = "Está ingresando a un sistema que gestiona información propiedad del Instituto del Fondo Nacional de la\n Vivienda para los Trabajadores, por lo que su uso se encuentra sujeto a las leyes y normas aplicables.\n Toda actividad que realice será monitoreada y registrada."

            CALL fn_ventana_confirma("Alterta",v_msj_confirma,"stop") RETURNING bnd_confirma

            IF(bnd_confirma = 1)THEN
               CONTINUE INPUT
            ELSE
               EXIT PROGRAM
            END IF

         ON ACTION ACCEPT
            IF(g_nss IS NULL) THEN
               LET v_msj_ventana = "Se requiere capturar el NSS para consulta"

               CALL fn_mensaje(g_aviso,v_msj_ventana,"stop")
               NEXT FIELD g_nss
            ELSE 
               -- Valida campo
               IF(LENGTH(g_nss) < 11) THEN
               LET v_msj_ventana = "El NSS debe ser de 11 dígitos"

                  CALL fn_mensaje(g_aviso,v_msj_ventana,"stop")
                  NEXT FIELD g_nss
               END IF

               LET v_ind_numero = fn_es_numerico(g_nss)
               LET v_msj_ventana = "El NSS debe ser numérico"

               IF(v_ind_numero = 1) THEN
                  CALL fn_mensaje(g_aviso,v_msj_ventana, "about")
                  NEXT FIELD g_nss
               END IF

               -- Valida Acceso
               LET v_usuario_saci       = 0
               LET v_usuario_ef         = 0
               LET v_cve_ent_financiera = NULL
               LET v_estado_usuario     = NULL 

               IF(p_usuario_cod = 'SAFREVIV') THEN
                  LET p_rfc_usr_ef = "INFONAVIT"
                  LET p_cve_ent_financiera = 0
               ELSE
                  -- Verifica si el usuario es de SACI
                  SELECT COUNT(*)
                    INTO v_usuario_saci
                    FROM seg_usuario
                   WHERE usuario_cod = p_usuario_cod
                   
                 IF(v_usuario_saci > 0) THEN
                    LET p_rfc_usr_ef = "INFONAVIT"
                    LET p_cve_ent_financiera = 0
                 ELSE
                    -- Continúa validación del usuario externo EF
                    SELECT COUNT(*)
                      INTO v_usuario_ef
                      FROM cat_usuario_ef
                     WHERE rfc = p_rfc_usr_ef
                    
                    IF(v_usuario_ef = 0) THEN
                       LET v_msj_ventana = "EL USUARIO NO ESTÁ REGISTRADO"
                       CALL fn_mensaje(g_aviso,v_msj_ventana,"stop")
                       LET g_nss = NULL
                       NEXT FIELD g_nss
                    ELSE
                       -- Recupera datos de acceso
                       SELECT cve_ent_financiera,
                              estado
                         INTO v_cve_ent_financiera,
                              v_estado_usuario
                         FROM cat_usuario_ef
                        WHERE rfc = p_rfc_usr_ef

                        -- Valida estado
                       IF(v_estado_usuario <> 10) THEN
                          LET v_msj_ventana = "EL USUARIO ESTÁ DADO DE BAJA"

                          CALL fn_mensaje(g_aviso,v_msj_ventana,"stop")
                          LET g_nss = NULL
                          NEXT FIELD g_nss  
                       END IF 

                       -- Valida que sea la misma EF que se recibió como parámetro
                       IF(v_cve_ent_financiera <> p_cve_ent_financiera) THEN
                          LET v_msj_ventana = "EL USUARIO NO ESTÁ REGISTRADO EN LA ENTIDAD FINANCIERA "

                          CALL fn_mensaje(g_aviso,v_msj_ventana,"stop")
                          LET g_nss = NULL
                          NEXT FIELD g_nss 
                       END IF 
                       
                    END IF -- Valida usuario ef
                 END IF -- Valida usuario saci
               END IF
            END IF -- Valida nss

            LET v_ind_excepcion = 0
            
            # Al pasar por todas las validaciones verifica si es un acreditado Apoyo infonavit
            LET v_sqlqry = "EXECUTE FUNCTION fn_consulta_credito_gtia(?,?,?)"

            PREPARE prp_qry_cred FROM v_sqlqry
            EXECUTE prp_qry_cred USING p_rfc_usr_ef,
                                       p_cve_ent_financiera,
                                       g_nss
                                  INTO v_ax_error, 
                                       v_cod_rch_cons,
                                       v_desc_resp_cons,
                                       v_nombre_dh,
                                       v_curp,
                                       v_rfc,
                                       v_fallecido,
                                       v_rl,
                                       v_sdo,
                                       v_nss_unificador;

            LET v_desc_resp_cons = v_desc_resp_cons CLIPPED

            DISPLAY "error:  ", v_ax_error
            DISPLAY "código: ", v_cod_rch_cons
            DISPLAY "desc:   ", v_desc_resp_cons
            DISPLAY "nombre: ", v_nombre_dh
            DISPLAY "curp:   ",v_curp
            DISPLAY "rfc:    ",v_rfc
            DISPLAY "fall:   ",v_fallecido
            DISPLAY "rl:     ",v_rl
            DISPLAY "sdo:    ",v_sdo
            DISPLAY "unif:   ",v_nss_unificador

            IF (v_cod_rch_cons = 0) OR
               (v_cod_rch_cons = 9) OR
               (v_cod_rch_cons = 100) THEN

               IF v_ax_error = 9 THEN
                  LET g_nss_unificado = g_nss
                  LET g_nss           = v_nss_unificador

                  LET v_msj_confirma = "El NSS ingresado "||g_nss_unificado||" corresponde a un registro Unificado. \nSe desplegará la información del NSS que actualmente está vigente: "||v_nss_unificador

                  CALL fn_mensaje(g_aviso,v_msj_confirma,"stop")
               END IF

               -- Función que despliega información del acreditado
               CALL fn_consulta_acreditado(v_curp,
                                           v_rfc,
                                           v_rl,
                                           v_nombre_dh,
                                           v_fallecido,
                                           v_sdo,
                                           v_nss_unificador)
            ELSE
               -- Verifica si el nss fue unificado
               LET v_sqlqry = "EXECUTE FUNCTION fn_busca_nss_unificador(?)"

               PREPARE prp_unificado FROM v_sqlqry
               EXECUTE prp_unificado USING g_nss
                                     INTO v_nss_unificador,
                                          v_id_dh_unificador,
                                          v_diag_uni

               -- Si tiene unificación
               IF(v_diag_uni = 1) THEN
                   LET g_nss_unificado = g_nss
                   LET g_nss           = v_nss_unificador

                   -- Verifica el crédito Apoyo Infonavit
                   EXECUTE prp_qry_cred USING p_rfc_usr_ef,
                                              p_cve_ent_financiera,
                                              g_nss                 -- Nss unificador
                                         INTO v_ax_error, 
                                              v_cod_rch_cons,
                                              v_desc_resp_cons,
                                              v_nombre_dh,
                                              v_curp,
                                              v_rfc,
                                              v_fallecido,
                                              v_rl,
                                              v_sdo,
                                              v_nss_unificador;

                  IF (v_cod_rch_cons = 0) OR
                     (v_cod_rch_cons = 9) OR
                     (v_cod_rch_cons = 100) THEN
                     
                     LET v_msj_confirma = "El NSS ingresado "||g_nss_unificado||" corresponde a un registro Unificado. \nSe desplegará la información del NSS que actualmente está vigente: "||g_nss
                     CALL fn_mensaje(g_aviso,v_msj_confirma,"stop")

                     -- Función que despliega información del acreditado
                     CALL fn_consulta_acreditado(v_curp,
                                                 v_rfc,
                                                 v_rl,
                                                 v_nombre_dh,
                                                 v_fallecido,
                                                 v_sdo,
                                                 v_nss_unificador)
                  ELSE
                     -- Levanta bandera para mensaje de la excepcion
                     LET v_ind_excepcion = 1
                  END IF
               ELSE 
                  -- Levanta bandera para mensaje de la excepcion
                  LET v_ind_excepcion = 1
               END IF -- Unificación

               IF(v_ind_excepcion = 1) THEN

                  LET v_desc_resp_cons = v_desc_resp_cons CLIPPED  -- Elimina espacios

                  -- Descompone cadena
                  IF LENGTH(v_desc_resp_cons) > 60 THEN
                     CALL fn_mensaje_resp(v_desc_resp_cons) RETURNING v_desp_msj
                  ELSE
                     LET v_desp_msj = v_desc_resp_cons CLIPPED
                  END IF   

                  CALL fn_mensaje(g_aviso,v_desp_msj,"stop")
                  LET g_nss = NULL 

               END IF
            END IF

         ON ACTION CANCEL 
            EXIT INPUT

      END INPUT
      
   CLOSE WINDOW vtn1
   
END FUNCTION 

FUNCTION fn_es_numerico(p_cadena)

   DEFINE p_cadena   STRING
   DEFINE v_idx      INTEGER
   DEFINE indicador  BOOLEAN

   LET p_cadena = p_cadena CLIPPED

   FOR v_idx = 1 TO p_cadena.getLength()
      IF(p_cadena.subString(v_idx,v_idx) MATCHES '[0-9]') THEN
         LET indicador = 0
      ELSE
         LET indicador = 1
         EXIT FOR
      END IF
   END FOR

   RETURN indicador

END FUNCTION


FUNCTION fn_mensaje_resp(v_msj_alerta1)

   DEFINE a                   SMALLINT -- contador de linea para mensaje de alerta
   DEFINE b                   SMALLINT -- contador2 de linea para mensaje de alerta
   DEFINE v_msj_alerta        STRING   -- cadena con mensaje de alerta agregado por usuario
   DEFINE v_msj_alerta1       CHAR(255)-- variable para selección de alerta
   DEFINE v_msj_part1         STRING
   DEFINE v_msj_part2         STRING
   DEFINE v_msj_part3         STRING
   DEFINE v_msj_part4         STRING
   DEFINE v_msj_part5         STRING
   DEFINE buf base.StringBuffer

   LET v_msj_alerta = v_msj_alerta1

   LET buf = base.StringBuffer.create()
   CALL buf.append(v_msj_alerta)

   FOR a = 45 TO v_msj_alerta.getLength()
      CASE
         WHEN v_msj_alerta.getCharAt(a)= ' '
            LET v_msj_part1 = buf.subString(1,a)
            EXIT FOR
      END CASE
   END FOR

   FOR b = 90 TO v_msj_alerta.getLength()
      CASE 
         WHEN v_msj_alerta.getCharAt(b)= ' '
            LET v_msj_part2 = buf.subString(a,b)
            EXIT FOR
      END CASE
   END FOR

   FOR a = 135 TO v_msj_alerta.getLength()
      CASE 
         WHEN v_msj_alerta.getCharAt(a)= ' '
            LET v_msj_part3 = buf.subString(b,a)
            EXIT FOR
      END CASE
   END FOR

   FOR b = 180 TO v_msj_alerta.getLength()
      CASE 
         WHEN v_msj_alerta.getCharAt(b)= ' '
            LET v_msj_part4 = buf.subString(a,b)
            EXIT FOR
      END CASE
   END FOR

   FOR a = 225 TO v_msj_alerta.getLength()
      CASE 
         WHEN v_msj_alerta.getCharAt(a)= ' '
            LET v_msj_part5 = buf.subString(b,a)
            EXIT FOR
      END CASE
   END FOR

   LET v_msj_alerta = v_msj_part1 CLIPPED,"\n",
                      v_msj_part2 CLIPPED,"\n",
                      v_msj_part3 CLIPPED,"\n",
                      v_msj_part4 CLIPPED,"\n",
                      v_msj_part5 CLIPPED

   RETURN v_msj_alerta

END FUNCTION

# Objetivo: Muestra información del acreditado
FUNCTION fn_consulta_acreditado(r_parametros)

   -- Record para parámetros que recibe la función
   DEFINE r_parametros RECORD
      curp                          LIKE afi_derechohabiente.curp,
      rfc                           LIKE afi_derechohabiente.rfc,
      rl                            SMALLINT,
      nombre                        CHAR(123),
      fallecido                     SMALLINT,
      saldo                         DECIMAL(12,2),
      nss_unificador                CHAR(11)
   END RECORD

   DEFINE v_diagnostico             CHAR(3)
   DEFINE v_diag_desc               CHAR(70)
   DEFINE v_r_archivo               STRING
   DEFINE v_mjs_archivo             STRING
   DEFINE v_aux_rl                  CHAR(4)
   DEFINE w                         ui.Window
   DEFINE f                         ui.form

   OPEN WINDOW vtn2 WITH FORM "GRTC092"

      LET g_indicador_tj = 0    -- Inicia bandera indicando que no tiene TJ
      LET g_accion_tj    = NULL -- Acción en caso de tener la marca de tramite judicial
      
      -- Realiza consulta WS Procesar
      CALL fn_consulta_ws_procesar() RETURNING v_diagnostico,v_diag_desc

      -- Carga arreglos para marcas de Uso de Garantía y pagos UG
      CALL fn_llena_arreglos()

   DIALOG ATTRIBUTES(UNBUFFERED)

      -- Despliega arreglo de marcas UG
      DISPLAY ARRAY g_arr_marca_ug TO record_marcas.*
         BEFORE DISPLAY

            LET w = ui.Window.getCurrent()
            LET f = w.getForm()
            CALL f.setFieldStyle("w_accion","ConsultaUrl")
            
            IF(r_parametros.rl = 0) THEN
               LET v_aux_rl = "NO"
            ELSE 
               LET v_aux_rl = "SI"
            END IF

            -- Recupera acción en caso de tener Trámite judicial
            IF(g_indicador_tj = 1) THEN
               SELECT TRIM (desc_accion)||". "||TRIM(desc_sugerencia)
                 INTO g_accion_tj
                 FROM cat_accion_sugerida
                WHERE cod_accion = 14;
            END IF

            DISPLAY g_nss TO e_nss
            DISPLAY r_parametros.curp TO e_curp
            DISPLAY r_parametros.rfc  TO e_rfc
            DISPLAY r_parametros.nombre TO e_nombre
            DISPLAY v_aux_rl TO e_laboral
            DISPLAY "Crédito Apoyo Infonavit vigente" TO e_estado
            DISPLAY v_diag_desc TO e_estado_procesar
            DISPLAY "2 - Apoyo Infonavit" TO e_tpo_credito
            DISPLAY r_parametros.saldo TO e_saldo
            DISPLAY g_accion_tj TO w_accion

      END DISPLAY

      -- Arreglo Pagos UG
      DISPLAY ARRAY g_arr_pago_ug TO record_pagos.*
      END DISPLAY

      ON ACTION Reporte
         -- Valida si existen datos para el PDF
         IF(g_arr_pago_ug.getLength() >= 1) THEN
            -- Ejecuta función que genera PDF
            CALL fn_genera_pdf(r_parametros.nombre)
         ELSE 
            CALL fn_mensaje(g_aviso,"No existe información para el reporte PDF","about")
         END IF 

      ON ACTION Archivo
         -- Valida si existen datos para el archivo
         IF(g_arr_pago_ug.getLength() >= 1) THEN

            -- Ejecuta función que genera el archivo txt
            CALL fn_genera_archivo(r_parametros.nombre) RETURNING v_r_archivo

            LET v_mjs_archivo = "El archivo: ",v_r_archivo,"\n","se ha generado correctamente"
            CALL fn_mensaje("",v_mjs_archivo,"")
            
         ELSE 
            CALL fn_mensaje(g_aviso,"No existe información para generar el archivo","about")
         END IF 
      
      ON ACTION CLOSE
         LET g_nss = NULL 
         EXIT DIALOG

   END DIALOG 

   CLOSE WINDOW vtn2
   
END FUNCTION 

FUNCTION fn_consulta_ws_procesar()

   DEFINE v_w_paterno            CHAR(40)
   DEFINE v_w_materno            CHAR(40)
   DEFINE v_w_nombre             CHAR(40)
   DEFINE v_url_servidor         LIKE wsv_cliente.ruta_servidor
   DEFINE v_usuario              LIKE wsv_cliente.usuario
   DEFINE v_password             LIKE wsv_cliente.password
   DEFINE v_intentos             LIKE wsv_cliente.num_reintento
   DEFINE soapStatus             INTEGER
   DEFINE v_w_diagnostico        CHAR(3)
   DEFINE v_w_diag_desc          CHAR(70)

   # Obtiene datos de configuración del WS
   # La clave 'cre_3' del catalogo de clientes de webServices corresponde a la consulta de saldo
   SELECT ruta_servidor,
          usuario,
          password,
          num_reintento
     INTO v_url_servidor,
          v_usuario,
          v_password,
          v_intentos
     FROM wsv_cliente
    WHERE cve_cliente = 'cre_3'
   
   -- Obtiene inf. del derechohabiente
   SELECT ap_paterno_af,
          ap_materno_af,
          nombre_af
     INTO v_w_paterno,
          v_w_materno,
          v_w_nombre
    FROM afi_derechohabiente
   WHERE nss = g_nss;
   
   -- Se invoca a la función que ejecuta el web service
   CALL consultaSaldo(v_url_servidor CLIPPED,
                      v_usuario,
                      v_password,
                      v_w_materno CLIPPED,
                      v_w_paterno CLIPPED,
                      v_w_nombre  CLIPPED,
                      g_nss)
            RETURNING soapStatus,
                      ConsultaSaldoRespVO.apeMaternoBD,
                      ConsultaSaldoRespVO.apePaternoBD,
                      ConsultaSaldoRespVO.diagProceso,
                      ConsultaSaldoRespVO.nombresBD,
                      ConsultaSaldoRespVO.nss,
                      ConsultaSaldoRespVO.numAIVS92,
                      ConsultaSaldoRespVO.numAIVS97,
                      ConsultaSaldoRespVO.origenTipoCredito,
                      ConsultaSaldoRespVO.resultOperacion,
                      ConsultaSaldoRespVO.tramiteJudicial

   LET v_w_diagnostico = ConsultaSaldoRespVO.diagProceso
   
   IF(soapStatus = 0) THEN
      -- Búsca descripción del diagnostico
      SELECT desc_diag
        INTO v_w_diag_desc
        FROM cat_diag_ws_procesar
       WHERE diagnostico = v_w_diagnostico;
   ELSE 
      -- Ocurrió un error al consumir WS
      LET v_w_diag_desc = "-1"
   END IF 

   -- TRÁMITE JUDICIAL AFORES
   IF(v_w_diagnostico = "006") THEN
      LET g_indicador_tj = 1 -- Levanta bandera indicando que tiene TJ
   END IF

   -- Retorna diagnostico y descripción
   RETURN v_w_diagnostico, v_w_diag_desc
   
END FUNCTION 

FUNCTION fn_llena_arreglos()

   DEFINE v_id_derechohabiente   DECIMAL(9,0)
   DEFINE v_k                    INTEGER

   SELECT id_derechohabiente
     INTO v_id_derechohabiente
     FROM afi_derechohabiente
    WHERE nss = g_nss;

   --> Marcas Usos de Garantía
   LET v_sqlqry = "EXECUTE FUNCTION fn_valida_marcas_ug(?)"

   -- Se crea cursor para ejecutar la función, debido al WITH RESUME.
   PREPARE prp_marcas_ug FROM v_sqlqry 
   DECLARE crs_marcas_ug CURSOR FOR prp_marcas_ug

   CALL g_arr_marca_ug.clear()

   LET v_k = 1  -- Incializa contador

   FOREACH crs_marcas_ug USING v_id_derechohabiente 
                          INTO g_arr_marca_ug[v_k].codigo_resp,
                               g_arr_marca_ug[v_k].marca,
                               g_arr_marca_ug[v_k].marca_desc,
                               g_arr_marca_ug[v_k].f_marca

      IF(g_arr_marca_ug[v_k].marca_desc = "TRÁMITE JUDICIAL") THEN
         LET g_indicador_tj = 1 -- Levanta bandera indicando que tiene TJ
      END IF
      
      LET v_k = v_k + 1
      
   END FOREACH

   -- Elimina fila en blanco 
   IF(g_arr_marca_ug[g_arr_marca_ug.getLength()].codigo_resp = 0) OR 
     (g_arr_marca_ug[g_arr_marca_ug.getLength()].codigo_resp IS NULL) THEN
     CALL g_arr_marca_ug.deleteElement(g_arr_marca_ug.getLength())
   END IF

   --> Pagos Usos de Garantía
   LET v_sqlqry = "EXECUTE FUNCTION fn_consulta_pagos_ug(?)"

   -- Se crea cursor para ejecutar la función, debido al WITH RESUME.
   PREPARE prp_pagos_ug FROM v_sqlqry 
   DECLARE crs_pagos_ug CURSOR FOR prp_pagos_ug

   CALL g_arr_pago_ug.clear()

   LET v_k = 1

   FOREACH crs_pagos_ug USING v_id_derechohabiente
                         INTO g_arr_pago_ug[v_k].codigo_resp,
                              g_arr_pago_ug[v_k].periodo_pago,
                              g_arr_pago_ug[v_k].importe,
                              g_arr_pago_ug[v_k].f_liquida,
                              g_arr_pago_ug[v_k].estado_desc,
                              g_arr_pago_ug[v_k].accion

                              
      -- Resetea montos negativos a valores positivos
      IF(g_arr_pago_ug[v_k].importe < 0) THEN
         LET g_arr_pago_ug[v_k].importe = g_arr_pago_ug[v_k].importe * -1
      END IF
      
      LET v_k = v_k + 1

   END FOREACH

   -- Elimina la última fila en blanco
   IF(g_arr_pago_ug[g_arr_pago_ug.getLength()].codigo_resp IS NULL) THEN
     CALL g_arr_pago_ug.deleteElement(g_arr_pago_ug.getLength())
   END IF

   -- Validación en caso de que el derechohabiente tenga marca TJ y no tenga pagos registrados
   -- no debe mostrar leyendas en la sección de pagos (Esta leyenda queda en la primera fila)
   IF(g_indicador_tj = 1) AND
     (g_arr_pago_ug[g_arr_pago_ug.getLength()].codigo_resp = 0) THEN
     -- Limpia arreglo de pagos.
     CALL g_arr_pago_ug.clear()
   END IF 
   
END FUNCTION 

FUNCTION fn_genera_archivo(p_nombre_dh)

   DEFINE p_nombre_dh      CHAR(123)
   DEFINE object_arh       base.channel
   DEFINE v_det_encabezado STRING
   DEFINE v_detalle        STRING
   DEFINE v_nombre_archivo STRING
   DEFINE v_salida_archivo STRING
   DEFINE v_c              INTEGER
   DEFINE v_aux_importe    CHAR(15)
   DEFINE v_s_comando      STRING 
   

   -- Nombre del archivo
   LET v_nombre_archivo =  "Reporte_pagos_",p_rfc_usr_ef CLIPPED,"_",TODAY USING "yyyymmdd",".txt"
   
   -- Ruta para escritura del archivo
   LET v_salida_archivo = v_ruta_envio CLIPPED,"/",v_nombre_archivo

   -- Crea objeto
   LET object_arh = base.Channel.create()

   CALL object_arh.openFile(v_salida_archivo,"w")

   -- Encabezado
   LET v_det_encabezado = g_nss,"_",TODAY USING "yyyymmdd"," ",p_nombre_dh CLIPPED

   -- Escribe el encabezado
   CALL object_arh.writeLine([v_det_encabezado])

   LET v_detalle = NULL
   LET v_aux_importe = NULL

   FOR v_c = 1 TO g_arr_pago_ug.getLength()

      LET v_aux_importe = g_arr_pago_ug[v_c].importe

      LET v_detalle = v_aux_importe CLIPPED,"|",
                      g_arr_pago_ug[v_c].f_liquida CLIPPED USING "dd/mm/yyyy","|",
                      g_arr_pago_ug[v_c].estado_desc CLIPPED,"|",
                      g_arr_pago_ug[v_c].accion CLIPPED,"|"

      CALL object_arh.writeLine(v_detalle)

   END FOR

   -- Cierra archivo de escritura
   CALL object_arh.close()

   -- Transfiere archivo SERVER -> WORKSTATION del usuario externo
   CALL FGL_PUTFILE(v_salida_archivo,v_nombre_archivo)

   -- Elimina archivo del servidor
   LET v_s_comando = "rm ",v_salida_archivo
   RUN v_s_comando

   RETURN v_nombre_archivo

END FUNCTION

FUNCTION fn_genera_pdf(p_nombre_acre)

   DEFINE v_reporte_bin      STRING
   DEFINE v_ruta_rpt         STRING
   DEFINE object_rpt         om.SaxDocumentHandler
   DEFINE p_nombre_acre      CHAR(123)


   --> CONFIGURACIÓN DE LA SALIDA REPORTE PDF <--
   
   LET v_reporte_bin = v_ruta_binaria CLIPPED,"/GRTC093.4rp"
   LET v_ruta_rpt    = "Reporte_pagos_",p_rfc_usr_ef CLIPPED,".pdf"

   IF (fgl_report_loadCurrentSettings(v_reporte_bin)) THEN 
      CALL fgl_report_selectDevice ("PDF")
      CALL fgl_report_selectPreview(1) -- Se muestra en automático en el browser
      CALL fgl_report_setOutputFileName(v_ruta_rpt)
      LET object_rpt = fgl_report_commitCurrentSettings()

      IF (object_rpt IS NOT NULL) THEN
         
         START REPORT descarga_pdf TO XML HANDLER object_rpt

            OUTPUT TO REPORT descarga_pdf(p_nombre_acre)

         FINISH REPORT descarga_pdf

      END IF
   ELSE
      CALL fn_mensaje("","No fué posible abrir la platilla del reporte","")
   END IF
   
END FUNCTION 

REPORT descarga_pdf(p_nombre_rpt)

   DEFINE v_fecha        DATE
   DEFINE p_nombre_rpt   CHAR(123)
   DEFINE v_f            INTEGER

   FORMAT
      FIRST PAGE HEADER
         LET v_fecha = TODAY

         # Encabezado
         PRINTX p_rfc_usr_ef
         PRINTX v_fecha USING "dd/mm/yyyy"
         PRINTX g_nss
         PRINTX p_nombre_rpt

      ON EVERY ROW
         -- Despliega arreglo de pagos
         FOR v_f = 1 TO g_arr_pago_ug.getLength()
            PRINTX g_arr_pago_ug[v_f].importe
            PRINTX g_arr_pago_ug[v_f].f_liquida USING "dd/mm/yyyy"
            PRINTX g_arr_pago_ug[v_f].estado_desc
            PRINTX g_arr_pago_ug[v_f].accion
         END FOR 

END REPORT 