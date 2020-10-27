--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
###############################################################################
#PROYECTO          => SAFRE FONDO DE AHORRO                                   #
-------------------------------------------------------------------------------
#MODULO            => RET                                                     #
#PROGRAMA          => RETS19-168                                              #
#OBJETIVO          => Programa de Apoyo para dar de alta solicitudes          #
#                     del Fondo de Ahorro para el pago MASIVO                 #
#REQUERIMIENTO     => SACI2019-168                                            #
###############################################################################

 
DATABASE safre_viv 
# 
# USER GLOBALS VARIABLES
#
GLOBALS "ret_ws_disponibilidad_fa.inc"
GLOBALS "ret_ws_marca_fa.inc"
GLOBALS "ret_ws_crea_solicitud_fa.inc"

GLOBALS "RETG01.4gl"
GLOBALS
-- registro de disponibilidad

DEFINE rec_disponibilidad_in RECORD
         nss              STRING, -- nss del trabajador
         rfc              STRING, -- rfc del trabajador
         causal_ret_fa    STRING, -- causal del retiro de fondo de ahorro
         nrp              STRING, -- NRP, usado para plan privado de pension en ret FA
         f_inicio_pension STRING, -- fecha de inicio de pension en formato AAAAMMDD
         medio_entrega    STRING  -- Medio por el cual se hace la consulta 1 - Tableta, 0 - Otros
      END RECORD

DEFINE rec_disponibilidad_out  RECORD
         nss                STRING, --- Número de seguridad social del trabajador
         rfc                STRING, -- rfc del trabajador
         estado_solicitud   STRING, -- estado de la solicitud
         cod_rechazo        STRING, -- codigo de rechazo
         des_rechazo        STRING,    -----  *****************************************
         monto_pesos        STRING, -- saldo en pesos equivalente a AIVs por valor accion
         monto_adicional    STRING, -- monto del tanto adicional
         monto_total        STRING, -- monto total a devolver
         pago_dap           STRING   -- se agrega este dato para indicar so debe o no pagar por DAP 1-Candidato a pago via DAP, 2-No es candidato a pago via DAP
      END RECORD

-- registro de marca

DEFINE rec_marca_in RECORD 
         nss            STRING,
         rfc            STRING,
         caso_crm       STRING, -- caso crm
         cuenta_clabe   STRING,   -------     *************************
         ind_marca      STRING, -- indicador de marca/desmarca/aprobacion/rechazo
         cod_rechazo    STRING, -- codigo de rechazo en caso de desmarca y rechazo
         medio_entrega  STRING  -- Indica el grupo que se marcará
       END RECORD

DEFINE rec_marca_out RECORD
         nss          STRING,
         rfc          STRING,
         est_marca    STRING,
         con_retiro   STRING,
         caso_crm     STRING,
         cod_rechazo  STRING,
         des_rechazo  STRING, ------ *********************************
         saldo_pesos  STRING,  
         cve_dap      STRING    -- este campo solo tendra valor cuando sea fondo de ahorro y el monto de retiro sea menor a 1000 pesos
       END RECORD

-- registro de creación de la solicitud

DEFINE rec_crea_solicitud_in RECORD
         nss              STRING, -- nss del trabajador
         rfc              STRING, -- rfc del trabajador
         caso_crm         STRING, -- numero de caso CRM
         causal_retiro    STRING,
         nrp              STRING,
         f_inicio_pension STRING, -- fecha de inicio de pension
         medio_entrega    STRING,
         arr_beneficiario DYNAMIC ARRAY OF RECORD
              tipo_beneficiario  STRING,
              clabe_bancaria     STRING,
              rfc                STRING,
              email              STRING,
              telefono           STRING,
              tel_movil          STRING,
              nombre             STRING,
              ap_paterno         STRING,
              ap_materno         STRING,
              entidad_federativa STRING
         END RECORD
       END RECORD

DEFINE rec_crea_solicitud_out  RECORD
            nss                  STRING, --- Número de seguridad social del trabajador
            rfc                  STRING, -- rfc del trabajador
            caso_crm             STRING,
            estado_solicitud     STRING, -- estado de la solicitud
            cod_rechazo          STRING, -- codigo de rechazo
            des_rechazo          STRING,   ---- *************************************++
            monto_pesos          STRING, -- saldo en pesos 
            monto_adicional      STRING, -- monto adicional a devolver en pesos 
            monto_total          STRING, -- monto total a devolver en pesos
            referencia_dap       STRING -- referencia cuando es pago por DAP
       END RECORD

       
DEFINE g_indice_retiro      SMALLINT, -- indice del tipo de retiro consultado
       g_id_derechohabiente DECIMAL(9,0) ,
       g_id_fondo72         DECIMAL(9,0) ,
       g_causal_ref         SMALLINT     ,
       g_nss                CHAR(11)     ,
       g_rfc                CHAR(13)     , -- rfc del trabajador
       g_acc_acciones       DECIMAL(14,6),
       g_acc_pesos          DECIMAL(14,6),
       g_tanto_adicional    DECIMAL(14,6),
       g_id_solicitud       DECIMAL(9,0) ,
       g_refer              CHAR(18)     ,
       g_id_beneficiario    SMALLINT     , -- Identificador de beneficiario (si aplica)
       g_nombre             CHAR(18)     , -- Nombre del beneficiario 
       g_ape_pat            CHAR(18)     , -- Apellido paterno 
       g_ape_mat            CHAR(18)     , -- Apellido materno           
       g_causal_adai        SMALLINT     , -- Clave de Adai 
       g_entidad            SMALLINT     , -- Entidad federativa
       g_id_datamart        DECIMAL(9,0) , -- Identificador datamart
       g_causal_retiro      SMALLINT     ,
       g_bnd_uso_seq        SMALLINT     ,
       g_sq_ret_solicitud   DECIMAL(9,0) -- id de solicitud nueva 

DEFINE g_r_tmp_id_fondo72   RECORD
        nss                  CHAR(11)     ,
        id_derechohabiente   DECIMAL(9,0) ,
        id_afi_fondo72       DECIMAL(9,0) ,
        importe              DECIMAL(12,2),
        rfc                  CHAR(13)     ,
        estatus              SMALLINT     ,
        rechazo_cod          SMALLINT
       END RECORD

DEFINE g_id_peticion    DECIMAL(9,0) -- id de la peticion al ws
       
-- =======================================================
-- constantes para la evaluacion del resultado de la ejecucion del webservice
CONSTANT  g_res_procesada                    SMALLINT = 0  ,
          g_res_sin_solicitud                SMALLINT = -1 ,
          g_res_desconectado_del_servidor    SMALLINT = -2 ,
          g_res_conexion_con_cliente_perdida SMALLINT = -3 ,
          g_res_servidor_interrumpido_ctrl_c SMALLINT = -4 ,
          g_res_error_interno                SMALLINT = -10,
          g_msg_procesada                    STRING = "Solicitud procesada"                  ,
          g_msg_sin_solicitud                STRING = "Sin solicitud"                        ,
          g_msg_desconectado_del_servidor    STRING = "Desconectado del servidor"            ,
          g_msg_conexion_con_cliente_perdida STRING = "Se perdió la conexión con el cliente" ,
          g_msg_servidor_interrumpido_ctrl_c STRING = "Se interrumpió el servidor con CTRL-C",
          g_msg_error_interno                STRING = "Ocurrió un error interno"
         
DEFINE serverURL      STRING -- URL del servidor
DEFINE v_pantalla     SMALLINT
DEFINE g_pid          LIKE bat_ctr_proceso.pid,     --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_usuario      LIKE seg_usuario.usuario_cod, -- clave del usuario
       g_reg_modulo   RECORD
        ruta_exp         CHAR(40),
        ruta_rescate     CHAR(40),
        ruta_listados    CHAR(40)
       END RECORD,
       seg_modulo_bat RECORD
        ruta_listados    CHAR(40)
       END RECORD
DEFINE g_nombre_archivo  STRING        
END GLOBALS

#
# MAIN
#
MAIN
DEFINE v_contador       INTEGER
DEFINE v_resultado      SMALLINT
DEFINE v_comando        STRING 

   LET g_proceso_cod = g_proceso_cod_fondo_ahorro_masivo  -- Carga Archivo Masivo Fondo de Ahorro
   LET g_opera_cod   = g_opera_fondo_ahorro_masivo        -- Operación de carga de archivo para trámite de la devolución del fondo de ahorro MASIVO
   LET g_usuario     = "OPSISSACI"

   LET g_nombre_archivo = ARG_VAL(1)
   
   CALL fn_inicia_proceso(g_nombre_archivo) RETURNING v_resultado

   IF v_resultado = 0 THEN  
      DISPLAY "*************************************************************************************************"
      DISPLAY "**"
      DISPLAY "**"
      DISPLAY "**"
      DISPLAY CURRENT YEAR TO MINUTE, " Inicia proceso de carga de información de Solicitudes del Fondo de Ahorro de forma MASIVA "
      DISPLAY "**"
      DISPLAY "**"
      CALL fn_crea_temporal()
      DISPLAY CURRENT YEAR TO MINUTE, " Tablas temporales creadas "
      -- se asigna proceso y operacion
      CALL fn_carga_archivo(g_nombre_archivo)

      SELECT COUNT(*) 
      INTO   v_contador
      FROM   tmp_fondo72

      IF v_contador > 0 THEN 
         DISPLAY CURRENT YEAR TO MINUTE, " Se cargaron a la temporal ", v_contador, " registros del Archivo ", g_nombre_archivo CLIPPED
         DISPLAY " "
         DISPLAY " "   
         DISPLAY CURRENT YEAR TO MINUTE, " Comienza el proceso "
         DISPLAY " "
         DISPLAY " "   
         DISPLAY CURRENT YEAR TO MINUTE, " Se valida que no haya NSS's duplicados en el archivo "
         DISPLAY " "
         CALL fn_valida_duplicidad_nss(0,'') RETURNING v_resultado    
         IF v_resultado <> 0 THEN
             DISPLAY " "   
             DISPLAY CURRENT YEAR TO MINUTE, " Se detectan NSS's repetidos, estos no serán procesados, serán incluidos en el archivo de rechazos "
             DISPLAY " "
         END IF 
         DISPLAY " "   
         DISPLAY CURRENT YEAR TO MINUTE, " Se valida que no haya CLABE's duplicadas en el archivo "
         DISPLAY " "
         CALL fn_valida_duplicidad_clabe(0,'') RETURNING v_resultado
         IF v_resultado <> 0 THEN
            DISPLAY " "
            DISPLAY CURRENT YEAR TO MINUTE, " Se detectan CLABE's repetidas, estas no serán procesadas, serán incluidas en el archivo de rechazos "
            DISPLAY " "
         END IF 
         DISPLAY " "
         DISPLAY CURRENT YEAR TO MINUTE, " Procesando Archivo "
         DISPLAY " "
         CALL fn_procesa_informacion()
         DISPLAY " "
         DISPLAY " "
         DISPLAY CURRENT YEAR TO MINUTE, " Descargando aceptados y rechazados "
         DISPLAY " "
         CALL fn_descarga_temporales()
         DISPLAY CURRENT YEAR TO MINUTE, " Termina proceso "
      ELSE 
         DISPLAY CURRENT YEAR TO MINUTE, " No existe información por procesar "
      END IF 
      CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod) RETURNING v_resultado
   ELSE
      DISPLAY "Error no se pudo incializar el proceso"
   END IF 
   -- mueve el log al monitor de procesos
   LET v_comando = "cp /safreviv_req/SACI2019-168/log.out /safreviv_lst/bat/finnohup:",g_pid USING "&&&&&",":",g_proceso_cod USING "&&&&&",":",g_opera_cod USING "&&&&&"
--   DISPLAY " el comando ", v_comando
   RUN v_comando
   
END MAIN 

FUNCTION fn_procesa_informacion()       
DEFINE v_resultado           INTEGER -- recibe el resultado de la ejecucion del servicio 
DEFINE v_contador            INTEGER 

DEFINE rec_solicitud RECORD 
         nss             CHAR(11),
         curp            CHAR(18),
         nombre_af       CHAR(40),
         ap_paterno_af   CHAR(40),
         ap_materno_af   CHAR(40),
         f_nacimiento    CHAR(10),
         sexo            CHAR(1),
         cuenta_clabe    CHAR(18),
         tpo_pension     CHAR(2),
         grupo_familiar  CHAR(2)
END RECORD 

DEFINE v_marca_entra    SMALLINT 
DEFINE v_proceso_cod    SMALLINT
DEFINE v_folio          DECIMAL(9,0)
DEFINE v_estado_marca   SMALLINT
DEFINE v_codigo_rechazo SMALLINT
DEFINE v_marca_causa    SMALLINT
DEFINE v_fecha_causa    DATE
DEFINE v_usuario        CHAR(20)
DEFINE v_rfc              CHAR(13)


DEFINE v_r_ret_solicitud_generico RECORD LIKE ret_solicitud_generico.* -- registro de solicitud

    LET v_marca_entra    = 802
    LET v_proceso_cod    = g_proceso_cod
    LET v_folio          = "0"
    LET v_estado_marca   = "0"
    LET v_codigo_rechazo = "0"
    LET v_marca_causa    = "0"
    LET v_fecha_causa    = NULL
    LET v_usuario        = "OPSISSACI"

   DECLARE cur_procesa_solicitud CURSOR FOR
   SELECT * 
   FROM   tmp_fondo72

   -- se leen las solicitudes de estos casos
   FOREACH cur_procesa_solicitud INTO rec_solicitud.*
      LET v_contador = v_contador + 1
      IF v_contador MOD 100 = 0 THEN 
         DISPLAY CURRENT YEAR TO MINUTE, " Registros procesados : ", v_contador
      END IF
      INITIALIZE rec_disponibilidad_in  TO NULL
      INITIALIZE rec_disponibilidad_out TO NULL
      INITIALIZE rec_marca_in  TO NULL
      INITIALIZE rec_marca_out TO NULL
      INITIALIZE rec_crea_solicitud_in  TO NULL
      INITIALIZE rec_crea_solicitud_out TO NULL
      INITIALIZE v_rfc TO NULL 
      
      CALL fn_valida_duplicidad_nss(1,rec_solicitud.nss) RETURNING v_resultado
      IF v_resultado = 0 THEN  
         CALL fn_valida_duplicidad_clabe(1,rec_solicitud.cuenta_clabe) RETURNING v_resultado
         IF v_resultado = 0 THEN
            LET rec_disponibilidad_in.nss = rec_solicitud.nss
            CALL fn_busca_rfc(rec_solicitud.nss) RETURNING v_resultado, v_rfc
            IF v_resultado = 0 THEN 
               LET rec_disponibilidad_in.rfc = v_rfc
               LET rec_disponibilidad_in.causal_ret_fa = 2
               LET rec_disponibilidad_in.medio_entrega = 7
               LET rec_disponibilidad_in.f_inicio_pension = ""
               LET rec_disponibilidad_in.nrp = ""
               CALL fn_disponibilidad () RETURNING v_resultado -- se invoca al servicio de disponibilidad
               IF v_resultado = 0 THEN 
                  IF rec_disponibilidad_out.estado_solicitud = 10 THEN -- ACEPTADA
                     LET rec_marca_in.nss = rec_solicitud.nss
                     LET rec_marca_in.rfc = v_rfc
                     LET rec_marca_in.ind_marca = 1
                     LET rec_marca_in.medio_entrega = 7
                     LET rec_marca_in.cuenta_clabe = rec_solicitud.cuenta_clabe
                     LET rec_marca_in.caso_crm = " "
                     LET rec_marca_in.cod_rechazo = 0
                     CALL fn_marca() RETURNING v_resultado
                     IF v_resultado = 0 THEN 
                        IF rec_marca_out.est_marca = 1  THEN  -- ACEPTADA
                           --  Se invoca al de Crea Solicitud
                           LET rec_crea_solicitud_in.nss = rec_solicitud.nss
                           LET rec_crea_solicitud_in.rfc = v_rfc
                           LET rec_crea_solicitud_in.causal_retiro = 2
                           LET rec_crea_solicitud_in.medio_entrega = 7
                           LET rec_crea_solicitud_in.caso_crm = " "
                           LET rec_crea_solicitud_in.f_inicio_pension = ""
                           LET rec_crea_solicitud_in.nrp = ""
                           LET rec_crea_solicitud_in.arr_beneficiario[1].ap_paterno = rec_solicitud.ap_paterno_af
                           LET rec_crea_solicitud_in.arr_beneficiario[1].clabe_bancaria = rec_solicitud.cuenta_clabe
                           LET rec_crea_solicitud_in.arr_beneficiario[1].nombre = rec_solicitud.nombre_af
                           LET rec_crea_solicitud_in.arr_beneficiario[1].entidad_federativa = "09"
                           IF rec_solicitud.tpo_pension = "VI" OR 
                              rec_solicitud.tpo_pension = "VO" OR
                              rec_solicitud.tpo_pension = "AS" OR 
                              rec_solicitud.tpo_pension = "OR" THEN 
                              LET rec_crea_solicitud_in.arr_beneficiario[1].tipo_beneficiario = 2
                           ELSE
                              LET rec_crea_solicitud_in.arr_beneficiario[1].tipo_beneficiario = 1
                           END IF
                           LET rec_crea_solicitud_in.arr_beneficiario[1].ap_materno = rec_solicitud.ap_materno_af
                           LET rec_crea_solicitud_in.arr_beneficiario[1].email = ""
                           LET rec_crea_solicitud_in.arr_beneficiario[1].rfc = v_rfc
                           LET rec_crea_solicitud_in.arr_beneficiario[1].tel_movil = ""
                           LET rec_crea_solicitud_in.arr_beneficiario[1].telefono = ""
                           CALL fn_crea_solicitud() RETURNING v_resultado
                           IF v_resultado = 0 THEN 
                              IF rec_crea_solicitud_out.estado_solicitud = 10 THEN 
                                 -- Se manda actualizar el estado de la solicitud a 15 para que inicie el proceso de pago
                                 CALL fn_actualiza_solicitud(rec_marca_out.con_retiro) RETURNING v_resultado
                                 IF v_resultado = 0 THEN 
                                    -- Se inserta la solicitud en la tabla de aceptados
                                    CALL fn_inserta_aceptada(rec_marca_out.con_retiro,
                                                             rec_solicitud.*, 
                                                             rec_crea_solicitud_out.monto_pesos, 
                                                             rec_crea_solicitud_out.monto_adicional,
                                                             rec_crea_solicitud_out.monto_total) -- SOLICITUD ACEPTADA
                                 ELSE 
                                    -- Se inserta la solicitud en las rechazadas
                                    CALL fn_inserta_rechazo(5,6,rec_solicitud.*) -- NO SE PUEDO ACTUALIZAR EL ESTADO 
                                 END IF 
                              ELSE
                                 CALL fn_inserta_rechazo(4,rec_crea_solicitud_out.cod_rechazo,rec_solicitud.*) -- NO SE CREO LA SOLICITUD
                                 LET v_resultado = 1
                              END IF
                           ELSE
                              CALL fn_inserta_rechazo(4,9,rec_solicitud.*) -- PROBLEMAS EN EL LLAMADO AL CREA SOLICITUD
                              LET v_resultado = 1
                           END IF
                        ELSE 
                           CALL fn_inserta_rechazo(3,rec_marca_out.cod_rechazo,rec_solicitud.*) -- NO PASO LA MARCA
                           LET v_resultado = 1
                        END IF
                     ELSE 
                        CALL fn_inserta_rechazo(3,8,rec_solicitud.*) -- PROBLEMAS EN EL LLAMADO A LA MARCA
                        LET v_resultado = 1
                     END IF
                  ELSE 
                     CALL fn_inserta_rechazo(2,rec_disponibilidad_out.cod_rechazo,rec_solicitud.*) -- NO PASO LAS VALIDACIONES DE LA DISPONIBILIDAD
                     LET v_resultado = 1
                  END IF
               ELSE 
                  CALL fn_inserta_rechazo(2, 7,rec_solicitud.*) -- PROBLEMAS EN EL LLAMADO A LA DISPONIBILIDAD
                  LET v_resultado = 1
               END IF 
            ELSE
               CALL fn_inserta_rechazo(1,v_resultado,rec_solicitud.*) -- PROBLEMAS AL OBTENER EL RFC, MULTIPLES RFC PARA EL NSS, NO SE ENCONTRO NSS, ETC
            END IF
         ELSE
            CALL fn_inserta_rechazo(1,1,rec_solicitud.*)  -- 1 RECHAZO POR CLABE DUPLICADA 
         END IF  -- CLABE duplicada
      ELSE 
         CALL fn_inserta_rechazo(1,2,rec_solicitud.*)  -- 2 RECHAZO POR NSS DUPLICADO 
      END IF  -- nss duplicado
   END FOREACH

END FUNCTION 


FUNCTION fn_crea_temporal()
DEFINE v_query  STRING

DROP TABLE IF EXISTS tmp_fondo72;
DROP TABLE IF EXISTS tmp_fondo72_aceptados;
DROP TABLE IF EXISTS tmp_fondo72_rechazados;

CREATE TEMP TABLE tmp_fondo72 (
         nss             CHAR(11),
         curp            CHAR(18),
         nombre_af       CHAR(40),
         ap_paterno_af   CHAR(40),
         ap_materno_af   CHAR(40),
         f_nacimiento    CHAR(10),
         sexo            CHAR(1),
         cuenta_clabe    CHAR(18),
         tpo_pension     CHAR(2),
         grupo_familiar  CHAR(2));

CREATE INDEX idxtmp_fondo72_nss   ON tmp_fondo72 (nss); 
CREATE INDEX idxtmp_fondo72_clabe ON tmp_fondo72 (cuenta_clabe); 

CREATE TEMP TABLE tmp_fondo72_aceptados (
         nss              CHAR(11),
         curp             CHAR(18),
         nombre_af        CHAR(40),
         ap_paterno_af    CHAR(40),
         ap_materno_af    CHAR(40),
         f_nacimiento     CHAR(10),
         sexo             CHAR(1),
         cuenta_clabe     CHAR(18),
         tpo_pension      CHAR(2),
         grupo_familiar   CHAR(2),
         id_solicitud     DECIMAL(9,0),
         estado_solicitud SMALLINT,
         f_solicitud      CHAR(10),
         tanto_normal     CHAR(10),
         tanto_adicional  CHAR(10),
         total            CHAR(10),
         mensaje          CHAR(30),
         archivo_origen   CHAR(50));

CREATE TEMP TABLE tmp_fondo72_rechazados (
         nss             CHAR(11),
         curp            CHAR(18),
         nombre_af       CHAR(40),
         ap_paterno_af   CHAR(40),
         ap_materno_af   CHAR(40),
         f_nacimiento    CHAR(10),
         sexo            CHAR(1),
         cuenta_clabe    CHAR(18),
         tpo_pension     CHAR(2),
         grupo_familiar  CHAR(2),
         cod_rechazo     SMALLINT,
         mensaje         CHAR(30),
         archivo_origen  CHAR(50),
         rechazado_por   CHAR(30));

 LET v_query = "SELECT des_corta FROM ret_rechazo_generico WHERE cod_rechazo = ?;"       
 PREPARE prep_busca_descripcion FROM v_query
 
END FUNCTION

FUNCTION fn_carga_archivo(p_nombre_archivo)
DEFINE v_sql            STRING 
DEFINE v_archivo        CHAR(100)
DEFINE p_nombre_archivo CHAR(55) 

   LET v_archivo = "/safreviv_int/ret/rescate/", p_nombre_archivo CLIPPED

   LOAD FROM v_archivo
   INSERT INTO tmp_fondo72
   
END FUNCTION

FUNCTION fn_inicia_proceso(p_nombre_archivo)
DEFINE v_resultado        SMALLINT 
DEFINE v_folio            DECIMAL(9,0)
DEFINE p_nombre_archivo   STRING

   -- se verifica si se puede continuar con la operacion
   LET v_resultado = fn_valida_operacion(0,g_proceso_cod,g_opera_cod)
   
   -- se valida que se pueda iniciar la operacion
   CALL fn_valida_operacion(0,g_proceso_cod,g_opera_cod) RETURNING v_resultado

   -- si se pudo validar
   IF ( v_resultado = 0 ) THEN
      	
      -- se genera el pid 
      CALL fn_genera_pid(g_proceso_cod, g_opera_cod, g_usuario) RETURNING g_pid
      -- se genera el folio
      CALL fn_genera_folio(g_proceso_cod, g_opera_cod, g_usuario) RETURNING v_folio 
      -- Se inicializa el proceso
      CALL fn_inicializa_proceso(g_pid             ,
                                 g_proceso_cod     ,
                                 g_opera_cod       ,
                                 v_folio           ,
                                 "RETS19-168"      ,
                                 p_nombre_archivo  ,
                                 g_usuario)  RETURNING v_resultado
      
      -- si se pudo iniciar la operacion
      IF ( v_resultado = 0 ) THEN
         
         -- inicia la operacion
         CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,v_folio,"RETS19-168",p_nombre_archivo,g_usuario)
         RETURNING v_resultado
      ELSE
         DISPLAY "No se pudo inicializar el proceso: ",v_resultado
      END IF 
   ELSE 
      DISPLAY "No se puede iniciar la operación: ",v_resultado
   END IF 
         
   RETURN v_resultado
   
END FUNCTION 

FUNCTION fn_valida_duplicidad_nss(p_tipo_busqueda, p_nss)
   DEFINE p_tipo_busqueda SMALLINT 
   DEFINE p_nss           CHAR(11)
   DEFINE v_registros     INTEGER 

   LET v_registros = 0

   IF p_tipo_busqueda = 0 THEN 
      SELECT COUNT(*)
      INTO   v_registros
      FROM   (SELECT COUNT(*), nss 
              FROM   tmp_fondo72
              GROUP  BY nss
              HAVING COUNT(*) > 1)
   ELSE
      SELECT COUNT(*)
      INTO   v_registros
      FROM   tmp_fondo72
      WHERE  nss = p_nss
      IF v_registros = 1 THEN 
        LET v_registros = 0
      END IF 
   END IF 

   RETURN v_registros
END FUNCTION 

FUNCTION fn_valida_duplicidad_clabe(p_tipo_busqueda, p_clabe)
   DEFINE p_tipo_busqueda SMALLINT 
   DEFINE p_clabe         CHAR(18)

   DEFINE v_registros     INTEGER 

   LET v_registros = 0

   IF p_tipo_busqueda = 0 THEN 
      SELECT COUNT(*)
      INTO   v_registros
      FROM   (SELECT COUNT(*), cuenta_clabe
              FROM   tmp_fondo72
              GROUP  BY cuenta_clabe
              HAVING COUNT(*) > 1)

   ELSE
      SELECT COUNT(*)
      INTO   v_registros
      FROM   tmp_fondo72
      WHERE  cuenta_clabe = p_clabe
      IF v_registros = 1 THEN 
        LET v_registros = 0
      END IF 
   END IF 

   RETURN v_registros
END FUNCTION 
FUNCTION fn_inserta_rechazo(p_quien_rechaza,p_tipo_rechazo,p_rec_solicitud)
   DEFINE p_quien_rechaza    SMALLINT 
   DEFINE p_tipo_rechazo     SMALLINT
   DEFINE p_rec_solicitud RECORD 
         nss             CHAR(11),
         curp            CHAR(18),
         nombre_af       CHAR(40),
         ap_paterno_af   CHAR(40),
         ap_materno_af   CHAR(40),
         f_nacimiento    CHAR(10),
         sexo            CHAR(1),
         cuenta_clabe    CHAR(18),
         tpo_pension     CHAR(2),
         grupo_familiar  CHAR(2)
   END RECORD 
   DEFINE cod_rechazo     SMALLINT
   DEFINE mensaje         CHAR(30)
   DEFINE v_des_corta     CHAR(30)
   DEFINE archivo         CHAR (50)
   DEFINE v_rechazado_por CHAR(30)

   LET archivo = g_nombre_archivo CLIPPED 

   CASE p_tipo_rechazo
      WHEN 1
         LET mensaje = "CLABE DUPLICADA EN A"
      WHEN 2
         LET mensaje = "NSS DUPLICADO EN A"
      WHEN 3
         LET mensaje = "NSS NO EXISTE"
      WHEN 4
         LET mensaje = "MULTIPLE NSS EN CAT"
      WHEN 6
         LET mensaje = "NO SE ACTUALIZA ESTADO"
      WHEN 7
         LET mensaje = "LLAMADO NO EXITOSO DISP"
      WHEN 8
         LET mensaje = "LLAMADO NO EXITOSO MARCA"
      WHEN 9
         LET mensaje = "LLAMADO NO EXITOSO CREA SOL"
      OTHERWISE
         -- Busca la descripción en el catálogo de rechazos
         LET mensaje = ""
   END CASE

   IF p_tipo_rechazo >= 10 THEN

      EXECUTE prep_busca_descripcion USING p_tipo_rechazo
         INTO    v_des_corta
      LET mensaje = v_des_corta
   END IF 

   CASE p_quien_rechaza 
      WHEN 1 -- rechazado por validaciones previas
         LET v_rechazado_por = "VALIDACIONES PREVIAS"
      WHEN 2 -- rechazadi por dispoibilidad
         LET v_rechazado_por = "DISPONIBILIDAD" 
      WHEN 3 -- rechazadi por marca
         LET v_rechazado_por = "MARCA" 
      WHEN 4 -- rechazadi por crea solicitud
         LET v_rechazado_por = "CREA SOLICITUD" 
      OTHERWISE
         LET v_rechazado_por = "NO IDENTIFICADO"
    END CASE 


   LET cod_rechazo = p_tipo_rechazo
   INSERT INTO tmp_fondo72_rechazados 
      VALUES (p_rec_solicitud.nss,            p_rec_solicitud.curp,          p_rec_solicitud.nombre_af, 
              p_rec_solicitud.ap_paterno_af,  p_rec_solicitud.ap_materno_af, p_rec_solicitud.f_nacimiento,
              p_rec_solicitud.sexo,           p_rec_solicitud.cuenta_clabe,  p_rec_solicitud.tpo_pension,
              p_rec_solicitud.grupo_familiar, cod_rechazo,                   mensaje,
              archivo, v_rechazado_por)

END FUNCTION 
FUNCTION fn_inserta_aceptada(p_id_solicitud,p_rec_solicitud, p_tanto_normal, p_tanto_adicional, p_total)
   DEFINE p_rec_solicitud RECORD 
         nss             CHAR(11),
         curp            CHAR(18),
         nombre_af       CHAR(40),
         ap_paterno_af   CHAR(40),
         ap_materno_af   CHAR(40),
         f_nacimiento    CHAR(10),
         sexo            CHAR(1),
         cuenta_clabe    CHAR(18),
         tpo_pension     CHAR(2),
         grupo_familiar  CHAR(2)
   END RECORD 
   DEFINE p_id_solicitud     DECIMAL(9,0)
   DEFINE p_tanto_normal     DECIMAL(12,2)
   DEFINE p_tanto_adicional  DECIMAL(12,2)
   DEFINE p_total            DECIMAL(12,2)
   DEFINE mensaje            CHAR(30)
   DEFINE archivo            CHAR (50)
   DEFINE v_estado_solicitud SMALLINT 
   DEFINE v_f_solicitud      CHAR(10)
   DEFINE v_tanto_normal     CHAR(10)
   DEFINE v_tanto_adicional  CHAR(10)
   DEFINE v_total            CHAR(10)
   
   LET archivo = g_nombre_archivo CLIPPED 

   LET mensaje            = "ACEPTADA"
   LET v_estado_solicitud = 15
   LET v_f_solicitud      = TODAY USING "dd/mm/yyyy"
   LET v_tanto_normal     = p_tanto_normal USING "&&&&&&&.&&"
   LET v_tanto_adicional  = p_tanto_adicional USING "&&&&&&&.&&"
   LET v_total            = p_total USING "&&&&&&&.&&"
   

   INSERT INTO tmp_fondo72_aceptados
      VALUES (p_rec_solicitud.nss,            p_rec_solicitud.curp,          p_rec_solicitud.nombre_af, 
              p_rec_solicitud.ap_paterno_af,  p_rec_solicitud.ap_materno_af, p_rec_solicitud.f_nacimiento,
              p_rec_solicitud.sexo,           p_rec_solicitud.cuenta_clabe,  p_rec_solicitud.tpo_pension,
              p_rec_solicitud.grupo_familiar, p_id_solicitud,                v_estado_solicitud, 
              v_f_solicitud,                  v_tanto_normal,                v_tanto_adicional,
              v_total,                        mensaje,                       archivo)

END FUNCTION 
FUNCTION fn_busca_rfc (p_nss)
   DEFINE p_nss        CHAR(11)
   DEFINE v_rfc        CHAR(13)
   DEFINE v_cantidad   SMALLINT 
   DEFINE v_resultado  SMALLINT 

   LET v_cantidad = 0
   LET v_resultado = 0
   INITIALIZE v_rfc TO NULL

   SELECT COUNT(*)
   INTO   v_cantidad 
   FROM   afi_fondo72
   WHERE  nss = p_nss
   AND    ind_estado_cuenta = 0  -- cuenta Activa

   IF ( v_cantidad IS NULL OR v_cantidad < 1 ) THEN
      LET v_resultado = 3 -- NSS NO EXISTE
   ELSE
      IF ( v_cantidad > 1 ) THEN
         LET v_resultado = 4 -- NSS MAS DE UNO EN CATALOGO
      ELSE
         LET v_resultado = 0
         SELECT rfc
         INTO   v_rfc
         FROM   afi_fondo72
         WHERE  nss = p_nss
         AND    ind_estado_cuenta = 0  -- cuenta Activa
      END IF
   END IF 

   RETURN v_resultado, v_rfc 
END FUNCTION 

FUNCTION fn_disponibilidad()
   DEFINE v_resultado     SMALLINT

   INITIALIZE fn_saldo_disponible_faRequest TO NULL
   INITIALIZE fn_saldo_disponible_faResponse TO NULL
   
      -- Invocacion al servicio de disponibilidad 
   LET fn_saldo_disponible_faRequest.nss = rec_disponibilidad_in.nss
   LET fn_saldo_disponible_faRequest.rfc = rec_disponibilidad_in.rfc
   LET fn_saldo_disponible_faRequest.causal_ret_fa = rec_disponibilidad_in.causal_ret_fa
   LET fn_saldo_disponible_faRequest.f_inicio_pension = rec_disponibilidad_in.f_inicio_pension
   LET fn_saldo_disponible_faRequest.medio_entrega = rec_disponibilidad_in.medio_entrega
   LET fn_saldo_disponible_faRequest.nrp = rec_disponibilidad_in.nrp
   CALL fn_saldo_disponible_fa_g() RETURNING v_resultado
   IF v_resultado = 0 THEN
      LET rec_disponibilidad_out.cod_rechazo = fn_saldo_disponible_faResponse.cod_rechazo
      LET rec_disponibilidad_out.des_rechazo = fn_saldo_disponible_faResponse.des_rechazo
      LET rec_disponibilidad_out.estado_solicitud = fn_saldo_disponible_faResponse.estado_solicitud
      LET rec_disponibilidad_out.monto_adicional = fn_saldo_disponible_faResponse.monto_adicional
      LET rec_disponibilidad_out.monto_pesos = fn_saldo_disponible_faResponse.monto_pesos
      LET rec_disponibilidad_out.monto_total = fn_saldo_disponible_faResponse.monto_total
      LET rec_disponibilidad_out.nss = fn_saldo_disponible_faResponse.nss
      LET rec_disponibilidad_out.pago_dap = fn_saldo_disponible_faResponse.pago_dap
      LET rec_disponibilidad_out.rfc = fn_saldo_disponible_faResponse.rfc
   END IF 

   RETURN v_resultado 
END FUNCTION 

FUNCTION fn_marca()
   DEFINE v_resultado     SMALLINT

   INITIALIZE fn_marcaje_faRequest TO NULL
   INITIALIZE fn_marcaje_faResponse TO NULL
   
      -- Invocacion al servicio de marca
   LET fn_marcaje_faRequest.nss = rec_marca_in.nss
   LET fn_marcaje_faRequest.rfc = rec_marca_in.rfc
   LET fn_marcaje_faRequest.medio_entrega = rec_marca_in.medio_entrega
   LET fn_marcaje_faRequest.ind_marca = rec_marca_in.ind_marca
   LET fn_marcaje_faRequest.cuenta_clabe = rec_marca_in.cuenta_clabe
   LET fn_marcaje_faRequest.cod_rechazo = rec_marca_in.cod_rechazo
   LET fn_marcaje_faRequest.caso_crm = rec_marca_in.caso_crm
   
   CALL fn_marcaje_fa_g() RETURNING v_resultado
   IF v_resultado = 0 THEN
      LET rec_marca_out.caso_crm = fn_marcaje_faResponse.caso_crm
      LET rec_marca_out.cod_rechazo = fn_marcaje_faResponse.cod_rechazo
      LET rec_marca_out.con_retiro = fn_marcaje_faResponse.con_retiro
      LET rec_marca_out.cve_dap = fn_marcaje_faResponse.cve_dap
      LET rec_marca_out.des_rechazo = fn_marcaje_faResponse.des_rechazo
      LET rec_marca_out.est_marca = fn_marcaje_faResponse.est_marca
      LET rec_marca_out.nss = fn_marcaje_faResponse.nss
      LET rec_marca_out.rfc = fn_marcaje_faResponse.rfc
      LET rec_marca_out.saldo_pesos = fn_marcaje_faResponse.saldo_pesos
   END IF 

   RETURN v_resultado 
END FUNCTION 

FUNCTION fn_crea_solicitud()
   DEFINE v_resultado     SMALLINT

   INITIALIZE fn_solicitud_fondo_ahorroRequest TO NULL
   INITIALIZE fn_solicitud_fondo_ahorroResponse TO NULL
   
   -- Invocacion al servicio de crea solicitud 
   LET fn_solicitud_fondo_ahorroRequest.rfc = rec_crea_solicitud_in.rfc
   LET fn_solicitud_fondo_ahorroRequest.nss = rec_crea_solicitud_in.nss
   LET fn_solicitud_fondo_ahorroRequest.nrp = rec_crea_solicitud_in.nrp
   LET fn_solicitud_fondo_ahorroRequest.medio_entrega = rec_crea_solicitud_in.medio_entrega
   LET fn_solicitud_fondo_ahorroRequest.f_inicio_pension = rec_crea_solicitud_in.f_inicio_pension
   LET fn_solicitud_fondo_ahorroRequest.causal_retiro = rec_crea_solicitud_in.causal_retiro
   LET fn_solicitud_fondo_ahorroRequest.caso_crm = rec_crea_solicitud_in.caso_crm
   LET fn_solicitud_fondo_ahorroRequest.arr_beneficiario.element[1].ap_materno = rec_crea_solicitud_in.arr_beneficiario[1].ap_materno
   LET fn_solicitud_fondo_ahorroRequest.arr_beneficiario.element[1].ap_paterno = rec_crea_solicitud_in.arr_beneficiario[1].ap_paterno
   LET fn_solicitud_fondo_ahorroRequest.arr_beneficiario.element[1].clabe_bancaria = rec_crea_solicitud_in.arr_beneficiario[1].clabe_bancaria
   LET fn_solicitud_fondo_ahorroRequest.arr_beneficiario.element[1].email = rec_crea_solicitud_in.arr_beneficiario[1].email
   LET fn_solicitud_fondo_ahorroRequest.arr_beneficiario.element[1].entidad_federativa = rec_crea_solicitud_in.arr_beneficiario[1].entidad_federativa
   LET fn_solicitud_fondo_ahorroRequest.arr_beneficiario.element[1].nombre = rec_crea_solicitud_in.arr_beneficiario[1].nombre
   LET fn_solicitud_fondo_ahorroRequest.arr_beneficiario.element[1].rfc = rec_crea_solicitud_in.arr_beneficiario[1].rfc
   LET fn_solicitud_fondo_ahorroRequest.arr_beneficiario.element[1].tel_movil = rec_crea_solicitud_in.arr_beneficiario[1].tel_movil
   LET fn_solicitud_fondo_ahorroRequest.arr_beneficiario.element[1].telefono = rec_crea_solicitud_in.arr_beneficiario[1].telefono
   LET fn_solicitud_fondo_ahorroRequest.arr_beneficiario.element[1].tipo_beneficiario = rec_crea_solicitud_in.arr_beneficiario[1].tipo_beneficiario

   CALL fn_solicitud_fondo_ahorro_g() RETURNING v_resultado

   IF v_resultado = 0 THEN
      LET rec_crea_solicitud_out.caso_crm = fn_solicitud_fondo_ahorroResponse.caso_crm
      LET rec_crea_solicitud_out.cod_rechazo = fn_solicitud_fondo_ahorroResponse.cod_rechazo
      LET rec_crea_solicitud_out.des_rechazo = fn_solicitud_fondo_ahorroResponse.des_rechazo
      LET rec_crea_solicitud_out.estado_solicitud = fn_solicitud_fondo_ahorroResponse.estado_solicitud
      LET rec_crea_solicitud_out.monto_adicional = fn_solicitud_fondo_ahorroResponse.monto_adicional
      LET rec_crea_solicitud_out.monto_pesos = fn_solicitud_fondo_ahorroResponse.monto_pesos
      LET rec_crea_solicitud_out.monto_total = fn_solicitud_fondo_ahorroResponse.monto_total
      LET rec_crea_solicitud_out.nss = fn_solicitud_fondo_ahorroResponse.nss
      LET rec_crea_solicitud_out.referencia_dap = fn_solicitud_fondo_ahorroResponse.referencia_dap
      LET rec_crea_solicitud_out.rfc = fn_solicitud_fondo_ahorroResponse.rfc
   END IF 

   RETURN v_resultado 
END FUNCTION 

FUNCTION fn_actualiza_solicitud(p_id_solicitud)
   DEFINE p_id_solicitud        DECIMAL(9,0)


   UPDATE ret_solicitud_generico
   SET    estado_solicitud = 15
   WHERE  id_solicitud = p_id_solicitud
   AND    estado_solicitud = 10;
   
   UPDATE ret_fondo_ahorro_generico
   SET    estado_solicitud = 15
   WHERE  id_solicitud = p_id_solicitud
   AND    estado_solicitud = 10;

   RETURN 0
END FUNCTION

FUNCTION fn_descarga_temporales ()
   DEFINE v_resultado         SMALLINT
   DEFINE v_archivo_salida    CHAR(100)
   DEFINE v_fecha_hora_genera CHAR (14)
   DEFINE v_contador          INTEGER 

   
   SELECT to_char(current year to second, "%Y%m%d%H%M%S") 
   INTO   v_fecha_hora_genera
   FROM   systables 
   WHERE  tabid = 1

   LET v_contador = 0
   -- cuanta los registros a bajar
   SELECT COUNT(*) 
   INTO   v_contador
   FROM   tmp_fondo72_aceptados
   IF v_contador > 0 THEN 
      LET v_archivo_salida = "/safreviv_int/ret/envio/", g_nombre_archivo CLIPPED, "_ACEPTADOS_", v_fecha_hora_genera
      DISPLAY " " 
      DISPLAY " " 
      DISPLAY "Bajando los aceptados"
      DISPLAY " " 
      DISPLAY " Se genera el archivo ", v_archivo_salida CLIPPED, " de aceptados con ", v_contador USING "<<<<<<<<<<" , " registros."
      UNLOAD TO v_archivo_salida
      SELECT * 
      FROM   tmp_fondo72_aceptados;
      DISPLAY " " 
      DISPLAY "Aceptados generado :",  v_archivo_salida
      DISPLAY " " 
      
   END IF 

   LET v_contador = 0
   -- cuanta los registros a bajar
   SELECT COUNT(*) 
   INTO   v_contador
   FROM   tmp_fondo72_rechazados
   IF v_contador > 0 THEN 
      LET v_archivo_salida = "/safreviv_int/ret/envio/", g_nombre_archivo CLIPPED, "_RECHAZADOS_", v_fecha_hora_genera
      DISPLAY " " 
      DISPLAY " " 
      DISPLAY "Bajando los rechazados"
      DISPLAY " " 
      DISPLAY " Se genera el archivo ", v_archivo_salida CLIPPED , " de rechazados con ", v_contador USING "<<<<<<<<<<", " registros."
      UNLOAD TO v_archivo_salida
      SELECT * 
      FROM   tmp_fondo72_rechazados;
      DISPLAY " " 
      DISPLAY "Rechazados generado :",  v_archivo_salida
      DISPLAY " " 
      
   END IF 

END FUNCTION
