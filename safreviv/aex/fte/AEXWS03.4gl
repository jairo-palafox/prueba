################################################################################
#Modulo        => AEX                                                          #
#Programa      => AEXWS03                                                      #
#Ojetivo       => Cliente para notificación de pagos a CRM para Aportaciones,  #
#                 extraordinarias                                              #
#Fecha inicio  => Septiembre, 2020.                                            #
#Requerimiento => SACI2019-127                                                 #
#Autor         => Jairo Giovanny Palafox Sanchez                               #
#------------------------------------------------------------------------------#
#Fecha de modificacion =>   Modificación                                       #
#                      =>                                                      #
#                      =>                                                      #
################################################################################

IMPORT com
IMPORT util

DATABASE safre_viv

GLOBALS "IndividualizacionPago.inc"
GLOBALS
DEFINE    v_solicitudes_procesadas   INTEGER
DEFINE    v_solicitudes_enviadas     INTEGER
CONSTANT  v_estado_pagado_saci         = 72   --PAGADA SACI CRM
CONSTANT  v_estado_rechazada_saci      = 102  --RECHAZADA SACI CRM
CONSTANT  v_estado_envio_aceptada      = 60
CONSTANT  v_estado_envio_rechazada     = 100
DEFINE    v_fecha_envio           CHAR(08)
DEFINE g_pid                DECIMAL (9,0)                 -- ID del proceso
DEFINE g_proceso_cod        LIKE cat_proceso.proceso_cod  -- código del proceso
DEFINE g_opera_cod          LIKE cat_operacion.opera_cod  -- código de operacion
DEFINE g_usuario            CHAR (20)
DEFINE g_nom_archivo        STRING
DEFINE g_folio              DECIMAL(10,0)
END GLOBALS

MAIN
 DEFINE v_fecha_char  CHAR(20) 
 DEFINE r_bandera     SMALLINT
 
   -- argumentos del programa lanzador
   LET g_usuario           = ARG_VAL (1)
   LET g_pid               = ARG_VAL (2)     --forma como se ejecutara el programa
   LET g_proceso_cod       = ARG_VAL (3)
   LET g_opera_cod         = ARG_VAL (4)
   LET g_folio             = ARG_VAL (5)
   LET g_nom_archivo       = ARG_VAL (6)
   DISPLAY "========================================================"
   
   DISPLAY "PARAMETROS DE ENTRADA: "
   DISPLAY "USUARIO: ", g_usuario
   DISPLAY "PID: ", g_pid
   DISPLAY "PROCESO: ", g_proceso_cod
   DISPLAY "OPERACION: ", g_opera_cod
   DISPLAY "FOLIO: ", g_folio
   DISPLAY "NOMBRE ARCHIVO : ", g_nom_archivo
   DISPLAY "========================================================"
   -- inicializa variables
   LET r_bandera     = 0
   LET v_fecha_char  = CURRENT HOUR TO SECOND
   --DISPLAY "v_fecha_char",v_fecha_char
   LET v_fecha_envio = v_fecha_char
   --DISPLAY "v_fecha_envio",v_fecha_envio
   -- ejecutar servicio web
   DISPLAY "EJECUTA SERVICIO WEB :>>> "
   DISPLAY "IndividualizacionPago :>> "
   DISPLAY "EJECUTA NEGOCIO :>>"

   CALL fn_obtieneDatos()

   DISPLAY "========================================================"
   
   --Actualiza la operación
   CALL fn_actualiza_opera_fin(g_pid, g_proceso_cod,g_opera_cod) RETURNING r_bandera

   --Valida Operación Final
   IF r_bandera <> 0 THEN
      CALL fn_error_opera(g_pid, g_proceso_cod,g_opera_cod)
      RETURNING r_bandera
   ELSE  
      DISPLAY "Fin de la operación: Se confirmo la Comunicacion con CRM."
   END IF
    
END MAIN

FUNCTION  fn_obtieneDatos()
 DEFINE v_codigo       STRING
 DEFINE v_descripcion  STRING
 DEFINE v_sql               CHAR(500)
 DEFINE v_resultado         SMALLINT
 DEFINE v_cod_respuesta_crm CHAR(50)
 DEFINE v_des_respuesta_crm CHAR(100)
 DEFINE v_solicitud_pago    RECORD 
          id_solicitud      DECIMAL(9,0),
          estado_solicitud  SMALLINT,
          nss               CHAR(11),
          caso_crm          DECIMAL(10,0),
          llave_referencia  CHAR(27),
          monto_pesos       DECIMAL(12,0),
          f_pago            CHAR(10),
          folio_fico        CHAR(45),
          cod_rechazo       SMALLINT,
          des_rechazo       CHAR(50)
        END RECORD
  DISPLAY "CONSULTA A OBTENER:>>"

  LET v_sql = "SELECT p.id_solicitud,p.estado_solicitud,p.nss, p.caso_crm,p.llave_referencia,p.monto_pesos,p.f_pago,",
              "\np.llave_consecutivo||p.llave_referencia||p.llave_secuencia_pago,",
              "\n p.cod_rechazo, r.des_corta FROM aex_solicitud_pago p LEFT JOIN aex_cat_rechazo r ",
              "\ON p.cod_rechazo = r.cod_Rechazo",
              "\n WHERE 1 = 1 ",
              "\n AND p.estado_solicitud IN (60,100) "
  -- todas las solicitudes con estado de pago
  -- 60- PAGO REGISTRADO CUENTA INDIVIDUAL DEL TRABAJADOR
  --100-SOLICITUD RECHAZADA
  --DISPLAY v_sql
  PREPARE pre_obt_universo FROM v_sql

  DECLARE cur_obt_datos CURSOR FOR pre_obt_universo
  LET v_solicitudes_procesadas = 0
  LET v_solicitudes_enviadas   = 0
  
  FOREACH cur_obt_datos INTO v_solicitud_pago.*

    
     
     DISPLAY "solicitud pago ", v_solicitud_pago.id_solicitud
     LET MT_IndividualizacionPago.nss            = v_solicitud_pago.nss
     LET MT_IndividualizacionPago.noCaso         = v_solicitud_pago.caso_crm
     LET MT_IndividualizacionPago.lineadecaptura = v_solicitud_pago.llave_referencia -- lave referencia en aex_solicitud_pago
     LET MT_IndividualizacionPago.monto          = v_solicitud_pago.monto_pesos
     LET MT_IndividualizacionPago.fechadepago    = v_solicitud_pago.f_pago USING "yyyy-mm-dd"||"T09:00:00.000Z"
     LET MT_IndividualizacionPago.foliofico      = v_solicitud_pago.folio_fico -- Llave_consecutivo + llave_referencia + llave_secuencia_pago
     LET MT_IndividualizacionPago.cEstatus       = v_solicitud_pago.cod_rechazo
     LET MT_IndividualizacionPago.dcodigoestatus = v_solicitud_pago.des_rechazo

     -- se ejecuta el WS
    
     CALL  SI_IndividualizacionPago_SO_g() RETURNING v_resultado
     DISPLAY "   "
     DISPLAY "   "
     DISPLAY "########################################################################"
     DISPLAY " EL resultado de la Notificación nuevo"
     DISPLAY "   "
     DISPLAY "   "
     DISPLAY "Resultado de la ejecucion  :", v_resultado                                              ,":"
     DISPLAY "CODE                       :", wsError.code                                             ,":"
     DISPLAY "CODENS                     :", wsError.codeNS                                           ,":"
     DISPLAY "DESCRIPTION                :", wsError.description                                      ,":"
     DISPLAY "ACTION                     :", wsError.action                                           ,":"

    -- si el servicio se ejecuta de forma satisfactoria se recibe codigod e respuesta

     IF v_resultado = 0 THEN
        LET v_codigo              = MT_IndividualizacionPago_res.CODIGO
        LET v_descripcion         = MT_IndividualizacionPago_res.DESCRIPCION

        -- se obtiene la respuesta
       
        DISPLAY "RESULTADO EJECUCIÓN: >> ",v_resultado
        DISPLAY "INFORMACION RECIBIDA: >> "
        DISPLAY "CÓDIGO: >>",v_codigo
        DISPLAY "DESCRIPCIÓN: >> ",v_descripcion
        DISPLAY "FINALIZA INFORMACION : >>"
        

        -- se asignan valores recibidos
        LET v_cod_respuesta_crm  = v_codigo
        LET v_des_respuesta_crm  = v_descripcion

        -- se guarda informacion en la tabla de control
        INSERT INTO aex_individualiza_pago VALUES(v_solicitud_pago.id_solicitud,v_solicitud_pago.estado_solicitud,TODAY,v_fecha_envio,v_cod_respuesta_crm,v_des_respuesta_crm);
           
        DISPLAY "----------------------------------------------"
        -- si la respuesta es satisfactoria se actualiza maquinaria
        IF v_cod_respuesta_crm = '0001' THEN
           LET v_solicitudes_enviadas = v_solicitudes_enviadas + 1
           --se actualiza maquinaria de solicitud a PAGADA SACI CRM
           IF v_solicitud_pago.estado_solicitud = v_estado_envio_aceptada THEN
              UPDATE aex_solicitud_pago
               SET estado_solicitud = v_estado_pagado_saci
              WHERE id_solicitud  = v_solicitud_pago.id_solicitud
               AND estado_solicitud = v_estado_envio_aceptada
           ELSE
              UPDATE aex_solicitud_pago
               SET estado_solicitud = v_estado_rechazada_saci
              WHERE id_solicitud  = v_solicitud_pago.id_solicitud
               AND estado_solicitud = v_estado_envio_rechazada
           END IF
        END IF
     ELSE
        DISPLAY "No se obtuvo respuesta del servicio web  :", v_resultado ,":"
     END IF
     -- Limpia el arreglo de respuesta
     INITIALIZE MT_IndividualizacionPago_res TO NULL
     INITIALIZE wsError TO NULL
     INITIALIZE v_solicitud_pago TO NULL 
     LET v_solicitudes_procesadas = v_solicitudes_procesadas + 1
  END FOREACH
    
  DISPLAY ""
  DISPLAY ""
  DISPLAY "               SOLICITUDES"
  DISPLAY "----------------------------------------------"
  DISPLAY "ENVIADAS:              ", v_solicitudes_enviadas
  DISPLAY "PROCESADAS:            ", v_solicitudes_procesadas
END FUNCTION