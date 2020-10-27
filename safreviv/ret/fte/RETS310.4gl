#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETS310                                                                #
#Objetivo     => Programa que genera el archivo para SIAF de solicitures Retiro Ley 73  #
#Fecha inicio => 30/12/2013                                                             #
#Modificacion:                                                                          #
#########################################################################################
DATABASE safre_viv

GLOBALS "RETG01.4gl"

MAIN
DEFINE r_resultado_opera           INTEGER
       ,v_resultado_gen            INTEGER
       ,p_pid                      LIKE bat_ctr_operacion.pid                  -- PID del proceso
       ,p_proceso_cod              LIKE bat_ctr_operacion.proceso_cod          -- codigo del proceso
       ,p_opera_cod                LIKE bat_ctr_operacion.opera_cod            -- codigo de la operacion
       ,p_usuario_cod              LIKE seg_usuario.usuario_cod                -- clave del usuario firmado
       ,p_nombre_archivo           LIKE glo_ctr_archivo.nombre_archivo         -- nombre dle archivo
       ,p_fecha_pago               DATE
       ,v_folio                    LIKE glo_ctr_archivo.folio
       ,v_proceso_desc             LIKE cat_proceso.proceso_desc
       ,v_opera_desc               LIKE cat_operacion.opera_desc
       ,v_detalle_monitoreo        STRING

    -- se recuperan los parametros la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET v_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   LET p_fecha_pago     = ARG_VAL(7)

   SELECT proceso_desc
     INTO v_proceso_desc
     FROM cat_proceso
    WHERE proceso_cod = p_proceso_cod

   SELECT opera_desc
     INTO v_opera_desc
     FROM cat_operacion
    WHERE proceso_cod = p_proceso_cod
      AND opera_cod   = p_opera_cod

   -- Encabezado para el archivo de monitoreo
   DISPLAY "___________________________________________________________________"
   LET v_detalle_monitoreo = "\n PROCESO            : ",v_proceso_desc,
                             "\n OPERACIÓN          : ",v_opera_desc,
                             "\n FECHA              : ",TODAY USING 'dd-mm-yyyy',
                             "\n HORA               : ",TIME(CURRENT)
   DISPLAY v_detalle_monitoreo
   DISPLAY "___________________________________________________________________"

   -- Se genera un numero de folio asociado a la operacion. Parametros: proceso, operacion, usuario
   CALL fn_genera_folio(p_proceso_cod,p_opera_cod,p_usuario_cod)
   RETURNING v_folio
   
   -- Se invoca la funcion de generacion del archivo de salida
   CALL fn_genera_archivo(v_proceso_desc, v_opera_desc, v_folio, p_fecha_pago)
   RETURNING v_resultado_gen
   
   -- si la generacion del archivo finalizo correctamente
   IF ( v_resultado_gen = 0 ) THEN
      -- Finaliza la operacion de generacion del archivo
      CALL  fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
      RETURNING r_resultado_opera
	  
	  -- si no se pudo finalizar la operacion
      IF ( r_resultado_opera <> 0 ) THEN
         -- Actualiza a estado erróneo
         CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
         RETURNING r_resultado_opera
      END IF 
   ELSE
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
           RETURNING r_resultado_opera
   END IF
   
END MAIN

{
======================================================================
Clave: 
Nombre: fn_genera_archivo
Fecha creacion: Marzo 18, 2914
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Genera el archivo de salida para pago de solicitudes de retiro Ley 73 por SIAF

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_genera_archivo(v_proceso_desc, v_opera_desc, v_folio, v_fecha_pago)
-- Variables para el manejo de las consultas
DEFINE  v_folio              LIKE glo_ctr_archivo.folio
       ,v_fecha_pago         DATE
       ,v_id_solicitud       DECIMAL(9,0)
       ,v_monto              DECIMAL(13,2)
       ,v_monto_total        DECIMAL(16,2)
       ,v_monto_pesos        DECIMAL(12,2)
       ,v_proceso_desc       LIKE cat_proceso.proceso_desc
       ,v_opera_desc         LIKE cat_operacion.opera_desc
       ,v_consecutivo        INTEGER
       ,v_consecutivo_detalle INTEGER
   
-- para la generacion del archivo de salida
DEFINE v_encabezado         RECORD
          tpo_registro        CHAR(2),     
		  num_secuencia       CHAR(7),
		  cod_operacion       CHAR(2),
		  banco_participante  CHAR(3),
		  sentido             CHAR(1),
		  servicio            CHAR(1),
		  num_bloque          CHAR(7),
		  f_presentacion      CHAR(8),
		  cod_divisa          CHAR(2),
		  causa_rechazo       CHAR(2),
		  modalidad           CHAR(1),
		  uso_futuro_cce      CHAR(41),
		  uso_futuro_banco    CHAR(345)
	   END RECORD,
	   v_detalle                  RECORD -- registro de detalle del archivo de salida
        tpo_registro             CHAR(2),
		  num_secuencia            CHAR(7),
		  cod_operacion            CHAR(2),
		  cod_divisa               CHAR(2),
		  f_transferencia          CHAR(8),
		  banco_presentador        CHAR(3),
		  banco_receptor           CHAR(3),
		  importe_operacion        CHAR(15),
		  uso_futuro_cce           CHAR(16),
		  tpo_operacion            CHAR(2),
		  f_aplicacion             CHAR(8),
		  tpo_cuenta_ordenante     CHAR(2),
		  num_cuenta_ordenante     CHAR(20),
		  nombre_ordenante         CHAR(40),
		  rfc_curp_ordenante       CHAR(18),
		  tpo_cuenta_receptor      CHAR(2),
		  num_cuenta_receptor      CHAR(20),
		  nombre_receptor          CHAR(40),
		  rfc_curp_receptor        CHAR(18),
		  referencia_servicio      CHAR(40),
		  nombre_titular_servicio  CHAR(40),
		  importe_iva              CHAR(15),
		  num_referencia_ord       CHAR(7),
		  referenca_leyenda_ord    CHAR(40),
		  clave_rastreo            CHAR(30),
		  motivo_devolucion        CHAR(2),
		  f_presentacion_inicial   CHAR(8),
		  solicitud_confirmacion   CHAR(1),
		  uso_futuro_banco         CHAR(11)
	   END RECORD,
	   v_sumario                  RECORD
	      tpo_registro             CHAR(2),
		  num_secuencia            CHAR(7),
		  cod_operacion            CHAR(2),
		  num_bloque               CHAR(7),
		  num_operaciones          CHAR(7),
		  importe_total            CHAR(18),
		  uso_futuro_cce           CHAR(40),
		  uso_futuro_banco         CHAR(339)
	   END RECORD,
      v_ruta_envio               LIKE seg_modulo.ruta_envio,
	   v_ruta_archivo             STRING, -- ruta completa del archivo
	   v_nom_archivo              STRING,
      v1_nom_archivo             VARCHAR(30),
	   v_archivo                  base.Channel, -- apuntador al archivo de salida
	   v_cadena                   STRING, -- cadena auxiliar
	   v_secuencia_detalle        SMALLINT, -- secuencia de los registros de detalle
	   v_nombre_beneficiario      STRING, -- nombre del beneficiario
	   v_cuenta_clabe             CHAR(18),
      v_nom_titular              CHAR(40),
	   v_consulta_detalle         STRING,
      v_f_presentacion           DATE,
      v_f_transferencia          DATE,
      v_f_aplicacion             DATE,
      v_f_presentacion_inicial   DATE,
      v_fecha                    DATE,
      v_banco_presentador        SMALLINT,
      v_banco_receptor           SMALLINT,
      v_hora                     CHAR(8),
      v_nss                      CHAR(11),
      v_estado_solicitud         SMALLINT,
      v_cuenta_regs              INTEGER
	   
   
   DISPLAY "Se inicia la generacion del archivo"

   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio 
   INTO   v_ruta_envio
   FROM   seg_modulo
   WHERE  modulo_cod = 'ret'

   -- Incializacion de totales
   LET v_monto_total = 0
   LET v_consecutivo_detalle = 0

   --Calculo fecha de presentacion
   LET v_f_presentacion                 = fn_determina_fecha_presentacion(v_fecha_pago - 1)
   LET v_encabezado.f_presentacion      = v_f_presentacion USING "yyyymmdd" -- ESTA INCOMPLETO, es un dia habil antes de la fecha de pago

   LET v_consecutivo = 0

   SELECT COUNT(*) INTO v_consecutivo
   FROM ret_cza_envio_siaff
   WHERE nombre_archivo[12,19] = v_encabezado.f_presentacion

   IF v_consecutivo IS NOT NULL AND v_consecutivo > 0 THEN
      LET v_consecutivo = v_consecutivo + 1
   ELSE
      LET v_consecutivo = 1
   END IF

   -- Se asigna el nombre del archivo
   ##  Nombre del archivo:
   ##        RR_UUU_CLC_AAAAMMDD.dat
   ##  Donde:
   ##        RR       = 06= Ramo generador de la información:
   ##        UUU      = 800 (valor fijo)
   ##        CLC      = 001 Consecutivo del archivo
   ##        AAAAMMDD = Fecha de presentación del archivo (un día hábil antes de la fecha de pago):
   LET v_nom_archivo = "06_800_",v_consecutivo USING "&&&","_", v_f_presentacion USING 'yyyymmdd', ".dat"
   LET v1_nom_archivo = v_nom_archivo

   -- Se crea el nombre del archivo con la ruta fisica en el servidor
   LET v_ruta_archivo = v_ruta_envio CLIPPED || "/" || v_nom_archivo
   
   DISPLAY "Archivo de salida: ", v_ruta_archivo

   -- se crea el manejador de archivo
   LET v_archivo = base.Channel.create()

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_archivo.openFile(v_ruta_archivo, "w" )
   CALL v_archivo.setDelimiter("")

   
   -- ===============================================================================
   --                                 ENCABEZADO 
   -- ===============================================================================
   -- Valores fijos del encabezado
   LET v_encabezado.tpo_registro        = "01"
   LET v_encabezado.num_secuencia       = "0000001"
   LET v_encabezado.cod_operacion       = "60" -- pago
   LET v_encabezado.banco_participante  = "167" -- TESOFE
   LET v_encabezado.sentido             = "E" -- entrada
   LET v_encabezado.servicio            = "2" -- Transf Electronica
   LET v_encabezado.num_bloque          = DAY(v_f_presentacion) USING "&&","18301" -- ESTE ESTA INCOMPLETO
   LET v_encabezado.cod_divisa          = "01" -- pesos mexicanos
   LET v_encabezado.causa_rechazo       = "00"
   LET v_encabezado.modalidad           = "2"
   LET v_encabezado.uso_futuro_cce      = 41 SPACES
   LET v_encabezado.uso_futuro_banco    = 345 SPACES
   
   -- se escribe el encabezado
   LET v_cadena = v_encabezado.tpo_registro      ,
                  v_encabezado.num_secuencia     ,
                  v_encabezado.cod_operacion     ,
                  v_encabezado.banco_participante,
                  v_encabezado.sentido           ,
                  v_encabezado.servicio          ,
                  v_encabezado.num_bloque        ,
                  v_encabezado.f_presentacion    ,
                  v_encabezado.cod_divisa        ,
                  v_encabezado.causa_rechazo     ,
                  v_encabezado.modalidad         ,
                  v_encabezado.uso_futuro_cce    ,
                  v_encabezado.uso_futuro_banco  
				   
   CALL v_archivo.writeLine(v_cadena)

   LET v_fecha = TODAY
   LET v_hora = CURRENT HOUR TO SECOND
   
   -- Se guarda información del encabezado
   INSERT INTO ret_cza_envio_siaff VALUES (  v_folio,
                                             v1_nom_archivo,
                                             v_fecha,
                                             v_hora,
                                             v_encabezado.num_secuencia,
                                             v_encabezado.banco_participante,
                                             v_encabezado.num_bloque,
                                             v_f_presentacion)
   
   -- ===============================================================================
   --                                 DETALLE
   -- ===============================================================================
   -- se inicia la secuencia
   LET v_secuencia_detalle = 2
   
   -- se asignan los datos fijos del detalle
   LET v_detalle.tpo_registro             = "02" -- indicador de detalle
   LET v_detalle.cod_operacion            = "60" -- pago
   LET v_detalle.cod_divisa               = "01" -- pesos MX
   LET v_detalle.f_transferencia          = v_fecha_pago USING "yyyymmdd"
   LET v_f_transferencia                  = v_fecha_pago
   LET v_detalle.banco_presentador        = "167" -- TESOFE
   LET v_banco_presentador                = 167   -- TESOFE
   LET v_detalle.uso_futuro_cce           = 16 SPACES
   LET v_detalle.tpo_operacion            = "02" -- entrega de Viv97
   LET v_detalle.f_aplicacion             = v_fecha_pago USING "yyyymmdd"
   LET v_f_aplicacion                     = v_fecha_pago
   LET v_detalle.tpo_cuenta_ordenante     = "40"
   LET v_detalle.num_cuenta_ordenante     = "00001180228001000108"
   LET v_detalle.nombre_ordenante         = "TESORERIA DE LA FEDERACION"
   LET v_detalle.rfc_curp_ordenante       = "SHC850101U37"
   LET v_detalle.tpo_cuenta_receptor      = "40"
   LET v_detalle.referencia_servicio      = 40 SPACES
   LET v_detalle.nombre_titular_servicio  = 40 SPACES
   LET v_detalle.importe_iva              = "000000000000000"
   LET v_detalle.referenca_leyenda_ord    = "INFONAVIT DEVOLUCION SUBCUENTA VIV 97   "
   LET v_detalle.motivo_devolucion        = "00"
   LET v_detalle.f_presentacion_inicial   = v_f_presentacion USING "yyyymmdd"
   LET v_f_presentacion_inicial           = v_f_presentacion
   LET v_detalle.solicitud_confirmacion   = "1"
   LET v_detalle.uso_futuro_banco         = 11 SPACES
   
   
   -- se consultan las solicitudes listas para pago
   LET v_consulta_detalle =   " SELECT rs.id_solicitud              ,              ",
                              "        rs.rfc                       ,              ",
                              "        r73.importe_viv97_anexo1     ,              ",
                              "        rs.nss                                      ",
                              " FROM   ret_solicitud_generico rs    ,              ",
                              "        ret_ley73_generico  r73                     ",
                              " WHERE  rs.estado_solicitud      IN (60,61)         ",
                              " AND    rs.id_solicitud          = r73.id_solicitud ",
                              " AND    r73.gpo_ley73            = 4                ",
                              " AND    r73.importe_viv92        = 0                ",
                              " AND    r73.importe_viv97        = 0                ",
                              " AND    r73.importe_viv97_anexo1 > 0                "

   -- se prepara y ejecuta la consulta de detalle
   PREPARE exe_consulta_detalle FROM v_consulta_detalle
   DECLARE cur_consulta_detalle CURSOR FOR exe_consulta_detalle

   FOREACH cur_consulta_detalle INTO v_id_solicitud             ,
                                     v_detalle.rfc_curp_receptor,
                                     v_monto                    ,
                                     v_nss                           --NSS

      INITIALIZE v_cuenta_clabe,v_nom_titular TO NULL

      LET v_cuenta_regs = 0

      --Realizare las siguientes validaciones:
      --   a) Que el registro tenga cuenta CLABE en la tabla ret_cuenta_clabe
      --   b) Que el NSS encontrado solo este en un registro (doy por hecho que es el encontrado)
      --   c) Que el registro solo tenga una cuenta CLABE asociada

      --Reviso la cuenta por NSS, dado que puede estar duplicado
--      SELECT COUNT(*)
--      INTO v_cuenta_regs
--      FROM ret_cuenta_clabe
--      WHERE nss = v_nss
      -- Se cambia la tabla de donde se toma la cuenta clabe req SACI2018-124
      SELECT COUNT(*)
      INTO v_cuenta_regs
      FROM ret_pago_siaf
      WHERE id_solicitud = v_id_solicitud
      
      IF v_cuenta_regs IS NULL THEN LET v_cuenta_regs = 0 END IF

      CASE
      
         WHEN v_cuenta_regs = 0
         
            --No existen registros con el NSS (Caso a), actualizo el registro
            --Se cambia el estado de la solicitud a Pendiente de Envio
            CALL fn_cambia_estado_solicitud_ley73(gi_estado_pendiente_envio,v_id_solicitud)

            --Guardo en la tabla ret_ctr_envio_siaf
            CALL fn_inserta_ret_ctr_envio_siaff(v_folio,v_id_solicitud,gi_estado_pendiente_envio,gi_cod_rech_605,v_fecha)

            DISPLAY "Registro no integrado. Causa: El NSS no tiene cuenta CLABE registrada del IMSS. NSS: ",v_nss
            CONTINUE FOREACH
            
         WHEN v_cuenta_regs = 1

            --Se toma que existe solo un registro para el NSS
            --Reviso que la Cuenta CLABE encontrada no tenga mas de un NSS asociado
            --SELECT clabe, nom_titular
            --INTO v_cuenta_clabe, v_nom_titular
            --FROM ret_cuenta_clabe
            --WHERE nss = v_nss
            
            SELECT rps.cuenta_clabe, TRIM(ad.nombre_af) || ' ' ||
                                     TRIM(ad.ap_paterno_af) || ' ' ||
                                     TRIM(ad.ap_materno_af) AS nom_titular
            INTO   v_cuenta_clabe, v_nom_titular
            FROM   ret_pago_siaf rps,
                   ret_solicitud_generico rsg,
                   afi_derechohabiente ad
            WHERE  rps.id_solicitud       = rsg.id_solicitud
            AND    rsg.id_derechohabiente = ad.id_derechohabiente
            AND    rps.id_solicitud       = v_id_solicitud
                                    

--            SELECT COUNT(*)
--            INTO v_cuenta_regs
--            FROM ret_pago_siaf
--            WHERE cuenta_clabe = v_cuenta_clabe

            SELECT COUNT(*)
            INTO v_cuenta_regs
            FROM   ret_pago_siaf a, 
                   ret_solicitud_generico b
            WHERE  a.id_solicitud = b.id_solicitud
            AND    a.cuenta_clabe = v_cuenta_clabe
            AND    b.nss <> v_nss

            IF v_cuenta_regs > 0 THEN
               --Existe mas de un registro con la cuenta CLABE encontrada (Caso b)
               DISPLAY "Registro no integrado. Causa: La cuenta CLABE encontrada esta siendo utilizada por mas de un NSS, NSS: ",v_nss

               --Se cambia el estado de la solicitud a Pendiente de Envio
               CALL fn_cambia_estado_solicitud_ley73(gi_estado_pendiente_envio,v_id_solicitud)

               --Guardo en la tabla ret_ctr_envio_siaf
               CALL fn_inserta_ret_ctr_envio_siaff(v_folio,v_id_solicitud,gi_estado_pendiente_envio,gi_cod_rech_606,v_fecha)
            
               CONTINUE FOREACH

            ELSE
            
               --Paso todas las validaciones
               --Se cambia el estado de la solicitud a Enviada a TESOFE
               CALL fn_cambia_estado_solicitud_ley73(gi_estado_enviada_tesofe,v_id_solicitud)

               EXIT CASE
               
            END IF

         WHEN v_cuenta_regs > 0

            --El registro tiene mas de cuenta CLABE asociada (Caso c)
            DISPLAY "Registro no integrado. Causa: El NSS existe mas de una vez en la carga del archivo del IMSS. NSS:",v_nss

            --Se cambia el estado de la solicitud a Pendiente de Envio
            CALL fn_cambia_estado_solicitud_ley73(gi_estado_pendiente_envio,v_id_solicitud)

            --Guardo en la tabla ret_ctr_envio_siaf
            CALL fn_inserta_ret_ctr_envio_siaff(v_folio,v_id_solicitud,gi_estado_pendiente_envio,gi_cod_rech_607,v_fecha)
            
            CONTINUE FOREACH
            
      END CASE
      
      -- se genera el nombre del beneficiario
      LET v_nombre_beneficiario = v_nom_titular CLIPPED
									 
      -- se asignan los datos al registro de detalle
      LET v_detalle.num_secuencia            = v_secuencia_detalle USING "&&&&&&&"
      LET v_detalle.banco_receptor           = v_cuenta_clabe[1,3] -- 3 ultimos digitos DE LA CLAVE DE LA INSTITUCION DEL CATALOGO DE INSTITUCIONES FINANCIERAS
      LET v_banco_receptor                   = v_cuenta_clabe[1,3]
      LET v_detalle.rfc_curp_receptor        = v_nss USING '&&&&&&&&&&&&&&&&&&'
      LET v_detalle.importe_operacion        = (v_monto * 100) USING '&&&&&&&&&&&&&&&'
      LET v_detalle.num_cuenta_receptor      = (v_cuenta_clabe CLIPPED) USING '&&&&&&&&&&&&&&&&&&&&'
      LET v_detalle.nombre_receptor          = v_nombre_beneficiario
      LET v_detalle.num_referencia_ord       = (v_secuencia_detalle-1) USING "&&&&&&&" -- Se agregará consecutivo del detalle
      LET v_consecutivo_detalle = v_consecutivo_detalle + 1
      LET v_detalle.clave_rastreo            = fn_genera_clave(v_f_presentacion, v_consecutivo_detalle)

      -- se concatenan los datos del detalle
      LET v_cadena = v_detalle.tpo_registro             ,
                     v_detalle.num_secuencia            , 
                     v_detalle.cod_operacion            ,
                     v_detalle.cod_divisa               , 
                     v_detalle.f_transferencia          , 
                     v_detalle.banco_presentador        , 
                     v_detalle.banco_receptor           , 
                     v_detalle.importe_operacion        , 
                     v_detalle.uso_futuro_cce           , 
                     v_detalle.tpo_operacion            , 
                     v_detalle.f_aplicacion             , 
                     v_detalle.tpo_cuenta_ordenante     , 
                     v_detalle.num_cuenta_ordenante     , 
                     v_detalle.nombre_ordenante         , 
                     v_detalle.rfc_curp_ordenante       , 
                     v_detalle.tpo_cuenta_receptor      , 
                     v_detalle.num_cuenta_receptor      , 
                     v_detalle.nombre_receptor          , 
                     v_detalle.rfc_curp_receptor        , 
                     v_detalle.referencia_servicio      , 
                     v_detalle.nombre_titular_servicio  , 
                     v_detalle.importe_iva              , 
                     v_detalle.num_referencia_ord       , 
                     v_detalle.referenca_leyenda_ord    , 
                     v_detalle.clave_rastreo            , 
                     v_detalle.motivo_devolucion        , 
                     v_detalle.f_presentacion_inicial   , 
                     v_detalle.solicitud_confirmacion   , 
                     v_detalle.uso_futuro_banco          

      -- se escribe el registro de detalle en el archivo
      CALL v_archivo.writeLine(v_cadena)

      -- Se guarda información del detalle
      INSERT INTO ret_det_envio_siaff VALUES (  v_id_solicitud,
                                                v_folio,
                                                v_secuencia_detalle, -- v_detalle.num_secuencia,
                                                v_f_transferencia, -- v_detalle.f_transferencia,
                                                v_banco_presentador, -- v_detalle.banco_presentador,
                                                v_banco_receptor, -- v_detalle.banco_receptor,
                                                v_monto, -- v_detalle.importe_operacion,
                                                v_f_aplicacion, -- v_detalle.f_aplicacion,
                                                v_detalle.num_cuenta_ordenante,
                                                v_detalle.nombre_ordenante,
                                                v_detalle.rfc_curp_ordenante,
                                                v_detalle.num_cuenta_receptor,
                                                v_detalle.nombre_receptor,
                                                v_detalle.rfc_curp_receptor,
                                                v_detalle.num_referencia_ord,
                                                v_detalle.referenca_leyenda_ord,
                                                v_detalle.clave_rastreo,
                                                v_f_presentacion_inicial, -- v_detalle.f_presentacion_inicial,
                                                v_detalle.solicitud_confirmacion,
                                                v_estado_solicitud)      --Estado nuevo de la solicitud

      -- monto sale de la tabla de solicitudes del retiro
      SELECT importe_viv97_anexo1
      INTO   v_monto_pesos
      FROM   ret_ley73_generico
      WHERE  id_solicitud = v_id_solicitud

	  -- se incrementa el monto total
      LET v_monto_total = v_monto_total + v_monto
	  
	  -- se incrementa la secuencia
	  LET v_secuencia_detalle = v_secuencia_detalle + 1

   END FOREACH -- detalle

   -- se asignan los datos del registro de sumario
   LET v_sumario.tpo_registro     = "09" -- reg de sumario
   LET v_sumario.num_secuencia    = v_secuencia_detalle USING "&&&&&&&" -- no se incrementa porque el foreach lo hizo
   LET v_sumario.cod_operacion    = "60"
   LET v_sumario.num_bloque       = v_encabezado.num_bloque
   LET v_sumario.num_operaciones  = (v_secuencia_detalle - 2) USING "&&&&&&&"
   LET v_sumario.importe_total    = (v_monto_total * 100) USING "&&&&&&&&&&&&&&&&&&"
   LET v_sumario.uso_futuro_cce   = 40 SPACES
   LET v_sumario.uso_futuro_banco = 339 SPACES

   -- se concatenan los campos del registro de sumario
   LET v_cadena = v_sumario.tpo_registro    ,
                  v_sumario.num_secuencia   ,
                  v_sumario.cod_operacion   ,
                  v_sumario.num_bloque      ,
                  v_sumario.num_operaciones ,
                  v_sumario.importe_total   ,
                  v_sumario.uso_futuro_cce  ,
                  v_sumario.uso_futuro_banco
   
   -- se escribe el registro de sumario
   CALL v_archivo.writeLine(v_cadena)

   -- Se guarda información del sumario
   INSERT INTO ret_sum_envio_siaff VALUES (  v_folio,
                                             v_secuencia_detalle, -- v_sumario.num_secuencia,
                                             v_secuencia_detalle - 2, -- v_sumario.num_operaciones,
                                             v_monto_total) -- v_sumario.importe_total

   -- se cierra el archivo
   CALL v_archivo.close()
   
   DISPLAY "___________________________________________________________________"
   DISPLAY ""
   DISPLAY "Termino la generacion del archivo de saldos: "
   DISPLAY ""
   DISPLAY " PROCESO            : ",v_proceso_desc
   DISPLAY " OPERACIÓN          : ",v_opera_desc
   DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY ""
   DISPLAY "___________________________________________________________________"


   -- se devuelve cero indicando que finalizo correctamente
   RETURN 0
END FUNCTION


#Esta clave deberá constar de 26 caracteres, más cuatro espacios en blanco a la derecha para completar las 30 posiciones.
#Formato de los 26 caracteres de la clave:
#                                            PPYYYYRROOOJJJQQNNN999999D

#Nomenclatura:
#  PP       =  Código de Pago; para pagos de INFONAVIT será 80 (constante)
#  YYYY     =  Año Calendario ( en nuestro caso es siempre 2013)
#  RR       =  Ramo Generador de Pago, para INFONAVIT  (06)
#  OOO      =  Oficina según Catálogo de Oficinas Generadoras de Pago. para INFONAVIT (183)
#  JJJ      =  Día consecutivo del año correspondiente a la fecha de presentación (001=1 de enero; 032=1 de febrero; 365=31 de diciembre de año no bisiesto. 
#  QQ       =  Número de quincena que se paga, en nuestro caso siempre colocamos (01)
#  NNN      =  Tipo de pago, para pagos de INFONAVIT será (001) 
#  999999   =  Número consecutivo por día, cambia si generas más de un archivo al día
#  D        =  Dígito Verificador (calculado) 

FUNCTION fn_genera_clave(p_fecha, p_referencia_numerica)
DEFINE v_clave             CHAR(25),
       v_respuesta         CHAR(30),
       v_dia               SMALLINT,
       v_funcion_rastreo   STRING,
       p_fecha             DATE,
       p_referencia_numerica SMALLINT

   LET v_funcion_rastreo = "EXECUTE FUNCTION fn_genera_clave_rastreo(?)"
   
   PREPARE exe_funcion_rastreo FROM v_funcion_rastreo
   
   LET v_dia = (p_fecha - MDY(1,1,YEAR(TODAY))) + 1
   LET v_clave =  '80',
                  TODAY USING 'yyyy',
                  '06',
                  '183',
                  v_dia USING '&&&',
                  '01',
                  '001',
                  p_referencia_numerica USING '&&&&&&'

   EXECUTE exe_funcion_rastreo USING v_clave
                               INTO v_respuesta
                               
   RETURN v_respuesta
   
END FUNCTION

# Funcion que determina la fecha de presentacion, la fecha parametro revisa que 
# no sea en fin de semana y tampoco sea un dia inhabil, en caso de ser así, se
# regresa un día

FUNCTION fn_determina_fecha_presentacion(v_fecha)
   DEFINE
        v_fecha         DATE,
        es_correcta     SMALLINT
   LET es_correcta = FALSE

   WHILE (NOT es_correcta)
      IF WEEKDAY(v_fecha) = 0 OR WEEKDAY(v_fecha) = 6 THEN
         LET v_fecha = v_fecha - 1
      ELSE
         SELECT "ok"
         FROM   cat_feriado
         WHERE  feriado_fecha = v_fecha
         GROUP BY 1
         
         IF sqlca.sqlcode = NOTFOUND THEN
            LET es_correcta = TRUE
         ELSE
            LET v_fecha = v_fecha - 1
         END IF
      END IF
   END WHILE

   RETURN v_fecha
   
END FUNCTION

--Función que actualiza un estado en las tablas ret_solicitud_generico y ret_ley73_generico
FUNCTION fn_cambia_estado_solicitud_ley73(p_estado_solicitud,p_id_solicitud)
   DEFINE
      p_estado_solicitud   SMALLINT,
      p_id_solicitud       DECIMAL(9,0)

   --Se actualiza la solicitud al estado solicitado
   UPDATE   ret_solicitud_generico
   SET      estado_solicitud = p_estado_solicitud
   WHERE    id_solicitud     = p_id_solicitud

   UPDATE   ret_ley73_generico
   SET      estado_solicitud = p_estado_solicitud
   WHERE    id_solicitud     = p_id_solicitud
   
END FUNCTION

--Funcion que guarda datos en la tabla ret_ctr_envio_siaf
FUNCTION fn_inserta_ret_ctr_envio_siaff(p_folio,p_id_solicitud,p_estado_solicitud,p_cod_rechazo,p_f_proceso_envio)
   DEFINE
      p_folio              DECIMAL(9,0),
      p_id_solicitud       DECIMAL(9,0),
      p_estado_solicitud   SMALLINT,
      p_cod_rechazo        SMALLINT,
      p_f_proceso_envio    DATE
   INSERT INTO ret_ctr_envio_siaff VALUES (p_folio,p_id_solicitud,p_estado_solicitud,p_cod_rechazo,p_f_proceso_envio)
END FUNCTION

