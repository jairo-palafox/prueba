--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
##############################################################################################
#Modulo            =>RET                                                                     #
#Programa          =>RETS21                                                                  #
#Objetivo          =>Programa que ejecuta el proceso de generacion                           #
#                    de archivo de salida de retiros por                                     #
#                    transferencia para PROCESAR                                             #
#Modificado                                                                                  #
# Ivan Vega     09/sep/2013  - Se generara una copia de los archivos para procesar y trm     #
#                              segun lo solicitado en PRODINF-64                             #
#                              Adicionalmente se cambia el encabezado en el archivo de salida#
#                              de los archivos que van para TRM pues la clave destino debe   #
#                              ser la adecuada para TRM                                      #
# Ivan Vega      24/Sep/2013 - Se cambia la V por S en el nombre del archivo de salida para  #
#                              PROCESAR - Req. PRODINF-96                                    # 
# Ivan Vega      11/Feb/2014 - Se agregan AIVs aceptadas viv97 para los casos cuando         #
#                              se pago menos monto de lo que se habia solicitado             # 
#                            - Los NSS que se aceptan con la diferencia se graban como 03 en #
#                              el resultado de la operacion del registro en el archivo
#                            - Se crea un archivo copia en directorio ret/transfer segun
#                              requerimiento PRODINF-159 del archivo que se envia a PROCESAR  
# Ivan Vega      13 Feb 2014 - Se revisan los NSS que tuvieron hasta 1 AIV de diferencia entre
#                              el monto solicitado y saldo poseido y son informados con
#                              diagnostico 03 a PROCESAR segun requerimiento PRODINF-159
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
   DEFINE p_usuario_cod       LIKE seg_usuario.usuario_cod, -- nombre del usuario        
          p_folio             LIKE glo_folio.folio, -- numero de folio
          p_incluye_rechazos  SMALLINT, -- booleana que indica que se incluyan los registros rechazados
          p_es_previo         SMALLINT -- booleana indica si el archivo es previo

   -- se reciben los parametros del programa
   LET p_folio            = ARG_VAL(1)
   LET p_usuario_cod      = ARG_VAL(2)
   LET p_incluye_rechazos = ARG_VAL(3)
   LET p_es_previo        = ARG_VAL(4)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".RETS21.log")
   
   -- Llamado a función que genera el archivo de salida
   CALL fn_archivo_salida( p_folio, p_usuario_cod, p_incluye_rechazos, p_es_previo)
END MAIN

-- Funcion que se encarga de crear el archivo de salida
-- recibe como parametro la fecha inicial y final para tomar el rango de salida
FUNCTION fn_archivo_salida( p_folio, p_usuario_cod, p_incluye_rechazos, p_es_previo)
DEFINE p_folio                      LIKE glo_folio.folio,
       p_usuario_cod                LIKE seg_usuario.usuario_cod,      
       v_v_nom_archivo              STRING, -- nombre del archivo de salida
       v_v_nom_archivo_copia        STRING, -- nombre del archivo de salida
       v_v_ruta_nomarch             STRING, -- ruta y nombre del archivo de salida
       v_v_ruta_nomarch_copia       STRING, -- ruta y nombre del archivo de salida
       v_c_ruta_env_acr             LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
       v_ch_arch_disposicion        BASE.CHANNEL, -- manejador de apuntador hacia archivo
       v_ch_arch_disposicion_copia  BASE.CHANNEL, -- manejador de apuntador hacia archivo
       v_s_registro                 STRING, -- registro a insertar
       p_incluye_rechazos           SMALLINT,
       p_es_previo                  SMALLINT, -- indica si el archivo es previo
       -- registro de salida
       r_tmp_ret_det_transferencia  RECORD     
	    id_solicitud                 DECIMAL(9,0),
        tpo_registro                 CHAR(2) ,
        id_servicio                  CHAR(2) ,
        id_operacion                 CHAR(2) ,
        nss                          CHAR(11),
        curp                         CHAR(18),
        nombre_trab_datam            CHAR(50),
        nombre_afore                 CHAR(40),
        paterno_afore                CHAR(40),
        materno_afore                CHAR(40),
        sec_pension                  CHAR(2) ,
        tpo_movimiento               CHAR(3) ,
        regimen                      CHAR(2) ,
        tpo_retiro                   CHAR(1) ,
        tpo_seguro                   CHAR(2) ,
        tpo_pension                  CHAR(2) ,
        tpo_prestacion               CHAR(2) ,
        f_inicio_pension             DATE    ,
        f_emision_resol              DATE    ,
        porc_valuacion               DECIMAL(5,0),
        sem_cotizadas                DECIMAL(4,0),
        f_carga_datamart             DATE,
        diagnostico_reg              CHAR(3),
        estatus_subcta               CHAR(1),
        periodo_pago                 DECIMAL(6,0) ,
        acciones_ret97               DECIMAL(14,0),
        acciones_cv                  DECIMAL(14,0),
        acciones_cuotsol             DECIMAL(14,0),
        filler1                      CHAR(8)      ,
        aiv97                        DECIMAL(14,0),
        filler2                      CHAR(18)     ,
        filler3                      CHAR(15)     ,
        result_operacion             CHAR(2)      ,
        cve_afore                    DECIMAL(3,0) ,   
        motivo_rech1                 CHAR(3),
        motivo_rech2                 CHAR(3),
        -- 13feb2014. Se agregan este campo para ver diferencias
        aiv97_con_dif              DECIMAL(14,0) -- aivs viv 97 aceptadas hasta con 1 aiv de diferencia
       END RECORD,

       r_tmp_ret_cza_transferencia   RECORD 
        tpo_registro           CHAR(2),             
        id_servicio            CHAR(2),                
        tpo_entidad_origen     CHAR(2),                
        cve_entidad_origen     CHAR(3),                
        tpo_entidad_destino    CHAR(2),                
        cve_entidad_destino    CHAR(3),                
        f_operacion            DATE   ,                
        f_valor_transferencia  DATE   ,                
        val_aplicacion_aivs    DECIMAL(14,0),          
        resultado_operacion    CHAR(2),                
        motivo_rech_1          CHAR(3),
        motivo_rech_2          CHAR(3),
        motivo_rech_3          CHAR(3),
        filler                 CHAR(315)
       END RECORD,
       r_tmp_ret_sum_transferencia RECORD
          tpo_registro          CHAR(2),        
          id_servicio           CHAR(2),        
          tpo_entidad_origen    CHAR(2),        
          cve_entidad_origen    CHAR(3),        
          tpo_entidad_destino   CHAR(2),        
          cve_entidad_destino   CHAR(3),        
          f_operacion           CHAR(8),        
          total_registros       INTEGER,
          filler                CHAR(342)       
       END RECORD,
       r_ret_preliquida    RECORD 
         f_liquida            DATE         ,
         id_derechohabiente   DECIMAL(9,0) ,
         subcuenta            SMALLINT     ,
         fondo_inversion      SMALLINT     ,
         movimiento           SMALLINT     ,
         folio_liquida        DECIMAL(9,0) ,
         id_referencia        DECIMAL(9,0) ,
         monto_acciones       DECIMAL(20,2),
         monto_pesos          DECIMAL(20,2),
         f_valor              DATE         ,
         f_registro           DATE         ,
         h_registro           DATETIME HOUR TO SECOND,
         origen               CHAR(20)
       END RECORD,
       r_ret_transferencia_rch RECORD LIKE ret_transferencia_rch.*,
       v_s_encabezado         STRING,
       v_d_total_registro     LIKE ret_cza_disposicion.total_registros, 
       v_dtotal_importe       LIKE ret_cza_disposicion.total_importe,
       v_c_usuario            LIKE ret_cza_disposicion.usuario,
       v_s_detalle            STRING,
       v_id_derechohabiente   LIKE ret_disposicion.id_derechohabiente,
       v_i_contador_registros INTEGER,
       v_s_sumario            STRING,
       v_c_fecha              CHAR(8),
       v_i_secuencia          INTEGER,       
       v_f_operacion            CHAR(8),
       v_f_valor_transferencia  CHAR(8),
       v_f_inicio_pension       CHAR(8),
       v_f_resolucion           CHAR(8),
       v_f_nacimiento           CHAR(8),
       v_f_solicitud_trab       CHAR(8),       
       v_f_operacion_cza        CHAR(8),
       v_f_carga_datamart       CHAR(8),
       v_sql                    STRING,
       v_id_ret_matriz_derecho  SMALLINT,
       v_archivo_original       LIKE glo_ctr_archivo.nombre_archivo,
       v_dif_viv92              DECIMAL(8,6), -- para calcular diferencias entre lo solicitado y lo pagado
       v_dif_viv97              DECIMAL(8,6),
       v_archivo_transfer       STRING, -- cadena auxiliar
       v_ejecuta_sh             STRING,
       v_resultado_copia        INTEGER -- resultado de ejecutar la copia de archivo


   INITIALIZE v_s_encabezado TO NULL
   INITIALIZE v_s_sumario TO NULL
   INITIALIZE v_s_detalle TO NULL
  
   LET v_i_contador_registros = 0

   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio 
   INTO   v_c_ruta_env_acr
   FROM   seg_modulo
   WHERE  modulo_cod = 'ret'

   -- se obtiene el nombre del archivo original
   SELECT nombre_archivo
   INTO   v_archivo_original
   FROM   glo_ctr_archivo
   WHERE  proceso_cod = g_proceso_cod_ret_transferencia
   AND    folio = p_folio
   
   -- si incluye aceptados y rechazados
   -- el nombre original va como sigue:
   -- fecha   Consecutivo
   -- 123456789012
   -- AAAAMMDDCXXX.transf
   -- si es previo
   IF ( p_es_previo ) THEN
      LET v_v_nom_archivo = "/PREVIO_"
      LET v_v_nom_archivo_copia = "/PREVIO_"
   ELSE
      LET v_v_nom_archivo = "/"
      LET v_v_nom_archivo_copia = "/"
   END IF

   IF ( p_incluye_rechazos ) THEN
      -- aceptados y rechazados
      LET v_v_nom_archivo = v_v_nom_archivo, "PRTFT.DP.I04002.S", v_archivo_original[3,8], ".TRANINF.C", v_archivo_original[10,12]
      LET v_v_nom_archivo_copia = v_v_nom_archivo_copia, "PRTFT.DP.I04002.S.TRANINF"
   ELSE
      -- solo aceptados
      LET v_v_nom_archivo = v_v_nom_archivo, "NMRFRET.DSPRM.TRANINF.V", v_archivo_original[3,8]
      LET v_v_nom_archivo_copia = v_v_nom_archivo_copia, "NMRFRET.DSPRM.TRANINF.V "
   END IF

   -- si solo se requeiren aceptados
   IF ( NOT p_incluye_rechazos ) THEN
      -- se verifica que haya registros aceptados
      SELECT COUNT(*)
      INTO   v_i_contador_registros
      FROM   ret_transferencia
      WHERE  cod_rechazo = 0
      AND    folio = p_folio
      
      -- si no hay aceptados, no se genera el archivo y se finaliza la ejecucion
      IF ( v_i_contador_registros IS NULL OR v_i_contador_registros < 1 ) THEN
         DISPLAY "____________________________________________________________"
         DISPLAY "   GENERACION DE ARCHIVO DE SALIDA DE REGISTROS ACEPTADOS"
         DISPLAY ""
         DISPLAY "   A T E N C I O N: No se tienen registros aceptados para el"
         DISPLAY "                    folio ", p_folio
         DISPLAY "                    No se generó archivo de salida."
         DISPLAY "\n____________________________________________________________"
         EXIT PROGRAM
      END IF
   END IF

   LET v_v_ruta_nomarch       = v_c_ruta_env_acr CLIPPED || v_v_nom_archivo
   LET v_v_ruta_nomarch_copia = v_c_ruta_env_acr CLIPPED || v_v_nom_archivo_copia

   DISPLAY "   Archivo        : ", v_v_ruta_nomarch
   DISPLAY "   Archivo [COPIA]: ", v_v_ruta_nomarch_copia
   
   -- se crea el manejador de archivo y su copia
   LET v_ch_arch_disposicion = base.Channel.create()
   LET v_ch_arch_disposicion_copia = base.Channel.create()

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_disposicion.openFile(v_v_ruta_nomarch, "w" )
   CALL v_ch_arch_disposicion_copia.openFile(v_v_ruta_nomarch_copia, "w" )
  
   -- se obtienen el encabezado del archivo
   SELECT
      f_operacion_procesar ,
      f_valor_transferencia, 
      precio_fondo         * 1000000,  --val_participacion
      total_registros      ,
      total_importe        ,
      usuario
   INTO
      r_tmp_ret_cza_transferencia.f_operacion          ,
      r_tmp_ret_cza_transferencia.f_valor_transferencia,
      r_tmp_ret_cza_transferencia.val_aplicacion_aivs  ,
      v_d_total_registro                               ,
      v_dtotal_importe                                 ,
      v_c_usuario
   FROM ret_cza_transferencia 
   WHERE folio = p_folio

   --se asignan variables segun layout 
   LET r_tmp_ret_cza_transferencia.tpo_registro        = "01"
   LET r_tmp_ret_cza_transferencia.id_servicio         = "04"
   
   -- para PROCESAR, incluye rechazados
   IF ( p_incluye_rechazos ) THEN
      --se asignan variables segun layout 
      LET r_tmp_ret_cza_transferencia.tpo_entidad_origen  = "04"
      LET r_tmp_ret_cza_transferencia.cve_entidad_origen  = "002"
      LET r_tmp_ret_cza_transferencia.tpo_entidad_destino = "03"
      LET r_tmp_ret_cza_transferencia.cve_entidad_destino = "001"
      LET r_tmp_ret_cza_transferencia.resultado_operacion = "01"
   ELSE
      -- PARA TRM, no incluye rechazos
      LET r_tmp_ret_cza_transferencia.tpo_entidad_origen  = "03"
      LET r_tmp_ret_cza_transferencia.cve_entidad_origen  = "001"
      LET r_tmp_ret_cza_transferencia.tpo_entidad_destino = "04"
      LET r_tmp_ret_cza_transferencia.cve_entidad_destino = "002"
   END IF

   LET r_tmp_ret_cza_transferencia.resultado_operacion = "01"
   LET r_tmp_ret_cza_transferencia.motivo_rech_1       = "000"
   LET r_tmp_ret_cza_transferencia.motivo_rech_2       = "000"
   LET r_tmp_ret_cza_transferencia.motivo_rech_3       = "000"
   LET r_tmp_ret_cza_transferencia.filler              = 315 SPACES
   
   LET v_f_operacion           = r_tmp_ret_cza_transferencia.f_operacion USING "yyyymmdd"
   LET v_f_valor_transferencia = r_tmp_ret_cza_transferencia.f_valor_transferencia USING "yyyymmdd"
   
   --se asignan los datos del encabezado para escribir en el archivo
   LET v_s_encabezado = r_tmp_ret_cza_transferencia.tpo_registro        ,
                        r_tmp_ret_cza_transferencia.id_servicio         ,   
                        r_tmp_ret_cza_transferencia.tpo_entidad_origen  , 
                        r_tmp_ret_cza_transferencia.cve_entidad_origen  , 
                        r_tmp_ret_cza_transferencia.tpo_entidad_destino , 
                        r_tmp_ret_cza_transferencia.cve_entidad_destino ,   
                        v_f_operacion                                   ,
                        v_f_valor_transferencia                         ,
                        r_tmp_ret_cza_transferencia.val_aplicacion_aivs USING "&&&&&&&&&&&&&&",
                        r_tmp_ret_cza_transferencia.resultado_operacion ,
                        r_tmp_ret_cza_transferencia.motivo_rech_1 ,
                        r_tmp_ret_cza_transferencia.motivo_rech_2 ,
                        r_tmp_ret_cza_transferencia.motivo_rech_3 ,
                        r_tmp_ret_cza_transferencia.filler        

   -- se escribe el encabezado en el archivo 
   CALL v_ch_arch_disposicion.writeLine(v_s_encabezado)
   
   -- encabezado en archivo copia
   CALL v_ch_arch_disposicion_copia.writeLine(v_s_encabezado)
                        
   -- se crea una tabla temporal para alojar los registros de detalle
   LET v_sql = "\nDROP TABLE IF EXISTS tmp_ret_det_transferencia;",
               "\nCREATE TABLE tmp_ret_det_transferencia (",
			   "\nid_solicitud          DECIMAL(9,0) ,",
               "\n tpo_registro         CHAR(2)      ,",
               "\n id_servicio          CHAR(2)      ,",
               "\n id_operacion         CHAR(2)      ,",
               "\n nss                  CHAR(11)     ,",
               "\n curp                 CHAR(18)     ,",
               "\n nombre_trab_datam    CHAR(50)     ,",
               "\n nombre_afore         CHAR(40)     ,",
               "\n paterno_afore        CHAR(40)     ,",
               "\n materno_afore        CHAR(40)     ,",
               "\n sec_pension          CHAR(2)      ,",
               "\n tpo_movimiento       CHAR(3)      ,",
               "\n regimen              CHAR(2)      ,",
               "\n tpo_retiro           CHAR(1)      ,",
               "\n tpo_seguro           CHAR(2)      ,",
               "\n tpo_pension          CHAR(2)      ,",
               "\n tpo_prestacion       CHAR(2)      ,",
               "\n f_inicio_pension     DATE         ,",
               "\n f_emision_resol      DATE         ,",
               "\n porc_valuacion       DECIMAL(5,0) ,",
               "\n sem_cotizadas        DECIMAL(4,0) ,",
               "\n f_carga_datamart     DATE         ,",
               "\n diagnostico_reg      CHAR(3)      ,",
               "\n estatus_subcta       CHAR(1)      ,",
               "\n periodo_pago         DECIMAL(6,0) ,",
               "\n acciones_ret97       DECIMAL(14,0),",
               "\n acciones_cv          DECIMAL(14,0),",
               "\n acciones_cuotsol     DECIMAL(14,0),",
               "\n filler1              CHAR(8)      ,",
               "\n aiv97                DECIMAL(14,0),",
               "\n filler2              CHAR(18)     ,",
               "\n filler3              CHAR(15)     ,",
               "\n result_operacion     CHAR(2)      ,",
               "\n cve_afore            DECIMAL(3,0) ,",   
               "\n motivo_rech1         CHAR(3)      ,",
               "\n motivo_rech2         CHAR(3)      ,",
               "\naiv97_con_dif        DECIMAL(14,0)  ", -- 13feb2014. Se agregan este campo
               ") IN tmp_2_dbs;"
                         
   PREPARE sid_tablatemporal FROM v_sql
   EXECUTE sid_tablatemporal

   -- se obtienen los registros de la tabla de detalle
   LET v_sql = "\n SELECT                     ",
               "\n    a.id_solicitud         ,",
               "\n    b.nss                  ,",
               "\n    a.id_ret_matriz_derecho,",       
               "\n    a.curp                 ,",      
               "\n    a.nombre_datamart      ,",      
               "\n    a.nombre_afore         ,",      
               "\n    a.paterno_afore        ,",      
               "\n    a.materno_afore        ,",
               "\n    a.sec_pension          ,",
               "\n    a.tpo_movimiento       ,",
               "\n    a.f_inicio_pension     ,",
               "\n    a.f_resolucion         ,",
               "\n    a.porcentaje_valuacion * 100,",
               "\n    a.semanas_cotizadas    ,",
               "\n    a.f_carga_datamart     ,",
               "\n    a.diag_registro        ,",
               "\n    a.estado_sub_viv       ,",
               "\n    a.aivs_viv97           * 1000000,",       
               "\n    a.cve_afore            ,",
               "\n    a.cod_rechazo           ",
               "\n FROM ret_transferencia   a,",
               "\n      afi_derechohabiente b ",
               "\n WHERE a.folio =            ", p_folio,
               "\n AND   a.id_derechohabiente = b.id_derechohabiente"
   -- si se no se necesitan los rechazados
   IF ( NOT p_incluye_rechazos ) THEN
      LET v_sql = v_sql, "\n AND a.cod_rechazo = 0"
   END IF
   
   --DISPLAY v_sql

   -- se obtienen los datos de detalle
   PREPARE sid_detalletrans FROM v_sql
   DECLARE cur_salidadetalletransf CURSOR FOR sid_detalletrans

   -- se escribe cada registro de detalle en la tabla temporal
   FOREACH  cur_salidadetalletransf
   INTO r_tmp_ret_det_transferencia.id_solicitud      ,
        r_tmp_ret_det_transferencia.nss               ,
        v_id_ret_matriz_derecho                       ,
   	    r_tmp_ret_det_transferencia.curp              ,
   	    r_tmp_ret_det_transferencia.nombre_trab_datam ,
        r_tmp_ret_det_transferencia.nombre_afore      ,
        r_tmp_ret_det_transferencia.paterno_afore     ,
        r_tmp_ret_det_transferencia.materno_afore     ,
        r_tmp_ret_det_transferencia.sec_pension       ,
        r_tmp_ret_det_transferencia.tpo_movimiento    ,
        r_tmp_ret_det_transferencia.f_inicio_pension  ,  
   	    r_tmp_ret_det_transferencia.f_emision_resol   ,  
   	    r_tmp_ret_det_transferencia.porc_valuacion    , 
        r_tmp_ret_det_transferencia.sem_cotizadas     ,
        r_tmp_ret_det_transferencia.f_carga_datamart  ,
        r_tmp_ret_det_transferencia.diagnostico_reg   ,
        r_tmp_ret_det_transferencia.estatus_subcta    ,   
        r_tmp_ret_det_transferencia.aiv97             , 
        r_tmp_ret_det_transferencia.cve_afore         , 
        r_tmp_ret_det_transferencia.motivo_rech1 

      -- se asignan variables segun  layout      
      LET r_tmp_ret_det_transferencia.tpo_registro      = "03"
      LET r_tmp_ret_det_transferencia.id_servicio       = "04"  
      LET r_tmp_ret_det_transferencia.id_operacion      = "04"
       
      -- se obtienen los datos del tipo de retiro
      SELECT tpo_retiro    ,
             regimen       ,
             tpo_seguro    ,
             tpo_pension   ,
             tpo_prestacion
      INTO  r_tmp_ret_det_transferencia.tpo_retiro     ,
            r_tmp_ret_det_transferencia.regimen        ,
            r_tmp_ret_det_transferencia.tpo_seguro     ,
            r_tmp_ret_det_transferencia.tpo_pension    ,
            r_tmp_ret_det_transferencia.tpo_prestacion  
      FROM  ret_matriz_derecho
      WHERE id_ret_matriz_derecho = v_id_ret_matriz_derecho
       
      LET r_tmp_ret_det_transferencia.acciones_ret97    = 0
      LET r_tmp_ret_det_transferencia.acciones_cv       = 0
      LET r_tmp_ret_det_transferencia.acciones_cuotsol  = 0
      LET r_tmp_ret_det_transferencia.filler1           = 8 SPACES
      LET r_tmp_ret_det_transferencia.filler2           = 18 SPACES
      LET r_tmp_ret_det_transferencia.filler3           = 15 SPACES
      
      LET r_tmp_ret_det_transferencia.periodo_pago      = 0
      
      -- si no esta rechazado
      IF ( r_tmp_ret_det_transferencia.motivo_rech1 = 0 ) THEN
         LET r_tmp_ret_det_transferencia.result_operacion  = "01"
         LET r_tmp_ret_det_transferencia.motivo_rech1 = "000"
      ELSE
         LET r_tmp_ret_det_transferencia.result_operacion  = "02"
         LET r_tmp_ret_det_transferencia.motivo_rech1 = r_tmp_ret_det_transferencia.motivo_rech1 USING "&&&"
      END IF
      LET r_tmp_ret_det_transferencia.motivo_rech2      = "000"

      --- se reinician las diferencias
      LET r_tmp_ret_det_transferencia.aiv97_con_dif = 0
          
      -- cambio 02dic2013. Se agregan los montos aceptados con diferencia de hasta 1 AIV
      -- se verifica si la solicitud aparece como con sobregiro en viv97
      SELECT NVL(saldo_acciones * 1000000,0)
      INTO   r_tmp_ret_det_transferencia.aiv97_con_dif
      FROM   ret_his_saldo
      WHERE  id_solicitud    = r_tmp_ret_det_transferencia.id_solicitud
      AND    subcuenta       = 4 -- viv97
      AND    fondo_inversion = 11
      AND    folio           = p_folio

      LET v_dif_viv97 = (r_tmp_ret_det_transferencia.aiv97 - r_tmp_ret_det_transferencia.aiv97_con_dif) / 1000000
	  
	  -- si se encuentra una diferencia entre 0 y 1 AIV
      -- Se deben reportar con resultado 01 y sin motivo de rechazo
      -- se debe dejar de enviar 03 para los sobregiros  PRODINF-564

--- ***      IF ( v_dif_viv97 > 0 AND v_dif_viv97 <= 1 ) THEN
--- ***         LET r_tmp_ret_det_transferencia.result_operacion = "03"
--- ***         LET r_tmp_ret_det_transferencia.motivo_rech1     = "767" -- viv97
--- ***      END IF
	  
      -- se inserta en la tabla temporal
      INSERT INTO tmp_ret_det_transferencia VALUES ( r_tmp_ret_det_transferencia.* )

   END FOREACH
   
   FREE cur_salidadetalletransf
      
      
   -- si se cinluyen los rechazos, se lee la tabla de rechazos de integracion
   IF ( p_incluye_rechazos ) THEN
   
      DECLARE cur_rchtransferencia CURSOR FOR
      SELECT *
      FROM   ret_transferencia_rch
      WHERE  folio = p_folio
      
      -- para cada registro rechazado
      FOREACH cur_rchtransferencia INTO r_ret_transferencia_rch.*

         -- se asignan los datos al registro de salida
		 LET r_tmp_ret_det_transferencia.id_solicitud       = NULL
         LET r_tmp_ret_det_transferencia.nss                = r_ret_transferencia_rch.nss
         LET r_tmp_ret_det_transferencia.curp               = r_ret_transferencia_rch.curp
         LET r_tmp_ret_det_transferencia.nombre_trab_datam  = r_ret_transferencia_rch.nombre_datamart
         LET r_tmp_ret_det_transferencia.nombre_afore       = r_ret_transferencia_rch.nombre_afore
         LET r_tmp_ret_det_transferencia.paterno_afore      = r_ret_transferencia_rch.paterno_afore
         LET r_tmp_ret_det_transferencia.materno_afore      = r_ret_transferencia_rch.materno_afore
         LET r_tmp_ret_det_transferencia.sec_pension        = r_ret_transferencia_rch.sec_pension
         LET r_tmp_ret_det_transferencia.tpo_movimiento     = r_ret_transferencia_rch.tpo_movimiento
         LET r_tmp_ret_det_transferencia.regimen            = r_ret_transferencia_rch.regimen
         LET r_tmp_ret_det_transferencia.tpo_retiro         = r_ret_transferencia_rch.tpo_retiro
         LET r_tmp_ret_det_transferencia.tpo_seguro         = r_ret_transferencia_rch.tpo_seguro
         LET r_tmp_ret_det_transferencia.tpo_pension        = r_ret_transferencia_rch.tpo_pension
         LET r_tmp_ret_det_transferencia.tpo_prestacion     = r_ret_transferencia_rch.tpo_prestacion
         LET r_tmp_ret_det_transferencia.f_inicio_pension   = r_ret_transferencia_rch.f_inicio_pension
         LET r_tmp_ret_det_transferencia.f_emision_resol    = r_ret_transferencia_rch.f_resolucion
         LET r_tmp_ret_det_transferencia.porc_valuacion     = r_ret_transferencia_rch.porcentaje_valuacion * 100
         LET r_tmp_ret_det_transferencia.sem_cotizadas      = r_ret_transferencia_rch.semanas_cotizadas
         LET r_tmp_ret_det_transferencia.f_carga_datamart   = r_ret_transferencia_rch.f_carga_datamart
         LET r_tmp_ret_det_transferencia.diagnostico_reg    = r_ret_transferencia_rch.diag_registro
         LET r_tmp_ret_det_transferencia.estatus_subcta     = r_ret_transferencia_rch.estado_sub_viv
         LET r_tmp_ret_det_transferencia.aiv97              = r_ret_transferencia_rch.aivs_viv97 * 1000000
         LET r_tmp_ret_det_transferencia.cve_afore          = r_ret_transferencia_rch.cve_afore
         -- registro rechazado
         LET r_tmp_ret_det_transferencia.result_operacion   = "02"
         LET r_tmp_ret_det_transferencia.motivo_rech1       = r_ret_transferencia_rch.cod_rechazo_1 USING "&&&"
         LET r_tmp_ret_det_transferencia.motivo_rech2       = r_ret_transferencia_rch.cod_rechazo_2 USING "&&&"
         
         LET r_tmp_ret_det_transferencia.acciones_ret97    = 0
         LET r_tmp_ret_det_transferencia.acciones_cv       = 0
         LET r_tmp_ret_det_transferencia.acciones_cuotsol  = 0
         LET r_tmp_ret_det_transferencia.filler1           = 8 SPACES
         LET r_tmp_ret_det_transferencia.filler2           = 18 SPACES
         LET r_tmp_ret_det_transferencia.filler3           = 15 SPACES
         
         LET r_tmp_ret_det_transferencia.periodo_pago      = 0
         LET r_tmp_ret_det_transferencia.aiv97_con_dif       = 0 -- sin diferencias
         -- se inserta en la tabla temporal
         INSERT INTO tmp_ret_det_transferencia VALUES ( r_tmp_ret_det_transferencia.* )
     
      END FOREACH
      
      FREE cur_rchtransferencia
   
   END IF
      
   -- se leen los datos de la tabla temporal para escribir en archivo
   DECLARE cur_transferenciatmp CURSOR FOR
   SELECT *
   FROM   tmp_ret_det_transferencia
   
   -- se reinicia el contador de registos
   LET v_i_contador_registros = 0
   
   FOREACH cur_transferenciatmp INTO r_tmp_ret_det_transferencia.*
   
      -- se asignan variables segun  layout 
      LET v_f_inicio_pension = r_tmp_ret_det_transferencia.f_inicio_pension USING "yyyymmdd"
      LET v_f_resolucion     = r_tmp_ret_det_transferencia.f_emision_resol  USING "yyyymmdd"
      LET v_f_carga_datamart = r_tmp_ret_det_transferencia.f_carga_datamart USING "yyyymmdd"
   
      --se construye la cadena de detalle 
      LET v_s_detalle = r_tmp_ret_det_transferencia.tpo_registro      ,
                        r_tmp_ret_det_transferencia.id_servicio       ,
                        r_tmp_ret_det_transferencia.id_operacion      ,
                        r_tmp_ret_det_transferencia.nss               ,
                        r_tmp_ret_det_transferencia.curp              ,
                        r_tmp_ret_det_transferencia.nombre_trab_datam ,
                        r_tmp_ret_det_transferencia.nombre_afore      ,
                        r_tmp_ret_det_transferencia.paterno_afore     ,
                        r_tmp_ret_det_transferencia.materno_afore     ,
                        r_tmp_ret_det_transferencia.sec_pension       ,
                        r_tmp_ret_det_transferencia.tpo_movimiento    ,
                        r_tmp_ret_det_transferencia.regimen           ,
                        r_tmp_ret_det_transferencia.tpo_retiro        ,
                        r_tmp_ret_det_transferencia.tpo_seguro        ,
                        r_tmp_ret_det_transferencia.tpo_pension       ,
                        r_tmp_ret_det_transferencia.tpo_prestacion    , 
                        v_f_inicio_pension                            ,
                        v_f_resolucion                                ,
                        r_tmp_ret_det_transferencia.porc_valuacion USING "&&&&&",
                        r_tmp_ret_det_transferencia.sem_cotizadas  USING "&&&&" ,
                        v_f_carga_datamart                            ,
                        r_tmp_ret_det_transferencia.diagnostico_reg   ,
                        r_tmp_ret_det_transferencia.estatus_subcta    ,
                        r_tmp_ret_det_transferencia.periodo_pago     USING "&&&&&&",
                        r_tmp_ret_det_transferencia.acciones_ret97   USING "&&&&&&&&&&&&&&" ,
                        r_tmp_ret_det_transferencia.acciones_cv      USING "&&&&&&&&&&&&&&" ,
                        r_tmp_ret_det_transferencia.acciones_cuotsol USING "&&&&&&&&&&&&&&" ,
                        r_tmp_ret_det_transferencia.filler1           ,
                        r_tmp_ret_det_transferencia.aiv97            USING "&&&&&&&&&&&&&&" ,
                        r_tmp_ret_det_transferencia.filler2           ,
                        r_tmp_ret_det_transferencia.filler3           ,
                        r_tmp_ret_det_transferencia.result_operacion  ,
                        r_tmp_ret_det_transferencia.cve_afore        USING "&&&" ,
                        r_tmp_ret_det_transferencia.motivo_rech1      ,
                        r_tmp_ret_det_transferencia.motivo_rech2
       
      --se escribe el detalle en el archivo 
      CALL v_ch_arch_disposicion.writeLine(v_s_detalle)
      CALL v_ch_arch_disposicion_copia.writeLine(v_s_detalle)

      --se incrementa el contador de registros 
      LET v_i_contador_registros = v_i_contador_registros + 1 
      
   END FOREACH

      
   -- se asigana variables segun Layout
   LET  r_tmp_ret_sum_transferencia.tpo_registro        = "09"
   LET  r_tmp_ret_sum_transferencia.id_servicio         = "04"

   -- para procesar se incluyen los rechazos
   IF ( p_incluye_rechazos ) THEN
      LET  r_tmp_ret_sum_transferencia.tpo_entidad_origen  = "04"
      LET  r_tmp_ret_sum_transferencia.cve_entidad_origen  = "002"
      LET  r_tmp_ret_sum_transferencia.tpo_entidad_destino = "03"
      LET  r_tmp_ret_sum_transferencia.cve_entidad_destino = "001" 
   ELSE
      -- TRM. sin rechazos
      LET  r_tmp_ret_sum_transferencia.tpo_entidad_origen  = "03"
      LET  r_tmp_ret_sum_transferencia.cve_entidad_origen  = "001"
      LET  r_tmp_ret_sum_transferencia.tpo_entidad_destino = "04"
      LET  r_tmp_ret_sum_transferencia.cve_entidad_destino = "002" 
   END IF

   LET  r_tmp_ret_sum_transferencia.total_registros     = v_i_contador_registros
   LET  r_tmp_ret_sum_transferencia.filler              = 342 SPACES
      
   -- se asigana valores a la cadena de sumario 
   LET v_s_sumario = r_tmp_ret_sum_transferencia.tpo_registro       ,
                     r_tmp_ret_sum_transferencia.id_servicio        ,
                     r_tmp_ret_sum_transferencia.tpo_entidad_origen ,
                     r_tmp_ret_sum_transferencia.cve_entidad_origen ,
                     r_tmp_ret_sum_transferencia.tpo_entidad_destino,
                     r_tmp_ret_sum_transferencia.cve_entidad_destino,
                     v_f_operacion                                  ,
                     r_tmp_ret_sum_transferencia.total_registros    USING "&&&&&&" ,
                     r_tmp_ret_sum_transferencia.filler
       
   -- se escribe el sumario en el archivo                   
   CALL v_ch_arch_disposicion.writeLine(v_s_sumario)
   
   -- se escribe el sumario en el archivo copia
   CALL v_ch_arch_disposicion_copia.writeLine(v_s_sumario)
  
   -- se cierra el archivo   
   CALL v_ch_arch_disposicion.CLOSE()
   CALL v_ch_arch_disposicion_copia.CLOSE()


   -- Se verifica si p_es_previo = 1, lo cual implica que es la integracion
   -- o si es 0, que es preliquidacion (Se pidio que solo se enviara en la preliquidacion)
   IF (p_es_previo = 0) THEN  
       -- generando copia para PROCESAR
       IF ( p_incluye_rechazos ) THEN
          -- ruta del archivo en transfer
          LET v_archivo_transfer = v_c_ruta_env_acr CLIPPED, "/../transfer/PRTFT.DP.I04002.S", v_archivo_original[3,8], ".TRANINF.GDG"
          DISPLAY "\nGenerando copia en directorio transfer: ", v_archivo_transfer
          
          -- se ejecuta el comando para copiar el archivo
          CALL os.Path.copy(v_v_ruta_nomarch, v_archivo_transfer) RETURNING v_resultado_copia
          
          -- si se pudo realizar la copia
          IF ( v_resultado_copia ) THEN
             DISPLAY "\nArchivo en directorio transfer para PROCESAR: ", v_archivo_transfer
             DISPLAY "\n\nEjecutando scripts de transferencia de archivos ../transfer"
             DISPLAY "\n El Script de ejecucion para el envio del archivo es >sh /opt/Interpel/Scripts/variables/PROCESAR/SACI.sh<"
             LET v_ejecuta_sh = " sh /opt/Interpel/Scripts/variables/PROCESAR/SACI.sh"
             RUN v_ejecuta_sh
          ELSE
             DISPLAY "Error al generar archivo copia en directorio transfer... ", v_resultado_copia
          END IF
       ELSE -- generando copia para TRM
          -- ruta del archivo en transfer
          LET v_archivo_transfer = v_c_ruta_env_acr CLIPPED, "/../transfer/NMRFRET.DSPRM.TRANINF.F", v_archivo_original[3,8]
          DISPLAY "\nGenerando copia en directorio transfer: ", v_archivo_transfer
          
          -- se ejecuta el comando para copiar el archivo
          CALL os.Path.copy(v_v_ruta_nomarch, v_archivo_transfer) RETURNING v_resultado_copia
          
          -- si se pudo realizar la copia
          IF ( v_resultado_copia ) THEN
             DISPLAY "\nArchivo en directorio transfer para TRM: ", v_archivo_transfer
             DISPLAY "\n\nEjecutando scripts de transferencia de archivos ../transfer"
             DISPLAY "\n El Script de ejecucion para el envio del archivo es >sh /opt/Interpel/Scripts/variables/TRM/SACI_2.sh<"
             LET v_ejecuta_sh = " sh /opt/Interpel/Scripts/variables/TRM/SACI_2.sh"
             RUN v_ejecuta_sh
          ELSE
             DISPLAY "Error al generar archivo copia en directorio transfer... ", v_resultado_copia
          END IF
       END IF

       #Este shell solo se ejecuta para los archivos de TRM
       IF ( NOT p_incluye_rechazos ) THEN
           --- Ejecuta los scripts para envio a las carpetas de transmision 141021
           DISPLAY "\n\nEjecutando scripts de transferencia de archivos"
           DISPLAY "\n Se ejecuta Script >sh /opt/Interpel/Scripts/TRANINF_SYSR.sh<"

           LET v_ejecuta_sh = " sh /opt/Interpel/Scripts/TRANINF_SYSR.sh"
           RUN v_ejecuta_sh
       END IF 
   END IF -- IF (p_es_previo = 0)
   
END FUNCTION --fn_archivo_salida