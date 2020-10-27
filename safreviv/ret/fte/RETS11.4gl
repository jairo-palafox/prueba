--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
##############################################################################################
#Modulo            =>RET                                                                     #
#Programa          =>RETS11                                                                  #
#Objetivo          =>Genera el archivo de respuesta a PROCESAR de Retiros por Disposicion    #
#                    de Recursos que incluye registros aceptados y rechazados                #
#Modificado                                                                                  #
# Ivan Vega     09/sep/2013  - Se generara una copia de los archivos para procesar y trm     #
#                              segun lo solicitado en PRODINF-64                             #
#                              Adicionalmente se cambia el encabezado en el archivo de salida#
#                              de los archivos que van para TRM pues la clave destino debe   #
#                              ser la adecuada para TRM                                      #
# Ivan Vega      24/Sep/2013 - Se cambia la V por S en el nombre del archivo de salida para  #
#                              PROCESAR - Req. PRODINF-96                                    #
# Ivan Vega      02/Dic/2013 - Se agregan AIVs aceptadas viv92/viv97 para los casos cuando   #
#                              se pago menos monto de lo que se habia solicitado             # 
# Ivan Vega      04/Dic/2013 - Los NSS que se aceptan con la diferencia se graban como 03 en #
#                              el resultado de la operacion del registro en el archivo
# Ivan Vega      10/Feb/2014 - Se crea un archivo copia en directorio ret/transfer segun
#                              requerimiento PRODINF-159 del archivo que se envia a PROCESAR
# Ricardo Perez  22/Oct/2014 - Se implementa la ejecucion del srcipt para envio del archivo
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
       p_incluye_rechazos  SMALLINT, -- booleana que indica que se incluyan los rechazos
       p_es_previo         SMALLINT -- booleana. 1-es archivo previo. 0 es final

   -- se reciben los parametros del programa
   LET p_folio            = ARG_VAL(1)
   LET p_usuario_cod      = ARG_VAL(2)
   LET p_incluye_rechazos = ARG_VAL(3)
   LET p_es_previo        = ARG_VAL(4)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".RETS11.log")
   
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
       v_ch_arch_disposicion        base.channel, -- manejador de apuntador hacia archivo
       v_ch_arch_disposicion_copia  base.channel, -- manejador de apuntador hacia archivo
       p_incluye_rechazos           SMALLINT,
       p_es_previo                  SMALLINT,
       ls_sql                       STRING, -- cadena con enunciado SQL
       r_tmp_ret_det_disposicion    RECORD 
          id_solicitud               DECIMAL(9,0),    
          tpo_registro               CHAR(2),                  
          id_servicio                CHAR(2),                  
          id_operacion               CHAR(2),                  
          nss                        CHAR(11),                 
          curp                       CHAR(18),                 
          nombre_afore               CHAR(40),                 
          paterno_afore              CHAR(40),                 
          materno_afore              CHAR(40),                 
          sec_pension                CHAR(2),                  
          tpo_retiro                 CHAR(1),                  
          regimen                    CHAR(2),                  
          tpo_seguro                 CHAR(2),                  
          tpo_pension                CHAR(2),                  
          tpo_prestacion             CHAR(2),                  
          f_inicio_pension           DATE   ,                  
          f_emision_resol            DATE   ,                  
          porc_valuacion             DECIMAL(5,0),             
          sem_cotizadas              DECIMAL(4,0),             
          f_solicitud_trab           DATE,                     
          cve_doc_probatorio         CHAR(1),                  
          f_nacimiento               DATE,                     
          aseguradora                CHAR(3),                  
          actuario_autor             CHAR(7),                  
          num_reg_ppp                CHAR(8),                  
          periodo_pago               DECIMAL(6,0),             
          acciones_ret97             DECIMAL(14,0),            
          acciones_cv                DECIMAL(14,0),            
          acciones_cuotsol           DECIMAL(14,0),            
          filler1                    CHAR(14),                 
          acciones_ret92             DECIMAL(14,0) ,
          filler2                    CHAR(8),
          aiv97                      DECIMAL(14,0), 
          aiv92                      DECIMAL(14,0), 
          consec_trab                DECIMAL(11,0), 
          fondo_subcta_viv72         DECIMAL(14,0), 
          diagnostico_reg            CHAR(3),
          estatus_subcta             CHAR(1),
          filler3                    CHAR(12),
          result_operacion           CHAR(2),
          cve_afore                  DECIMAL(3,0),
          motivo_rech1               CHAR(3),
          motivo_rech2               CHAR(3),
          -- 02dic2013. Se agregan estos 2 campos
          aiv97_con_dif              DECIMAL(14,0), -- aivs viv 97 aceptadas hasta con 1 aiv de diferencia
          aiv92_con_dif              DECIMAL(14,0)  -- aivs viv 92 aceptadas hasta con 1 aiv de diferencia
       END RECORD

DEFINE r_tmp_ret_cza_dispo  RECORD                                        
          tpo_registro            CHAR(2),                 
          id_servicio             CHAR(2),                 
          tpo_entidad_origen      CHAR(2),                 
          cve_entidad_origen      CHAR(3),                 
          tpo_entidad_destino     CHAR(2),                 
          cve_entidad_destino     CHAR(3),                 
          f_operacion             DATE   ,                 
          f_valor_transferencia   DATE   ,                 
          val_participacion       DECIMAL(14,0),           
          resultado_operacion     CHAR(2),
          motivo_rech_1           CHAR(3),
          motivo_rech_2           CHAR(3),
          motivo_rech_3           CHAR(3),
          filler                  CHAR(335)
END RECORD 
DEFINE  r_tmp_ret_sum_disposicion RECORD
          tpo_registro          CHAR(2),
          id_servicio           CHAR(2),
          tpo_entidad_origen    CHAR(2),
          cve_entidad_origen    CHAR(3),
          tpo_entidad_destino   CHAR(2),
          cve_entidad_destino   CHAR(3),
          f_operacion           CHAR(8),
          total_registros       CHAR(6),
          filler                CHAR(362)  
END RECORD

DEFINE v_s_encabezado         STRING,
       v_d_total_registro     INTEGER,
       v_dtotal_importe       LIKE ret_cza_disposicion.total_importe,
       v_c_usuario            LIKE ret_cza_disposicion.usuario,
       v_s_detalle            STRING,
       v_id_derechohabiente   LIKE ret_disposicion.id_derechohabiente,
       v_i_contador_registros INTEGER,
       v_s_sumario            STRING,
       v_archivo_original     LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo original
       v_f_inicio_pension       CHAR(8),
       v_f_resolucion           CHAR(8),
       v_f_nacimiento           CHAR(8),
       v_f_solicitud_trab       CHAR(8),
       v_f_operacion_cza        CHAR(8),
       v_f_valor_transferencia  CHAR(8),
       v_id_ret_matriz_derecho  SMALLINT,
       v_fecha_formateada       CHAR(6), -- fecha en formato AAMMDD 
       v_dif_viv92              DECIMAL(8,6), -- para calcular diferencias entre lo solicitado y lo pagado
       v_dif_viv97              DECIMAL(8,6),
       v_archivo_transfer       STRING, -- cadena auxiliar
       v_ejecuta_sh             STRING,
       v_resultado_copia        INTEGER -- resultado de ejecutar la copia de archivo

   --se inicializan las variables 
   INITIALIZE v_s_encabezado TO NULL
   INITIALIZE v_s_sumario TO NULL
   INITIALIZE v_s_detalle TO NULL
   LET v_i_contador_registros = 0 

   --DISPLAY "folio =  ",p_folio
   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio 
     INTO v_c_ruta_env_acr
     FROM seg_modulo
    WHERE modulo_cod = 'ret'

   -- se obtiene el nombre del archivo original
   SELECT nombre_archivo
   INTO   v_archivo_original
   FROM   glo_ctr_archivo
   WHERE  proceso_cod = g_proceso_cod_ret_disposicion 
   AND    folio = p_folio
  
   LET v_fecha_formateada = TODAY USING "yymmdd"
   
   -- si incluye aceptados y rechazados
   -- el nombre original va como sigue:
   -- fecha   Consecutivo
   -- 123456789012
   -- 20130619C003.disp
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
      LET v_v_nom_archivo = v_v_nom_archivo, "PRTFT.DP.I04002.S", v_archivo_original[3,8], ".DISPINF.C", v_archivo_original[10,12]
      LET v_v_nom_archivo_copia = v_v_nom_archivo_copia, "PRTFT.DP.I04002.S.DISPINF"
   ELSE
      -- solo aceptados
      LET v_v_nom_archivo = v_v_nom_archivo, "NMRFRET.DSPRM.DISPINF.V", v_archivo_original[3,8]
      LET v_v_nom_archivo_copia = v_v_nom_archivo_copia, "NMRFRET.DSPRM.DISPINF.V"
   END IF
   
   -- si solo se requeiren aceptados
   IF ( NOT p_incluye_rechazos ) THEN
      -- se verifica que haya registros aceptados
      SELECT COUNT(*)
      INTO   v_i_contador_registros
      FROM   ret_disposicion
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
       f_operacion_procesar,
       f_valor_transferencia, 
       (precio_fondo * 1000000),  --val_participacion
       total_registros,
       total_importe,
       usuario
     INTO
       r_tmp_ret_cza_dispo.f_operacion,
       r_tmp_ret_cza_dispo.f_valor_transferencia,
       r_tmp_ret_cza_dispo.val_participacion,
       v_d_total_registro ,
       v_dtotal_importe,
       v_c_usuario
     FROM ret_cza_disposicion
    WHERE folio = p_folio

   -- encabezado de archivo
   LET r_tmp_ret_cza_dispo.tpo_registro = "01"
   LET r_tmp_ret_cza_dispo.id_servicio  = "04"   

   -- para PROCESAR, incluye rechazados
   IF ( p_incluye_rechazos ) THEN
      --se asignan variables segun layout 
      LET r_tmp_ret_cza_dispo.tpo_entidad_origen  = "04"
      LET r_tmp_ret_cza_dispo.cve_entidad_origen  = "002"
      LET r_tmp_ret_cza_dispo.tpo_entidad_destino = "03"
      LET r_tmp_ret_cza_dispo.cve_entidad_destino = "001"
      LET r_tmp_ret_cza_dispo.resultado_operacion = "01"
   ELSE
      -- PARA TRM, no incluye rechazos
      LET r_tmp_ret_cza_dispo.tpo_entidad_origen  = "03"
      LET r_tmp_ret_cza_dispo.cve_entidad_origen  = "001"
      LET r_tmp_ret_cza_dispo.tpo_entidad_destino = "04"
      LET r_tmp_ret_cza_dispo.cve_entidad_destino = "002"
   END IF
   
   LET r_tmp_ret_cza_dispo.resultado_operacion = "01"
   LET r_tmp_ret_cza_dispo.motivo_rech_1 =  "000"
   LET r_tmp_ret_cza_dispo.motivo_rech_2 =  "000"
   LET r_tmp_ret_cza_dispo.motivo_rech_3 =  "000"
   LET r_tmp_ret_cza_dispo.filler        = 335 SPACES

   -- se formatean las fechas                                          
   LET v_f_operacion_cza       =  r_tmp_ret_cza_dispo.f_operacion USING "yyyymmdd"
   LET v_f_valor_transferencia =  r_tmp_ret_cza_dispo.f_valor_transferencia USING "yyyymmdd"
                                           
   --se asignan los datos del encabezado para escribir en el archivo
   LET v_s_encabezado = r_tmp_ret_cza_dispo.tpo_registro        ,
                        r_tmp_ret_cza_dispo.id_servicio         ,
                        r_tmp_ret_cza_dispo.tpo_entidad_origen  ,
                        r_tmp_ret_cza_dispo.cve_entidad_origen  ,
                        r_tmp_ret_cza_dispo.tpo_entidad_destino ,
                        r_tmp_ret_cza_dispo.cve_entidad_destino ,   
                        v_f_operacion_cza                       ,
                        v_f_valor_transferencia                 ,
                        r_tmp_ret_cza_dispo.val_participacion   USING "&&&&&&&&&&&&&&",
                        r_tmp_ret_cza_dispo.resultado_operacion ,
                        r_tmp_ret_cza_dispo.motivo_rech_1       ,
                        r_tmp_ret_cza_dispo.motivo_rech_2       ,
                        r_tmp_ret_cza_dispo.motivo_rech_3       ,
                        r_tmp_ret_cza_dispo.filler 
   
   --se escribe el encabezado en el archivo 
   CALL v_ch_arch_disposicion.writeLine(v_s_encabezado) 
   
   -- encabezado en archivo copia
   CALL v_ch_arch_disposicion_copia.writeLine(v_s_encabezado)
   
   -- se crea una tabla temporal para generar el detalle de los registros
   LET ls_sql = "DROP TABLE IF EXISTS tmp_ret_det_disposicion;",
                    "\n CREATE TABLE tmp_ret_det_disposicion (",
                    "\nid_solicitud         DECIMAL(9,0),     ",
                    "\ntpo_registro         CHAR(2),          ",
                    "\nid_servicio          CHAR(2),          ",
                    "\nid_operacion         CHAR(2),          ",
                    "\nnss                  CHAR(11),         ",
                    "\ncurp                 CHAR(18),         ",
                    "\nnombre_afore         CHAR(40),         ",
                    "\npaterno_afore        CHAR(40),         ",
                    "\nmaterno_afore        CHAR(40),         ",
                    "\nsec_pension          CHAR(2),          ",
                    "\ntpo_retiro           CHAR(1),          ",
                    "\nregimen              CHAR(2),          ",
                    "\ntpo_seguro           CHAR(2),          ",
                    "\ntpo_pension          CHAR(2),          ",
                    "\ntpo_prestacion       CHAR(2),          ",
                    "\nf_inicio_pension     DATE   ,          ",
                    "\nf_emision_resol      DATE   ,          ",
                    "\nporc_valuacion       DECIMAL(5,0),     ",
                    "\nsem_cotizadas        DECIMAL(4,0),     ",
                    "\nf_solicitud_trab     DATE,             ",
                    "\ncve_doc_probatorio   CHAR(1),          ",
                    "\nf_nacimiento         DATE,             ",
                    "\naseguradora          CHAR(3),          ",
                    "\nactuario_autor       CHAR(7),          ",
                    "\nnum_reg_ppp          CHAR(8),          ",
                    "\nperiodo_pago         DECIMAL(6,0),     ",
                    "\nacciones_ret97       DECIMAL(14,0),    ",
                    "\nacciones_cv          DECIMAL(14,0),    ",
                    "\nacciones_cuotsol     DECIMAL(14,0),    ",
                    "\nfiller1              CHAR(14),         ",
                    "\nacciones_ret92       DECIMAL(14,0),    ",
                    "\nfiller2              CHAR(8),          ",
                    "\naiv97                DECIMAL(14,0),    ",
                    "\naiv92                DECIMAL(14,0),    ",
                    "\nconsec_trab          DECIMAL(11,0),    ",
                    "\nfondo_subcta_viv72   DECIMAL(14,0),    ",
                    "\ndiagnostico_reg      CHAR(3),          ",
                    "\nestatus_subcta       CHAR(1),          ",
                    "\nfiller3              CHAR(12),         ",
                    "\nresult_operacion     CHAR(2),          ",
                    "\ncve_afore            DECIMAL(3,0),     ",
                    "\nmotivo_rech1         CHAR(3),          ",
                    "\nmotivo_rech2         CHAR(3),          ",
                    "\naiv97_con_dif        DECIMAL(14,0),    ", -- 02dic2013. Se agregan estos dos campos 
                    "\naiv92_con_dif        DECIMAL(14,0)     ",
                 ") IN tmp_2_dbs;"

   --DISPLAY ls_sql                 
   PREPARE sid_creatabla FROM ls_sql
   EXECUTE sid_creatabla

   -- se insertan los registros procesados en la tabla temporal
   LET ls_sql = "SELECT   id_solicitud      ,       ",
                "\n       id_derechohabiente   ,    ",
                "\n       id_ret_matriz_derecho,    ",
                "\n       sec_pension,              ",
                "\n       curp,                     ",
                "\n       nombre_afore   ,          ",
                "\n       paterno_afore  ,          ",
                "\n       materno_afore  ,          ",
                "\n       f_inicio_pension,         ",
                "\n       f_resolucion,             ",
                "\n       f_solicitud,              ",
                "\n       porcentaje_valuacion *100,",
                "\n       semanas_cotizadas,        ",
                "\n       cve_doc_probatorio,       ",
                "\n       f_nacimiento ,            ",
                "\n       aseguradora ,             ",
                "\n       actuario,                 ",
                "\n       num_plan_privado,         ",
                "\n       periodo_primer_pago ,     ",
                "\n       aivs_viv97   * 1000000,   ",
                "\n       aivs_viv92   * 1000000,   ", 
                "\n       consec_trabajador,        ",
                "\n       importe_viv72 * 100,      ",
                "\n       estado_sub_viv ,          ",
                "\n       cve_afore ,               ",
                "\n       cod_rechazo               ",
                "\n FROM  ret_disposicion           ",
                "\n WHERE folio = "                  , p_folio
                
   -- si no se incluyen los rechazados
   IF ( NOT p_incluye_rechazos ) THEN
      LET ls_sql = ls_sql, "\nAND",
                           "\ncod_rechazo = 0"
   END IF

   -- se prepara la consulta
   PREPARE sid_retdisposicion FROM ls_sql

   DECLARE cur_salidadetalledisposicion CURSOR FOR sid_retdisposicion
   -- se escribe cada registro de detalle
   FOREACH cur_salidadetalledisposicion
   INTO 
      r_tmp_ret_det_disposicion.id_solicitud      ,
   	  v_id_derechohabiente                        ,
   	  v_id_ret_matriz_derecho                     ,
   	  r_tmp_ret_det_disposicion.sec_pension       ,
   	  r_tmp_ret_det_disposicion.curp              ,
   	  r_tmp_ret_det_disposicion.nombre_afore      ,
      r_tmp_ret_det_disposicion.paterno_afore     ,
      r_tmp_ret_det_disposicion.materno_afore     ,
      r_tmp_ret_det_disposicion.f_inicio_pension  , 
      r_tmp_ret_det_disposicion.f_emision_resol   ,
      r_tmp_ret_det_disposicion.f_solicitud_trab  ,
      r_tmp_ret_det_disposicion.porc_valuacion    ,  
      r_tmp_ret_det_disposicion.sem_cotizadas     ,  
      r_tmp_ret_det_disposicion.cve_doc_probatorio, 
      r_tmp_ret_det_disposicion.f_nacimiento      , 
      r_tmp_ret_det_disposicion.aseguradora       , 
      r_tmp_ret_det_disposicion.actuario_autor    , 
      r_tmp_ret_det_disposicion.num_reg_ppp       , 
      r_tmp_ret_det_disposicion.periodo_pago      , 
      r_tmp_ret_det_disposicion.aiv97             , 
      r_tmp_ret_det_disposicion.aiv92             , 
      r_tmp_ret_det_disposicion.consec_trab       , 
      r_tmp_ret_det_disposicion.fondo_subcta_viv72, 
      r_tmp_ret_det_disposicion.estatus_subcta    , 
      r_tmp_ret_det_disposicion.cve_afore         , 
      r_tmp_ret_det_disposicion.motivo_rech1
   
       --se extrae el número de seguridad social 
   	  SELECT nss
   	  INTO   r_tmp_ret_det_disposicion.nss
      FROM   afi_derechohabiente
      WHERE  id_derechohabiente = v_id_derechohabiente
       
      --se asignan variables segun  layout 
      LET r_tmp_ret_det_disposicion.tpo_registro      = "03"
      LET r_tmp_ret_det_disposicion.id_servicio       = "04"  
      LET r_tmp_ret_det_disposicion.id_operacion      = "08"
      LET r_tmp_ret_det_disposicion.acciones_ret97    = 0
      LET r_tmp_ret_det_disposicion.acciones_cv       = 0
      LET r_tmp_ret_det_disposicion.acciones_cuotsol  = 0
      LET r_tmp_ret_det_disposicion.filler1           = 14 SPACES
      LET r_tmp_ret_det_disposicion.acciones_ret92    = 0
      LET r_tmp_ret_det_disposicion.filler2           = 8 SPACES
      LET r_tmp_ret_det_disposicion.filler3           = 12 SPACES

      -- si no esta rechazado
      IF ( r_tmp_ret_det_disposicion.motivo_rech1 = 0 ) THEN
         LET r_tmp_ret_det_disposicion.result_operacion  = "01"
         LET r_tmp_ret_det_disposicion.motivo_rech1 = "000"
      ELSE
         LET r_tmp_ret_det_disposicion.result_operacion  = "02"
         LET r_tmp_ret_det_disposicion.motivo_rech1 = r_tmp_ret_det_disposicion.motivo_rech1 USING "&&&"
      END IF
      LET r_tmp_ret_det_disposicion.motivo_rech2      = "000"
   
      -- se obtienen los datos del tipo de retiro
      SELECT tpo_retiro    ,
             regimen       ,
             tpo_seguro    ,
             tpo_pension   ,
             tpo_prestacion
      INTO  r_tmp_ret_det_disposicion.tpo_retiro     ,
            r_tmp_ret_det_disposicion.regimen        ,
            r_tmp_ret_det_disposicion.tpo_seguro     ,
            r_tmp_ret_det_disposicion.tpo_pension    ,
            r_tmp_ret_det_disposicion.tpo_prestacion  
      FROM  ret_matriz_derecho
      WHERE id_ret_matriz_derecho = v_id_ret_matriz_derecho
          
      --DISPLAY "buscando diferencia para id_solicitud: ", r_tmp_ret_det_disposicion.id_solicitud
          
      --- se reinician las diferencias
      LET r_tmp_ret_det_disposicion.aiv97_con_dif = 0
      LET r_tmp_ret_det_disposicion.aiv92_con_dif = 0
          
      -- cambio 02dic2013. Se agregan los montos aceptados con diferencia de hasta 1 AIV
      -- se verifica si la solicitud aparece como con sobregiro en viv97
      SELECT NVL(saldo_acciones * 1000000,0)
      INTO   r_tmp_ret_det_disposicion.aiv97_con_dif
      FROM   ret_his_saldo
      WHERE  id_solicitud    = r_tmp_ret_det_disposicion.id_solicitud
      AND    subcuenta       = 4 -- viv97
      AND    fondo_inversion = 11
      AND    folio           = p_folio

      -- se verifica si la solicitud aparece como con sobregiro en viv92
      SELECT NVL(saldo_acciones * 1000000,0)
      INTO   r_tmp_ret_det_disposicion.aiv92_con_dif
      FROM   ret_his_saldo
      WHERE  id_solicitud    = r_tmp_ret_det_disposicion.id_solicitud
      AND    subcuenta       = 8 -- viv92
      AND    fondo_inversion = 11
      AND    folio           = p_folio

      -- si se registro que hubo diferencia en el saldo
      IF ( r_tmp_ret_det_disposicion.aiv92_con_dif <> 0 OR r_tmp_ret_det_disposicion.aiv97_con_dif <> 0 ) THEN
         -- se calcula la diferencia
         LET v_dif_viv92 = (r_tmp_ret_det_disposicion.aiv92 - r_tmp_ret_det_disposicion.aiv92_con_dif) / 1000000
         LET v_dif_viv97 = (r_tmp_ret_det_disposicion.aiv97 - r_tmp_ret_det_disposicion.aiv97_con_dif) / 1000000
         
         --DISPLAY "Dif viv97: ", v_dif_viv97
         --DISPLAY "Dif viv92: ", v_dif_viv92
         
         -- si se encontro una diferencia, entonces se marca como 03 el registro y se indica en el codigo de rechazo
         -- si es vivienda 97
         -- Se deben reportar con resultado 01 y sin motivo de rechazo
         -- se debe dejar de enviar 03 para los sobregiros  PRODINF-564
--- ***         IF ( v_dif_viv97 <> 0 OR v_dif_viv92 <> 0 ) THEN 
--- ***         
--- ***            -- si se trata de los dos, se ponen en ambos codigos de rechazo, primero 97 y luego 92
--- ***            IF ( v_dif_viv97 > 0 AND v_dif_viv97 <= 1 AND v_dif_viv92 > 0 AND v_dif_viv92 <= 1 ) THEN 
--- ***               LET r_tmp_ret_det_disposicion.result_operacion = "03"
--- ***                LET r_tmp_ret_det_disposicion.motivo_rech1     = "767" -- viv97
--- ***               LET r_tmp_ret_det_disposicion.motivo_rech2     = "766" -- viv92
--- ***            ELSE
--- ***               -- se revisa si fue viv92
--- ***               IF ( v_dif_viv92 > 0 AND v_dif_viv92 <= 1 ) THEN
--- ***                  LET r_tmp_ret_det_disposicion.result_operacion = "03"
--- ***                  LET r_tmp_ret_det_disposicion.motivo_rech1     = "766" -- viv92
--- ***               ELSE
--- ***                  -- fue viv97
--- ***                  IF ( v_dif_viv97 > 0 AND v_dif_viv97 <= 1 ) THEN
--- ***                     LET r_tmp_ret_det_disposicion.result_operacion = "03"
--- ***                     LET r_tmp_ret_det_disposicion.motivo_rech1     = "767" -- viv97
--- ***                  END IF
--- ***               END IF
--- ***            END IF
--- ***         END IF
      END IF

      INSERT INTO tmp_ret_det_disposicion VALUES (r_tmp_ret_det_disposicion.*)
   
   END FOREACH
   FREE cur_salidadetalledisposicion
   
   -- si se incluyen rechazados
   IF ( p_incluye_rechazos ) THEN
      -- se lee de la tabla de rechazos
      DECLARE cur_retdisposicionRCH CURSOR FOR
      SELECT
         id_solicitud         ,
         nss                  ,
         curp                 ,
         nombre_afore         ,
         paterno_afore        ,
         materno_afore        ,
         sec_pension          ,
         tipo_retiro          ,
         regimen              ,
         tpo_seguro           ,
         tpo_pension          ,
         tpo_prestacion       ,
         f_inicio_pension     ,
         f_resolucion         ,
         porcentaje_valuacion * 100,
         semanas_cotizadas    ,
         f_solicitud          ,
         cve_doc_probatorio   ,
         f_nacimiento         ,
         aseguradora          ,
         actuario             ,
         num_plan_privado     ,
         periodo_primer_pago  , 
         aivs_viv97           * 1000000,
         aivs_viv92           * 1000000,
         consec_trabajador    ,
         importe_viv72        * 100,
         diag_registro        ,
         estado_sub_viv       ,
         cve_afore            ,
         resultado_operacion  , 
         cod_rechazo_1        ,
         cod_rechazo_2        
      FROM   ret_disposicion_rch
      WHERE  folio = p_folio
      
      FOREACH cur_retdisposicionRCH
      INTO
         r_tmp_ret_det_disposicion.id_solicitud        ,
         r_tmp_ret_det_disposicion.nss                 ,
         r_tmp_ret_det_disposicion.curp                ,
         r_tmp_ret_det_disposicion.nombre_afore        ,
         r_tmp_ret_det_disposicion.paterno_afore       ,
         r_tmp_ret_det_disposicion.materno_afore       ,
         r_tmp_ret_det_disposicion.sec_pension         ,
         r_tmp_ret_det_disposicion.tpo_retiro          ,
         r_tmp_ret_det_disposicion.regimen             ,
         r_tmp_ret_det_disposicion.tpo_seguro          ,
         r_tmp_ret_det_disposicion.tpo_pension         ,
         r_tmp_ret_det_disposicion.tpo_prestacion      ,
         r_tmp_ret_det_disposicion.f_inicio_pension    ,
         r_tmp_ret_det_disposicion.f_emision_resol     ,
         r_tmp_ret_det_disposicion.porc_valuacion      ,
         r_tmp_ret_det_disposicion.sem_cotizadas       ,
         r_tmp_ret_det_disposicion.f_solicitud_trab    ,
         r_tmp_ret_det_disposicion.cve_doc_probatorio  ,
         r_tmp_ret_det_disposicion.f_nacimiento        ,
         r_tmp_ret_det_disposicion.aseguradora         ,
         r_tmp_ret_det_disposicion.actuario_autor      ,
         r_tmp_ret_det_disposicion.num_reg_ppp         ,
         r_tmp_ret_det_disposicion.periodo_pago        ,
         r_tmp_ret_det_disposicion.aiv97               ,
         r_tmp_ret_det_disposicion.aiv92               ,
         r_tmp_ret_det_disposicion.consec_trab         ,
         r_tmp_ret_det_disposicion.fondo_subcta_viv72  ,
         r_tmp_ret_det_disposicion.diagnostico_reg     ,
         r_tmp_ret_det_disposicion.estatus_subcta      ,
         r_tmp_ret_det_disposicion.cve_afore           ,
         r_tmp_ret_det_disposicion.result_operacion    ,
         r_tmp_ret_det_disposicion.motivo_rech1        ,
         r_tmp_ret_det_disposicion.motivo_rech2        
           
         --se asignan variables segun  layout 
         LET r_tmp_ret_det_disposicion.tpo_registro      = "03"
         LET r_tmp_ret_det_disposicion.id_servicio       = "04"  
         LET r_tmp_ret_det_disposicion.id_operacion      = "08"
         LET r_tmp_ret_det_disposicion.acciones_ret97    = 0
         LET r_tmp_ret_det_disposicion.acciones_cv       = 0
         LET r_tmp_ret_det_disposicion.acciones_cuotsol  = 0
         LET r_tmp_ret_det_disposicion.filler1           = 14 SPACES
         LET r_tmp_ret_det_disposicion.acciones_ret92    = 0
         LET r_tmp_ret_det_disposicion.filler2           = 8 SPACES
         LET r_tmp_ret_det_disposicion.filler3           = 12 SPACES
       
         LET r_tmp_ret_det_disposicion.result_operacion  = "02"
         LET r_tmp_ret_det_disposicion.motivo_rech1      = r_tmp_ret_det_disposicion.motivo_rech1 USING "&&&"
         LET r_tmp_ret_det_disposicion.motivo_rech2      = r_tmp_ret_det_disposicion.motivo_rech2 USING "&&&"
         
         -- no se tienen montos con diferencia en rechazos
         LET r_tmp_ret_det_disposicion.aiv97_con_dif = 0
         LET r_tmp_ret_det_disposicion.aiv92_con_dif = 0
         
         -- se inserta en la tabla temporal             
         INSERT INTO tmp_ret_det_disposicion VALUES (r_tmp_ret_det_disposicion.*)
      
      END FOREACH
   END IF -- registros rechazados
   
   -- se leen los registros de la tabla temporal para generar archivo
   DECLARE cur_temporal CURSOR FOR
   SELECT *
   FROM   tmp_ret_det_disposicion
   ORDER BY cve_afore, nss
   
   LET v_i_contador_registros = 0
   
   FOREACH cur_temporal INTO r_tmp_ret_det_disposicion.*
   
      --se forma el detalle a escribir en el archivo   
      LET v_f_inicio_pension = r_tmp_ret_det_disposicion.f_inicio_pension USING "yyyymmdd"
      LET v_f_resolucion     = r_tmp_ret_det_disposicion.f_emision_resol  USING "yyyymmdd"
      LET v_f_nacimiento     = r_tmp_ret_det_disposicion.f_nacimiento     USING "yyyymmdd"
      LET v_f_solicitud_trab = r_tmp_ret_det_disposicion.f_solicitud_trab USING "yyyymmdd"
      
      --se asignan variables segun  layout 
      LET r_tmp_ret_det_disposicion.filler1           = 14 SPACES
      LET r_tmp_ret_det_disposicion.filler2           = 8 SPACES
      LET r_tmp_ret_det_disposicion.filler3           = 12 SPACES
      
      LET v_s_detalle = r_tmp_ret_det_disposicion.tpo_registro      ,
                        r_tmp_ret_det_disposicion.id_servicio       ,
                        r_tmp_ret_det_disposicion.id_operacion      ,
                        r_tmp_ret_det_disposicion.nss               ,
                        r_tmp_ret_det_disposicion.curp              ,
   	                    r_tmp_ret_det_disposicion.nombre_afore      ,
                        r_tmp_ret_det_disposicion.paterno_afore     ,
                        r_tmp_ret_det_disposicion.materno_afore     ,
                        r_tmp_ret_det_disposicion.sec_pension       ,
                        r_tmp_ret_det_disposicion.tpo_retiro        ,
                        r_tmp_ret_det_disposicion.regimen           ,
                        r_tmp_ret_det_disposicion.tpo_seguro        ,
                        r_tmp_ret_det_disposicion.tpo_pension       ,
                        r_tmp_ret_det_disposicion.tpo_prestacion    ,
                        v_f_inicio_pension                          ,
                        v_f_resolucion                              ,
                        r_tmp_ret_det_disposicion.porc_valuacion USING "&&&&&",
                        r_tmp_ret_det_disposicion.sem_cotizadas  USING "&&&&" ,
                        v_f_solicitud_trab                          ,
                        r_tmp_ret_det_disposicion.cve_doc_probatorio,
                        v_f_nacimiento                              ,
                        r_tmp_ret_det_disposicion.aseguradora       ,
                        r_tmp_ret_det_disposicion.actuario_autor    ,
                        r_tmp_ret_det_disposicion.num_reg_ppp       ,
                        r_tmp_ret_det_disposicion.periodo_pago     USING "&&&&&&",
                        r_tmp_ret_det_disposicion.acciones_ret97   USING "&&&&&&&&&&&&&&",
                        r_tmp_ret_det_disposicion.acciones_cv      USING "&&&&&&&&&&&&&&",
                        r_tmp_ret_det_disposicion.acciones_cuotsol USING "&&&&&&&&&&&&&&",
                        r_tmp_ret_det_disposicion.filler1           ,
                        r_tmp_ret_det_disposicion.acciones_ret92   USING "&&&&&&&&&&&&&&",
                        r_tmp_ret_det_disposicion.filler2           ,
                        r_tmp_ret_det_disposicion.aiv97            USING "&&&&&&&&&&&&&&",
                        r_tmp_ret_det_disposicion.aiv92            USING "&&&&&&&&&&&&&&",
                        r_tmp_ret_det_disposicion.consec_trab      USING "&&&&&&&&&&&",
                        r_tmp_ret_det_disposicion.fondo_subcta_viv72 USING "&&&&&&&&&&&&&&",
                        r_tmp_ret_det_disposicion.diagnostico_reg   ,
                        r_tmp_ret_det_disposicion.estatus_subcta    ,
                        r_tmp_ret_det_disposicion.filler3           ,                       
                        r_tmp_ret_det_disposicion.result_operacion  ,
                        r_tmp_ret_det_disposicion.cve_afore        USING "&&&",
                        r_tmp_ret_det_disposicion.motivo_rech1     ,
                        r_tmp_ret_det_disposicion.motivo_rech2     
--                        r_tmp_ret_det_disposicion.aiv97_con_dif    USING "&&&&&&&&&&&&&&",
--                        r_tmp_ret_det_disposicion.aiv92_con_dif    USING "&&&&&&&&&&&&&&"

      --se escribe el detalle en el archivo 
      CALL v_ch_arch_disposicion.writeLine(v_s_detalle)
      CALL v_ch_arch_disposicion_copia.writeLine(v_s_detalle)
      
      --se incrementa el contador de registros 
      LET v_i_contador_registros = v_i_contador_registros + 1
   END FOREACH

   --se asignan valores segun Layout 
   LET  r_tmp_ret_sum_disposicion.tpo_registro        = "09"
   LET  r_tmp_ret_sum_disposicion.id_servicio         = "04"
     
   -- para procesar se incluyen los rechazos
   IF ( p_incluye_rechazos ) THEN
      LET  r_tmp_ret_sum_disposicion.tpo_entidad_origen  = "04"
      LET  r_tmp_ret_sum_disposicion.cve_entidad_origen  = "002"
      LET  r_tmp_ret_sum_disposicion.tpo_entidad_destino = "03"
      LET  r_tmp_ret_sum_disposicion.cve_entidad_destino = "001" 
   ELSE
      -- TRM. sin rechazos
      LET  r_tmp_ret_sum_disposicion.tpo_entidad_origen  = "03"
      LET  r_tmp_ret_sum_disposicion.cve_entidad_origen  = "001"
      LET  r_tmp_ret_sum_disposicion.tpo_entidad_destino = "04"
      LET  r_tmp_ret_sum_disposicion.cve_entidad_destino = "002" 
   END IF
   -- la fecha de operacion procesar es la misma del encabezado
   LET  r_tmp_ret_sum_disposicion.f_operacion         = v_f_operacion_cza
   LET  r_tmp_ret_sum_disposicion.total_registros     = v_i_contador_registros USING "&&&&&&"
   LET  r_tmp_ret_sum_disposicion.filler              = 362 SPACES
   --se forma el suamario
   
   LET v_s_sumario = r_tmp_ret_sum_disposicion.tpo_registro       ,
                     r_tmp_ret_sum_disposicion.id_servicio        ,
                     r_tmp_ret_sum_disposicion.tpo_entidad_origen ,
                     r_tmp_ret_sum_disposicion.cve_entidad_origen ,
                     r_tmp_ret_sum_disposicion.tpo_entidad_destino,
                     r_tmp_ret_sum_disposicion.cve_entidad_destino,
                     r_tmp_ret_sum_disposicion.f_operacion        ,
                     r_tmp_ret_sum_disposicion.total_registros    ,
                     r_tmp_ret_sum_disposicion.filler
   
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
          LET v_archivo_transfer = v_c_ruta_env_acr CLIPPED, "/../transfer/PRTFT.DP.I04002.S", v_archivo_original[3,8], ".DISPINF.GDG"
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
          -- ruta del archivo en transfer  NMRFRET.DSPRM.DISPINF.V", v_archivo_original[3,8]
          LET v_archivo_transfer = v_c_ruta_env_acr CLIPPED, "/../transfer/NMRFRET.DSPRM.DISPINF.F", v_archivo_original[3,8]
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
           DISPLAY "\n Se ejecuta Script >sh /opt/Interpel/Scripts/DISPINF_SYSR.sh<"

           LET v_ejecuta_sh = " sh /opt/Interpel/Scripts/DISPINF_SYSR.sh"
           RUN v_ejecuta_sh
       END IF
   END IF -- IF (p_es_previo = 0)

   
END FUNCTION