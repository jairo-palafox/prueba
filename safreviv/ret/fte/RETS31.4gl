--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
##############################################################################################
#Modulo            =>RET                                                                     #
#Programa          =>RETS31                                                                  #
#Objetivo          =>Programa que ejecuta el proceso de generacion                           #
#                    de archivo de salida de retiros tipo N incluyendo registros aceptados   #
#                    y/o rechazados para PROCESAR                                            #
#Modificado                                                                                  #
# Ivan Vega     09/sep/2013  - Se generara una copia de los archivos para procesar y trm     #
#                              segun lo solicitado en PRODINF-64                             #
#                              Adicionalmente se cambia el encabezado en el archivo de salida#
#                              de los archivos que van para TRM pues la clave destino debe   #
#                              ser la adecuada para TRM                                      #
# Ivan Vega      24/Sep/2013 - Se cambia la V por S en el nombre del archivo de salida para  #
#                              PROCESAR - Req. PRODINF-96                                    #
# Ivan Vega      11/Feb/2014 - Se agregan AIVs aceptadas viv92/viv97 para los casos cuando   #
#                              se pago menos monto de lo que se habia solicitado             # 
# Ivan Vega      11/Feb/2014 - Los NSS que se aceptan con la diferencia se graban como 03 en #
#                              el resultado de la operacion del registro en el archivo
# Ivan Vega      11/Feb/2014 - Se genera una copia del archivo para PROCESAR en la carpeta   #
#                              transfer, segun requerimiento PRODINF-159                     #
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
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".RETS31.log")
   
   -- Llamado a función que genera el archivo de salida
   CALL fn_archivo_salida( p_folio, p_usuario_cod, p_incluye_rechazos, p_es_previo)
END MAIN

-- Funcion que se encarga de crear el archivo de salida
-- recibe como parametro la fecha inicial y final para tomar el rango de salida
FUNCTION fn_archivo_salida( p_folio, p_usuario_cod, p_incluye_rechazos, p_es_previo)
DEFINE p_folio                      LIKE glo_folio.folio, -- folio del proceso
       p_usuario_cod                LIKE seg_usuario.usuario_cod, -- usuario que ejecuta
       p_incluye_rechazos           SMALLINT, -- booleana que indica si el archivo contiene rechazados tambien
       p_es_previo                  SMALLINT, -- booleana que indica si el archivo es previo
       v_v_nom_archivo              STRING, -- nombre del archivo de salida
       v_v_nom_archivo_copia        STRING, -- nombre del archivo de salida
       v_v_ruta_nomarch             STRING, -- ruta y nombre del archivo de salida
       v_v_ruta_nomarch_copia       STRING, -- ruta y nombre del archivo de salida
       v_c_ruta_env_acr             LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
       v_ch_arch_disposicion        BASE.CHANNEL, -- manejador de apuntador hacia archivo
       v_ch_arch_disposicion_copia  BASE.CHANNEL, -- manejador de apuntador hacia archivo
       v_s_registro                 STRING, -- registro a insertar
       v_r_ret_tipo_n               RECORD LIKE ret_tipo_n.*, -- registro de la tabla retiro Tipo N
       v_consec_cuenta              LIKE afi_decreto.consec_cuenta, -- consecutivo cuenta en decreto
       v_r_ret_tipo_n_rch           RECORD LIKE ret_tipo_n_rch.*, -- registro de retiro tipo N rechazado
       r_tmp_ret_det_tipo_n         RECORD     
	     id_solicitud                 DECIMAL(9,0) ,
         tpo_registro                 CHAR(2)      ,
         id_servicio                  CHAR(2)      ,
         id_operacion                 CHAR(2)      ,
         nss_icefa                    CHAR(11)     ,
         rfc_icefa                    CHAR(13)     ,
         nci_icefa                    CHAR(30)     ,
         cve_icefa                    CHAR(3)      ,
         nombre_icefa                 CHAR(120)    ,
         nss_id_presentada            CHAR(11)     ,
         rfc_id_presentada            CHAR(13)     ,
         nombre_id_presentada         CHAR(120)    ,
         docto_probatorio             CHAR(1)      ,
         num_referencia               CHAR(18)     ,
         origen_retiro                CHAR(1)      ,
         tpo_seguro                   CHAR(2)      ,
         tpo_pension                  CHAR(2)      ,
         tpo_prestacion               CHAR(2)      ,
         regimen                      CHAR(2)      ,
         f_inicio_pension             CHAR(8)      ,
         f_resolucion                 CHAR(8)      ,
         porc_valuacion               DECIMAL(5,0) ,
         actuario                     CHAR(7)      ,
         registro_ppp                 CHAR(8)      ,
         importe_ret92                DECIMAL(15,0),
         aivs_viv92                   DECIMAL(14,0),
         diagnostico                  CHAR(3)      ,
         cve_afore                    CHAR(3)      ,
         num_id_unico                 CHAR(11)     ,
         filler1                      CHAR(21)     ,
         result_operacion             CHAR(2)      ,
         motivo_rech1                 CHAR(3)      ,
         motivo_rech2                 CHAR(3)      ,
         motivo_rech3                 CHAR(3)      ,
         -- 02dic2013. se agrega el campo para verificar diferencias
         aiv92_con_dif                DECIMAL(14,0)  -- aivs viv 92 aceptadas hasta con 1 aiv de diferencia

       END RECORD,
       r_tmp_ret_cza_tipo_n   RECORD 
         tpo_registro          CHAR(2),
         id_servicio           CHAR(2),
         tpo_entidad_origen    CHAR(2),
         cve_entidad_origen    CHAR(3),
         tpo_entidad_destino   CHAR(2),
         cve_entidad_destino   CHAR(3),
         f_operacion           DATE ,
         resultado_operacion   CHAR(2),
         motivo_rech_1         CHAR(3),
         motivo_rech_2         CHAR(3),
         motivo_rech_3         CHAR(3),
         filler                CHAR(437)  
        END RECORD,
        r_tmp_ret_sum_tipo_n RECORD
          tpo_registro         CHAR(2)      ,
          id_servicio          CHAR(2)      ,
          total_registros      DECIMAL(6,0) ,
          total_ret92          DECIMAL(17,0),
          total_viv92          DECIMAL(17,0),
          filler               CHAR(426)
        END RECORD,
        v_regimen              smallint,
        v_tpo_seguro           char(2) ,
        v_tpo_pension          char(2) ,
        v_tpo_prestacion       char(2) ,
        v_s_encabezado         STRING,       
        v_s_detalle            STRING,       
        v_i_contador_registros INTEGER,
        v_s_sumario            STRING,       
        v_i_secuencia          INTEGER,       
        v_f_operacion          CHAR(8),       
        v_f_inicio_pension     CHAR(8),
        v_f_resolucion         CHAR(8),
        v_total_importe_ret92  DECIMAL (17,0),
        v_total_aivs_viv92     DECIMAL (17,0),
        v_sql                  STRING, -- cadena con instruccion SQL
        v_archivo_original     LIKE glo_ctr_archivo.nombre_archivo,
        v_dif_viv92            DECIMAL(8,6), -- para calcular diferencias entre lo solicitado y lo pagado
        v_archivo_transfer     STRING, -- cadena auxiliar
        v_ejecuta_sh           STRING,
        v_resultado_copia      INTEGER -- resultado de ejecutar la copia de archivo


   -- se inician los registros en nulo  
   LET v_i_contador_registros = 0
   LET v_total_importe_ret92  = 0
   LET v_total_aivs_viv92     = 0

   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio 
   INTO   v_c_ruta_env_acr
   FROM   seg_modulo
   WHERE  modulo_cod = 'ret'
    
   --se obtienela secuencia del archivo  
   SELECT seq_archivo_tesoreria.NEXTVAL
   INTO   v_i_secuencia
   FROM   systables
   WHERE  tabid = 1

   -- se obtiene el nombre del archivo original
   SELECT nombre_archivo
   INTO   v_archivo_original
   FROM   glo_ctr_archivo
   WHERE  proceso_cod = g_proceso_cod_ret_tipo_N 
   AND    folio = p_folio

   DISPLAY "nombre archivo origen: ", v_archivo_original
   
   -- si incluye aceptados y rechazados
   -- el nombre original va como sigue:
   -- fecha   Consecutivo
   -- 123456789012
   -- AAAAMMDDCXXX.retn
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
      LET v_v_nom_archivo = v_v_nom_archivo, "PRTFT.DP.I04002.S", v_archivo_original[3,8], ".RETSAR.C", v_archivo_original[10,12]
      LET v_v_nom_archivo_copia = v_v_nom_archivo_copia, "PRTFT.DP.I04002.S.RETSAR"
   ELSE
      -- solo aceptados
      LET v_v_nom_archivo = v_v_nom_archivo, "NMRFRET.DSPRM.RETSAR.V", v_archivo_original[3,8]
      LET v_v_nom_archivo_copia = v_v_nom_archivo_copia, "NMRFRET.DSPRM.RETSAR.V"
   END IF
   
   -- si solo se requeiren aceptados
   IF ( NOT p_incluye_rechazos ) THEN
      -- se verifica que haya registros aceptados
      SELECT COUNT(*)
      INTO   v_i_contador_registros
      FROM   ret_tipo_n
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
   SELECT f_operacion_procesar 
   INTO   r_tmp_ret_cza_tipo_n.f_operacion
   FROM   ret_cza_tipo_n
   WHERE  folio = p_folio
   
   --se asignan variables segun layout
   LET r_tmp_ret_cza_tipo_n.tpo_registro         = "01"
   LET r_tmp_ret_cza_tipo_n.id_servicio          = "04"
   
   -- para PROCESAR, incluye rechazados
   IF ( p_incluye_rechazos ) THEN
      --se asignan variables segun layout 
      LET r_tmp_ret_cza_tipo_n.tpo_entidad_origen  = "04"
      LET r_tmp_ret_cza_tipo_n.cve_entidad_origen  = "002"
      LET r_tmp_ret_cza_tipo_n.tpo_entidad_destino = "03"
      LET r_tmp_ret_cza_tipo_n.cve_entidad_destino = "001"
      LET r_tmp_ret_cza_tipo_n.resultado_operacion = "01"
   ELSE
      -- PARA TRM, no incluye rechazos
      LET r_tmp_ret_cza_tipo_n.tpo_entidad_origen  = "03"
      LET r_tmp_ret_cza_tipo_n.cve_entidad_origen  = "001"
      LET r_tmp_ret_cza_tipo_n.tpo_entidad_destino = "04"
      LET r_tmp_ret_cza_tipo_n.cve_entidad_destino = "002"
   END IF
      
   LET r_tmp_ret_cza_tipo_n.resultado_operacion  = "01"
   LET r_tmp_ret_cza_tipo_n.motivo_rech_1        = "000"  
   LET r_tmp_ret_cza_tipo_n.motivo_rech_2        = "000"  
   LET r_tmp_ret_cza_tipo_n.motivo_rech_3        = "000"  
   LET r_tmp_ret_cza_tipo_n.filler               = 437 SPACES
   
   LET v_f_operacion = r_tmp_ret_cza_tipo_n.f_operacion USING "yyyymmdd"
   
   --se asignan los datos del encabezado para escribir en el archivo
   LET v_s_encabezado = r_tmp_ret_cza_tipo_n.tpo_registro        ,
                        r_tmp_ret_cza_tipo_n.id_servicio         ,   
                        r_tmp_ret_cza_tipo_n.tpo_entidad_origen  , 
                        r_tmp_ret_cza_tipo_n.cve_entidad_origen  , 
                        r_tmp_ret_cza_tipo_n.tpo_entidad_destino , 
                        r_tmp_ret_cza_tipo_n.cve_entidad_destino ,  
                        v_f_operacion                            , 
                        r_tmp_ret_cza_tipo_n.resultado_operacion ,
                        r_tmp_ret_cza_tipo_n.motivo_rech_1       ,
                        r_tmp_ret_cza_tipo_n.motivo_rech_2       ,
                        r_tmp_ret_cza_tipo_n.motivo_rech_3       ,
                        r_tmp_ret_cza_tipo_n.filler        
                         
   -- se escribe el encabezado en el archivo 
   CALL v_ch_arch_disposicion.writeLine(v_s_encabezado)
   
   -- encabezado en archivo copia
   CALL v_ch_arch_disposicion_copia.writeLine(v_s_encabezado)

   -- se prepara la tabla temporal para extraccion de datos de detalle
   LET v_sql = "\nDROP TABLE IF EXISTS tmp_ret_det_tipo_n;",
               "\nCREATE TABLE tmp_ret_det_tipo_n   (",
			   "\nid_solicitud         DECIMAL(9,0) ,",
               "\ntpo_registro         CHAR(2)      ,",
               "\nid_servicio          CHAR(2)      ,",
               "\nid_operacion         CHAR(2)      ,",
               "\nnss_icefa            CHAR(11)     ,",
               "\nrfc_icefa            CHAR(13)     ,",
               "\nnci_icefa            CHAR(30)     ,",
               "\ncve_icefa            CHAR(13)     ,",
               "\nnombre_icefa         CHAR(120)    ,",
               "\nnss_id_presentada    CHAR(11)     ,",
               "\nrfc_id_presentada    CHAR(13)     ,",
               "\nnombre_id_presentada CHAR(120)    ,",
               "\ndocto_probatorio     CHAR(1)      ,",
               "\nnum_referencia       CHAR(18)     ,",
               "\norigen_retiro        CHAR(1)      ,",
               "\ntpo_seguro           CHAR(2)      ,",
               "\ntpo_pension          CHAR(2)      ,",
               "\ntpo_prestacion       CHAR(2)      ,",
               "\nregimen              CHAR(2)      ,",
               "\nf_inicio_pension     CHAR(8)      ,",
               "\nf_resolucion         CHAR(8)      ,",
               "\nporc_valuacion       DECIMAL(5,0) ,",
               "\nactuario             CHAR(7)      ,",
               "\nregistro_ppp         CHAR(8)      ,",
               "\nimporte_ret92        DECIMAL(15,0),",
               "\naivs_viv92           DECIMAL(14,0),",
               "\ndiagnostico          CHAR(3)      ,",
               "\ncve_afore            CHAR(3)      ,",
               "\nnum_id_unico         CHAR(11)     ,",
               "\nfiller1              CHAR(21)     ,",
               "\nresult_operacion     CHAR(2)      ,",
               "\nmotivo_rech1         CHAR(3)      ,",
               "\nmotivo_rech2         CHAR(3)      ,",
               "\nmotivo_rech3         CHAR(3)      ,",
               "\naiv92_con_dif        DECIMAL(14,0) );"  -- aivs viv 92 aceptadas hasta con 1 aiv de diferencia
                    
   -- se crea la tabla temporal para la extraccion
   PREPARE sid_creatabla FROM v_sql
   EXECUTE sid_creatabla
                    
   -- se obtiene los datos de la tabla historica
   LET v_sql = "\nSELECT                      ",
               "\n   a.id_solicitud          ,",
               "\n   a.nss_icefa             ,",
               "\n   a.rfc_icefa             ,",
               "\n   a.nombre_icefa          ,",
               "\n   a.num_ctr_interno       ,",   
               "\n   a.cve_icefa             ,",
               "\n   a.rfc                   ,",
               "\n   a.nombre                ,",
               "\n   a.cve_doc_probatorio    ,",
               "\n   a.num_referencia        ,",
               "\n   a.origen_retiro         ,",
               "\n   a.f_inicio_pension      ,",  
               "\n   a.f_resolucion          ,",  
               "\n   a.porcentaje_valuacion  ,",
               "\n   a.actuario              ,",   
               "\n   a.num_plan_privado      ,",
               "\n   a.importe_sar92         ,",
               "\n   a.aivs_viv92            ,",
               "\n   a.cve_afore             ,",
               "\n   a.cod_rechazo           ,",
               "\n   c.regimen               ,",
               "\n   c.tpo_seguro            ,",
               "\n   c.tpo_pension           ,",
               "\n   c.tpo_prestacion        ,",
               "\n   b.consec_cuenta          ",
               "\nFROM ret_tipo_n a,          ",
               "\n     afi_decreto b,         ",
               "\n     ret_matriz_derecho c   ",
               "\nWHERE a.folio = ", p_folio   ,
               "\nAND a.id_decreto = b.id_decreto",
               "\nAND a.id_ret_matriz_derecho = c.id_ret_matriz_derecho"
               
   -- si se no se necesitan los rechazados
   IF ( NOT p_incluye_rechazos ) THEN
      LET v_sql = v_sql, "\n AND a.cod_rechazo = 0"
   END IF

   --DISPLAY v_sql
   
   -- se prepara la consulta   
   PREPARE sid_salidadetalletipon FROM v_sql

   DECLARE cur_salidadetalletipon CURSOR FOR sid_salidadetalletipon
   -- se escribe cada registro de detalle
   FOREACH cur_salidadetalletipon 
   INTO v_r_ret_tipo_n.id_solicitud          ,
        v_r_ret_tipo_n.nss_icefa             ,
        v_r_ret_tipo_n.rfc_icefa             ,
        v_r_ret_tipo_n.nombre_icefa          ,
        v_r_ret_tipo_n.num_ctr_interno       ,   
        v_r_ret_tipo_n.cve_icefa             ,
        v_r_ret_tipo_n.rfc                   ,
        v_r_ret_tipo_n.nombre                ,
        v_r_ret_tipo_n.cve_doc_probatorio    ,
        v_r_ret_tipo_n.num_referencia        ,
        v_r_ret_tipo_n.origen_retiro         ,
        v_r_ret_tipo_n.f_inicio_pension      ,  
        v_r_ret_tipo_n.f_resolucion          ,  
        v_r_ret_tipo_n.porcentaje_valuacion  ,
        v_r_ret_tipo_n.actuario              ,   
        v_r_ret_tipo_n.num_plan_privado      ,
        v_r_ret_tipo_n.importe_sar92         ,
        v_r_ret_tipo_n.aivs_viv92            ,
        v_r_ret_tipo_n.cve_afore             ,
        v_r_ret_tipo_n.cod_rechazo           ,
        v_regimen                            ,
        v_tpo_seguro                         ,
        v_tpo_pension                        ,
        v_tpo_prestacion                     ,
        v_consec_cuenta
      
      -- se asignan los datos al registro de la tabla temporal
	  LET r_tmp_ret_det_tipo_n.id_solicitud         = v_r_ret_tipo_n.id_solicitud
      LET r_tmp_ret_det_tipo_n.tpo_registro         = "03"
      LET r_tmp_ret_det_tipo_n.id_servicio          = "04"
      LET r_tmp_ret_det_tipo_n.id_operacion         = "23"
      LET r_tmp_ret_det_tipo_n.nss_icefa            = v_r_ret_tipo_n.nss_icefa
      LET r_tmp_ret_det_tipo_n.rfc_icefa            = v_r_ret_tipo_n.rfc_icefa
      LET r_tmp_ret_det_tipo_n.nci_icefa            = v_r_ret_tipo_n.num_ctr_interno
      LET r_tmp_ret_det_tipo_n.cve_icefa            = v_r_ret_tipo_n.cve_icefa
      LET r_tmp_ret_det_tipo_n.nombre_icefa         = v_r_ret_tipo_n.nombre_icefa
      LET r_tmp_ret_det_tipo_n.nss_id_presentada    = "00000000000"
      LET r_tmp_ret_det_tipo_n.rfc_id_presentada    = v_r_ret_tipo_n.rfc
      LET r_tmp_ret_det_tipo_n.nombre_id_presentada = v_r_ret_tipo_n.nombre
      LET r_tmp_ret_det_tipo_n.docto_probatorio     = v_r_ret_tipo_n.cve_doc_probatorio
      LET r_tmp_ret_det_tipo_n.num_referencia       = v_r_ret_tipo_n.num_referencia
      LET r_tmp_ret_det_tipo_n.origen_retiro        = v_r_ret_tipo_n.origen_retiro
      LET r_tmp_ret_det_tipo_n.tpo_seguro           = v_tpo_seguro
      LET r_tmp_ret_det_tipo_n.tpo_pension          = v_tpo_pension
      LET r_tmp_ret_det_tipo_n.tpo_prestacion       = v_tpo_prestacion
      LET r_tmp_ret_det_tipo_n.regimen              = v_regimen
      
      LET v_f_inicio_pension = v_r_ret_tipo_n.f_inicio_pension USING "yyyymmdd"
      LET v_f_resolucion     = v_r_ret_tipo_n.f_resolucion     USING "yyyymmdd"
      
      --DISPLAY v_r_ret_tipo_n.f_inicio_pension, v_f_inicio_pension
      
      LET r_tmp_ret_det_tipo_n.f_inicio_pension     = v_f_inicio_pension
      LET r_tmp_ret_det_tipo_n.f_resolucion         = v_f_resolucion
      
      LET r_tmp_ret_det_tipo_n.porc_valuacion       = v_r_ret_tipo_n.porcentaje_valuacion * 100
      LET r_tmp_ret_det_tipo_n.actuario             = v_r_ret_tipo_n.actuario
      LET r_tmp_ret_det_tipo_n.registro_ppp         = v_r_ret_tipo_n.num_plan_privado
      LET r_tmp_ret_det_tipo_n.importe_ret92        = v_r_ret_tipo_n.importe_sar92 * 100
      LET r_tmp_ret_det_tipo_n.aivs_viv92           = v_r_ret_tipo_n.aivs_viv92 * 1000000
      LET r_tmp_ret_det_tipo_n.diagnostico          = "000"
      LET r_tmp_ret_det_tipo_n.cve_afore            = v_r_ret_tipo_n.cve_afore USING "&&&"
      LET r_tmp_ret_det_tipo_n.num_id_unico         = v_consec_cuenta USING "&&&&&&&&&&&"
      LET r_tmp_ret_det_tipo_n.filler1              = 21 SPACES
      
      -- registro aceptado
      IF ( v_r_ret_tipo_n.cod_rechazo = 0 ) THEN
         LET r_tmp_ret_det_tipo_n.result_operacion  = "01"
         LET r_tmp_ret_det_tipo_n.motivo_rech1      = "000"
         LET r_tmp_ret_det_tipo_n.motivo_rech2      = "000"
         LET r_tmp_ret_det_tipo_n.motivo_rech3      = "000"
      ELSE
         -- registro rechazado
         LET r_tmp_ret_det_tipo_n.result_operacion  = "02"
         LET r_tmp_ret_det_tipo_n.motivo_rech1      = v_r_ret_tipo_n.cod_rechazo USING "&&&"
         LET r_tmp_ret_det_tipo_n.motivo_rech2      = "000"
         LET r_tmp_ret_det_tipo_n.motivo_rech3      = "000"
      END IF
   
      -- se hace la suma de totales 
      LET v_total_importe_ret92 = v_total_importe_ret92 + r_tmp_ret_det_tipo_n.importe_ret92
      LET v_total_aivs_viv92    = v_total_aivs_viv92    + r_tmp_ret_det_tipo_n.aivs_viv92
      
      --- se reinician las diferencias
      LET r_tmp_ret_det_tipo_n.aiv92_con_dif = 0
          
      -- cambio 11feb2014. Se agregan los montos aceptados con diferencia de hasta 1 AIV
      -- se verifica si la solicitud aparece como con sobregiro en viv92
      SELECT NVL(saldo_acciones * 1000000,0)
      INTO   r_tmp_ret_det_tipo_n.aiv92_con_dif
      FROM   ret_his_saldo
      WHERE  id_solicitud    = v_r_ret_tipo_n.id_solicitud
      AND    subcuenta       = 48 -- viv92 decreto
      AND    fondo_inversion = 11
      AND    folio           = p_folio

	  --DISPLAY "Solicitud: ", v_r_ret_tipo_n.id_solicitud
	  --DISPLAY "Saldo encontrado: ", r_tmp_ret_det_tipo_n.aiv92_con_dif
	  
      -- si se registro que hubo diferencia en el saldo
      IF ( r_tmp_ret_det_tipo_n.aiv92_con_dif <> 0 ) THEN
         -- se calcula la diferencia
         LET v_dif_viv92 = (r_tmp_ret_det_tipo_n.aivs_viv92 - r_tmp_ret_det_tipo_n.aiv92_con_dif) / 1000000
         
         --DISPLAY "Dif viv92: ", v_dif_viv92
         
         -- si se encontro una diferencia, entonces se marca como 03 el registro y se indica en el codigo de rechazo
         -- se revisa si fue viv92
         -- Se deben reportar con resultado 01 y sin motivo de rechazo
         -- se debe dejar de enviar 03 para los sobregiros  PRODINF-564
         
--- ***         IF ( v_dif_viv92 > 0 AND v_dif_viv92 <= 1 ) THEN
--- ***            LET r_tmp_ret_det_tipo_n.result_operacion = "03"
--- ***            LET r_tmp_ret_det_tipo_n.motivo_rech1     = "766" -- viv92
--- ***			LET r_tmp_ret_det_tipo_n.motivo_rech2      = "000"
--- ***            LET r_tmp_ret_det_tipo_n.motivo_rech3      = "000"
--- ***         END IF
      END IF

	  -- se inserta en la tabla temporal
      INSERT INTO tmp_ret_det_tipo_n VALUES ( r_tmp_ret_det_tipo_n.* )
      
      -- se incrementa el contador de registros 
      LET v_i_contador_registros = v_i_contador_registros + 1 
   END FOREACH
   
   FREE cur_salidadetalletipon

   -- ================================================================
   -- REGISTROS RECHAZADOS EN INTEGRACION
   
   -- si se incluyen rechazados
   IF ( p_incluye_rechazos ) THEN
   
      -- estos registros no tienen diferencia
	  LET r_tmp_ret_det_tipo_n.aiv92_con_dif = 0
   
      -- se lee de la tabla de rechazos
      DECLARE cur_rettiponrch CURSOR FOR
      SELECT *
      FROM   ret_tipo_n_rch
      WHERE  folio = p_folio
      
      FOREACH cur_rettiponrch
      INTO    v_r_ret_tipo_n_rch.*
          
         -- se asignan los datos al registro de la tabla temporal
         LET r_tmp_ret_det_tipo_n.id_solicitud         = v_r_ret_tipo_n_rch.id_solicitud
		 LET r_tmp_ret_det_tipo_n.tpo_registro         = "03"
         LET r_tmp_ret_det_tipo_n.id_servicio          = "04"
         LET r_tmp_ret_det_tipo_n.id_operacion         = "23"
         LET r_tmp_ret_det_tipo_n.nss_icefa            = v_r_ret_tipo_n_rch.nss_icefa
         LET r_tmp_ret_det_tipo_n.rfc_icefa            = v_r_ret_tipo_n_rch.rfc_icefa
         LET r_tmp_ret_det_tipo_n.nci_icefa            = v_r_ret_tipo_n_rch.num_ctr_interno
         LET r_tmp_ret_det_tipo_n.cve_icefa            = v_r_ret_tipo_n_rch.cve_icefa
         LET r_tmp_ret_det_tipo_n.nombre_icefa         = v_r_ret_tipo_n_rch.nombre_icefa
         LET r_tmp_ret_det_tipo_n.nss_id_presentada    = "00000000000"
         LET r_tmp_ret_det_tipo_n.rfc_id_presentada    = v_r_ret_tipo_n_rch.rfc
         LET r_tmp_ret_det_tipo_n.nombre_id_presentada = v_r_ret_tipo_n_rch.nombre
         LET r_tmp_ret_det_tipo_n.docto_probatorio     = v_r_ret_tipo_n_rch.cve_doc_probatorio
         LET r_tmp_ret_det_tipo_n.num_referencia       = v_r_ret_tipo_n_rch.num_referencia
         LET r_tmp_ret_det_tipo_n.origen_retiro        = v_r_ret_tipo_n_rch.origen_retiro
         LET r_tmp_ret_det_tipo_n.tpo_seguro           = v_r_ret_tipo_n_rch.tpo_seguro
         LET r_tmp_ret_det_tipo_n.tpo_pension          = v_r_ret_tipo_n_rch.tpo_pension
         LET r_tmp_ret_det_tipo_n.tpo_prestacion       = v_r_ret_tipo_n_rch.tpo_prestacion
         LET r_tmp_ret_det_tipo_n.regimen              = v_r_ret_tipo_n_rch.regimen
         
         LET v_f_inicio_pension = v_r_ret_tipo_n_rch.f_inicio_pension USING "yyyymmdd"
         LET v_f_resolucion     = v_r_ret_tipo_n_rch.f_resolucion     USING "yyyymmdd"
         
         LET r_tmp_ret_det_tipo_n.f_inicio_pension     = v_f_inicio_pension
         LET r_tmp_ret_det_tipo_n.f_resolucion         = v_f_resolucion
         
         LET r_tmp_ret_det_tipo_n.porc_valuacion       = v_r_ret_tipo_n_rch.porcentaje_valuacion * 100
         LET r_tmp_ret_det_tipo_n.actuario             = v_r_ret_tipo_n_rch.actuario
         LET r_tmp_ret_det_tipo_n.registro_ppp         = v_r_ret_tipo_n_rch.num_plan_privado
         LET r_tmp_ret_det_tipo_n.importe_ret92        = v_r_ret_tipo_n_rch.importe_sar92 * 100
         LET r_tmp_ret_det_tipo_n.aivs_viv92           = v_r_ret_tipo_n_rch.aivs_viv92 * 1000000
         LET r_tmp_ret_det_tipo_n.diagnostico          = "   "
         LET r_tmp_ret_det_tipo_n.cve_afore            = v_r_ret_tipo_n_rch.cve_afore USING "&&&"
         LET r_tmp_ret_det_tipo_n.num_id_unico         = v_r_ret_tipo_n_rch.num_id_unico
         LET r_tmp_ret_det_tipo_n.filler1              = 21 SPACES
         
         -- registro rechazado
         LET r_tmp_ret_det_tipo_n.result_operacion  = "02"
         LET r_tmp_ret_det_tipo_n.motivo_rech1      = v_r_ret_tipo_n_rch.cod_rechazo_1 USING "&&&"
         LET r_tmp_ret_det_tipo_n.motivo_rech2      = v_r_ret_tipo_n_rch.cod_rechazo_2 USING "&&&"
         LET r_tmp_ret_det_tipo_n.motivo_rech3      = v_r_ret_tipo_n_rch.cod_rechazo_3 USING "&&&"
         
         -- se hace la suma de totales 
         LET v_total_importe_ret92 = v_total_importe_ret92 + r_tmp_ret_det_tipo_n.importe_ret92
         LET v_total_aivs_viv92    = v_total_aivs_viv92    + r_tmp_ret_det_tipo_n.aivs_viv92
         
         INSERT INTO tmp_ret_det_tipo_n VALUES ( r_tmp_ret_det_tipo_n.* )
      
      END FOREACH
   END IF -- registros rechazados

   -- se inicia el contador de registros
   LET v_i_contador_registros = 0

   -- se obtienen los datos de la tabla temporal para el archivo de salida
   DECLARE cur_salida CURSOR FOR
   SELECT * 
   FROM   tmp_ret_det_tipo_n
   
   FOREACH cur_salida INTO r_tmp_ret_det_tipo_n.*
   
      -- se construye la cadena de detalle 
      LET v_s_detalle = r_tmp_ret_det_tipo_n.tpo_registro          ,
                        r_tmp_ret_det_tipo_n.id_servicio           ,
                        r_tmp_ret_det_tipo_n.id_operacion          ,
                        r_tmp_ret_det_tipo_n.nss_icefa             ,
                        r_tmp_ret_det_tipo_n.rfc_icefa             ,
                        r_tmp_ret_det_tipo_n.nci_icefa             ,
                        r_tmp_ret_det_tipo_n.cve_icefa             ,
                        r_tmp_ret_det_tipo_n.nombre_icefa          ,
                        r_tmp_ret_det_tipo_n.nss_id_presentada     ,
                        r_tmp_ret_det_tipo_n.rfc_id_presentada     ,
                        r_tmp_ret_det_tipo_n.nombre_id_presentada  ,
                        r_tmp_ret_det_tipo_n.docto_probatorio      ,
                        r_tmp_ret_det_tipo_n.num_referencia        ,
                        r_tmp_ret_det_tipo_n.origen_retiro         ,
                        r_tmp_ret_det_tipo_n.tpo_seguro            ,
                        r_tmp_ret_det_tipo_n.tpo_pension           ,
                        r_tmp_ret_det_tipo_n.tpo_prestacion        ,
                        r_tmp_ret_det_tipo_n.regimen               ,
                        r_tmp_ret_det_tipo_n.f_inicio_pension      ,
                        r_tmp_ret_det_tipo_n.f_resolucion          ,
                        r_tmp_ret_det_tipo_n.porc_valuacion        USING "&&&&&",
                        r_tmp_ret_det_tipo_n.actuario              ,
                        r_tmp_ret_det_tipo_n.registro_ppp          ,
                        r_tmp_ret_det_tipo_n.importe_ret92         USING "&&&&&&&&&&&&&&&",
                        r_tmp_ret_det_tipo_n.aivs_viv92            USING "&&&&&&&&&&&&&&&",
                        r_tmp_ret_det_tipo_n.diagnostico           ,
                        r_tmp_ret_det_tipo_n.cve_afore             USING "&&&",
                        r_tmp_ret_det_tipo_n.num_id_unico          ,
                        r_tmp_ret_det_tipo_n.filler1               ,
                        r_tmp_ret_det_tipo_n.result_operacion      ,
                        r_tmp_ret_det_tipo_n.motivo_rech1          USING "&&&",
                        r_tmp_ret_det_tipo_n.motivo_rech2          USING "&&&",
                        r_tmp_ret_det_tipo_n.motivo_rech3          USING "&&&"
       
	  -- sin diferencias
	  LET r_tmp_ret_det_tipo_n.aiv92_con_dif = 0
	   
      --se escribe el detalle en el archivo 
      CALL v_ch_arch_disposicion.writeLine(v_s_detalle)
      CALL v_ch_arch_disposicion_copia.writeLine(v_s_detalle)
      
      -- se cuenta un registro de detalle
      LET v_i_contador_registros = v_i_contador_registros + 1
   END FOREACH

   -- se asigana variables segun Layout
   LET r_tmp_ret_sum_tipo_n.tpo_registro     = "09"
   LET r_tmp_ret_sum_tipo_n.id_servicio      = "04"
   LET r_tmp_ret_sum_tipo_n.total_registros  = v_i_contador_registros
   LET r_tmp_ret_sum_tipo_n.total_ret92      = v_total_importe_ret92
   LET r_tmp_ret_sum_tipo_n.total_viv92      = v_total_aivs_viv92
   LET r_tmp_ret_sum_tipo_n.filler           = 426 SPACES
   
   -- se asigana valores a la cadena de sumario 
   LET v_s_sumario = r_tmp_ret_sum_tipo_n.tpo_registro     ,
                     r_tmp_ret_sum_tipo_n.id_servicio      ,
                     r_tmp_ret_sum_tipo_n.total_registros  USING "&&&&&&",
                     r_tmp_ret_sum_tipo_n.total_ret92      USING "&&&&&&&&&&&&&&&&&",
                     r_tmp_ret_sum_tipo_n.total_viv92      USING "&&&&&&&&&&&&&&&&&",
                     r_tmp_ret_sum_tipo_n.filler
   
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
          
          LET v_archivo_transfer = v_c_ruta_env_acr CLIPPED, "/../transfer/PRTFT.DP.I04002.S", v_archivo_original[3,8], ".RETSAR.GDG"
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
          
          LET v_archivo_transfer = v_c_ruta_env_acr CLIPPED, "/../transfer/NMRFRET.DSPRM.RETSAR.F", v_archivo_original[3,8]
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
   END IF -- IF (p_es_previo = 0)

   
END FUNCTION --fn_archivo_salida