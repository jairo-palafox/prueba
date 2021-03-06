--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
##############################################################################################
#Modulo            =>RET                                                                     #
#Programa          =>RETS184                                                                  #
#Objetivo          =>Programa que ejecuta el proceso de generacion                           #
#                    de archivo de salida de retiros por                                     #
#                    disposicion de recursos                                                 #
##############################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"     --Archivo que almacena las variables globales del modulo
GLOBALS
DEFINE g_pid            LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod    LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod      LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
END GLOBALS

MAIN
   DEFINE p_usuario_cod          LIKE seg_usuario.usuario_cod, -- nombre del usuario        
          p_folio                LIKE glo_folio.folio -- numero de folio

   -- se reciben los parametros del programa
   LET p_folio          = ARG_VAL(1)
   LET p_usuario_cod    = ARG_VAL(2)
   {LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET g_nombre_archivo = ARG_VAL(6)
   }
   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".RETS01.log")
   
   -- Llamado a funci�n que genera el archivo de salida
   CALL fn_archivo_salida( p_folio, p_usuario_cod)
END MAIN

-- Funcion que se encarga de crear el archivo de salida
-- recibe como parametro la fecha inicial y final para tomar el rango de salida
FUNCTION fn_archivo_salida( p_folio, p_usuario_cod)
DEFINE p_folio        LIKE glo_folio.folio,
       p_usuario_cod  LIKE seg_usuario.usuario_cod,      
       v_v_nom_archivo                  STRING, -- nombre del archivo de salida
       v_v_ruta_nomarch                 STRING, -- ruta y nombre del archivo de salida
       v_c_ruta_env_acr                 LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
       v_ch_arch_solTransf              BASE.CHANNEL, -- manejador de apuntador hacia archivo
       v_s_registro                     STRING -- registro a insertar     
DEFINE r_tmp_ret_det_disposicion RECORD     
          tpo_registro         CHAR(2),                  
          id_servicio          CHAR(2),                  
          id_operacion         CHAR(2),                  
          nss                  CHAR(11),                 
          curp                 CHAR(18),                 
          nombre_afore         CHAR(40),                 
          paterno_afore        CHAR(40),                 
          materno_afore        CHAR(40),                 
          sec_pension          CHAR(2),                  
          tpo_retiro           CHAR(1),                  
          regimen              CHAR(2),                  
          tpo_seguro           CHAR(2),                  
          tpo_pension          CHAR(2),                  
          tpo_prestacion       CHAR(2),                  
          f_inicio_pension     DATE   ,                  
          f_emision_resol      DATE   ,                  
          porc_valuacion       DECIMAL(5,0),             
          sem_cotizadas        DECIMAL(4,0),             
          f_solicitud_trab     DATE,                     
          cve_doc_probatorio   CHAR(1),                  
          f_nacimiento         DATE,                     
          aseguradora          CHAR(3),                  
          actuario_autor       CHAR(7),                  
          num_reg_ppp          CHAR(8),                  
          periodo_pago         DECIMAL(6,0),             
          acciones_ret97       DECIMAL(14,0),            
          acciones_cv          DECIMAL(14,0),            
          acciones_cuotsol     DECIMAL(14,0),            
          filler1              CHAR(14),                 
          acciones_ret92       DECIMAL(14,0) ,
          filler2              CHAR(8),
          aiv97                DECIMAL(14,0), 
          aiv92                DECIMAL(14,0), 
          consec_trab          DECIMAL(11,0), 
          fondo_subcta_viv72   DECIMAL(14,0), 
          diagnostico_reg      CHAR(3),
          estatus_subcta       CHAR(1),
          filler3              CHAR(12),
          result_operacion     CHAR(2),
          cve_afore            DECIMAL(3,0),
          motivo_rech1         CHAR(3),
          motivo_rech2         CHAR(3)
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
          f_operacion           DATE ,
          total_registros       DECIMAL(2,0),
          filler                CHAR(366)  
END RECORD

DEFINE r_ret_preliquida    RECORD 
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
END RECORD
DEFINE v_s_encabezado         STRING,
       v_d_total_registro     LIKE ret_cza_disposicion.total_registros, 
       v_dtotal_importe       LIKE ret_cza_disposicion.total_importe,
       v_c_usuario            LIKE ret_cza_disposicion.usuario,
       v_s_detalle            STRING,
       v_id_derechohabiente   LIKE ret_disposicion.id_derechohabiente,
       v_i_contador_registros INTEGER,
       v_s_sumario            STRING,
       v_c_fecha              CHAR(8),
       v_i_secuencia          INTEGER,
       v_f_inicio_pension       CHAR(8),
       v_f_resolucion           CHAR(8),
       v_f_nacimiento           CHAR(8),
       v_f_solicitud_trab       CHAR(8),
       v_f_operacion            CHAR(8),
       v_f_operacion_cza        CHAR(8),
       v_f_valor_transferencia  CHAR(8) 

  
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

    --se obtienela secuencia del archivo  
    SELECT seq_archivo_tesoreria.NEXTVAL
      INTO v_i_secuencia
      --FROM ret_cza_tipo_n
      FROM systables
     WHERE tabname = "seq_archivo_tesoreria"

   -- se crea el nombre del archivo y posteriormente se concatena con la ruta   
   --modificaci�n realizada el d�a 21 de abril de 2012 por Rub�n Haro Castro
   --se modifica el nombre del archivo que comprende el a�o-mes-dia-consecutivo.disposicion
   --LET v_v_nom_archivo = "/" ||"solicitudes_liq_tesoreria_" || p_folio || ".ret"}
   LET v_v_nom_archivo = "/",YEAR(TODAY) USING "&&&&",MONTH(TODAY) USING "&&",DAY(TODAY) USING "&&",v_i_secuencia USING "&&&",".Rch_Disposicion"     

   LET v_v_ruta_nomarch = v_c_ruta_env_acr CLIPPED || v_v_nom_archivo
   --DISPLAY "  v_v_ruta_nomarch = ",v_v_ruta_nomarch
   -- se crea el manejador de archivo
   LET v_ch_arch_solTransf = base.Channel.create()

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_solTransf.openFile(v_v_ruta_nomarch, "w" )
   --CALL v_ch_arch_solTransf.setDelimiter("")

  -- se obtienen el encabezado del archivo
  SELECT
      f_operacion_procesar,
      f_valor_transferencia, 
      precio_fondo,  --val_participacion
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

  --se asignan variables segun layout 
  LET r_tmp_ret_cza_dispo.tpo_registro = "01"
  LET r_tmp_ret_cza_dispo.id_servicio  = "04"   
  LET r_tmp_ret_cza_dispo.tpo_entidad_origen  = "00"
  LET r_tmp_ret_cza_dispo.cve_entidad_origen  = "000"
  LET r_tmp_ret_cza_dispo.tpo_entidad_destino = "00"
  LET r_tmp_ret_cza_dispo.cve_entidad_destino = "000"
  LET r_tmp_ret_cza_dispo.resultado_operacion = "02"
  LET r_tmp_ret_cza_dispo.motivo_rech_1 =  "111"
  LET r_tmp_ret_cza_dispo.motivo_rech_2 =  "222"
  LET r_tmp_ret_cza_dispo.motivo_rech_3 =  "333"
  LET r_tmp_ret_cza_dispo.filler        = "0000000000000000000"
                                          
  LET v_f_operacion_cza       =  YEAR(r_tmp_ret_cza_dispo.f_operacion) USING "&&&&",MONTH(r_tmp_ret_cza_dispo.f_operacion) USING "&&",DAY(r_tmp_ret_cza_dispo.f_operacion) USING "&&"
  LET v_f_valor_transferencia =  YEAR(r_tmp_ret_cza_dispo.f_valor_transferencia) USING "&&&&",MONTH(r_tmp_ret_cza_dispo.f_valor_transferencia) USING "&&",DAY(r_tmp_ret_cza_dispo.f_valor_transferencia) USING "&&"

                                          
  --se asignan los datos del encabezado para escribir en el archivo
  LET  v_s_encabezado =  r_tmp_ret_cza_dispo.tpo_registro,
  r_tmp_ret_cza_dispo.id_servicio,
  r_tmp_ret_cza_dispo.tpo_entidad_origen  ,
  r_tmp_ret_cza_dispo.cve_entidad_origen  ,
  r_tmp_ret_cza_dispo.tpo_entidad_destino ,
  r_tmp_ret_cza_dispo.cve_entidad_destino ,   
  v_f_operacion_cza       ,
  v_f_valor_transferencia ,
  r_tmp_ret_cza_dispo.val_participacion,
  r_tmp_ret_cza_dispo.resultado_operacion ,
  r_tmp_ret_cza_dispo.motivo_rech_1 ,
  r_tmp_ret_cza_dispo.motivo_rech_2 ,
  r_tmp_ret_cza_dispo.motivo_rech_3 ,
  r_tmp_ret_cza_dispo.filler        
  
  --se escribe el encabezado en el archivo 
  CALL v_ch_arch_solTransf.writeLine([v_s_encabezado])

  DECLARE cur_salidadetalledisposicion CURSOR FOR
      SELECT 
        id_derechohabiente,
        sec_pension,
        curp,  
        nombre_afore   ,   
        paterno_afore  ,
        materno_afore  ,
        f_inicio_pension,
        f_resolucion,
        porcentaje_valuacion ,
        semanas_cotizadas,
        cve_doc_probatorio,
        f_nacimiento ,
        aseguradora ,
        actuario,
        num_plan_privado,
        periodo_primer_pago ,
        aivs_viv97   ,    
        aivs_viv92   ,    
        consec_trabajador,
        importe_viv72 ,
        estado_sub_viv ,
        cve_afore ,
        cod_rechazo
      FROM ret_disposicion    
     WHERE folio = p_folio
      AND estado_solicitud = 100 -- RECHAZADA
    	--cur_salidadetalledisposicion INTO   
      -- se escribe cada registro de detalle
      FOREACH  cur_salidadetalledisposicion INTO 
      	  v_id_derechohabiente,   
      	  r_tmp_ret_det_disposicion.sec_pension,
      	  r_tmp_ret_det_disposicion.curp          , 
      	  r_tmp_ret_det_disposicion.nombre_afore  ,
          r_tmp_ret_det_disposicion.paterno_afore ,
          r_tmp_ret_det_disposicion.materno_afore ,
          r_tmp_ret_det_disposicion.f_inicio_pension  , 
          r_tmp_ret_det_disposicion.f_emision_resol   ,  
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

          --se extrae el n�mero de seguridad social 
      	  SELECT nss
      	    INTO r_tmp_ret_det_disposicion.nss
            FROM afi_derechohabiente
           WHERE id_derechohabiente = v_id_derechohabiente
          
          --se asignan variables segun  layout 
          LET r_tmp_ret_det_disposicion.tpo_registro      = "03"
          LET r_tmp_ret_det_disposicion.id_servicio       = "04"  
          LET r_tmp_ret_det_disposicion.id_operacion      = "08"
          LET r_tmp_ret_det_disposicion.tpo_retiro        = "0"
          LET r_tmp_ret_det_disposicion.regimen           = "73"
          LET r_tmp_ret_det_disposicion.tpo_seguro        = "00"
          LET r_tmp_ret_det_disposicion.tpo_pension       = "00"
          LET r_tmp_ret_det_disposicion.tpo_prestacion    = "00"
          LET r_tmp_ret_det_disposicion.f_solicitud_trab  = "00000000"
          LET r_tmp_ret_det_disposicion.acciones_ret97    = "1"
          LET r_tmp_ret_det_disposicion.acciones_cv       = "0000000000000"
          LET r_tmp_ret_det_disposicion.acciones_cuotsol  = "0000000000000"
          LET r_tmp_ret_det_disposicion.filler1           = "00000000"
          LET r_tmp_ret_det_disposicion.acciones_ret92    = "0000000000000"
          LET r_tmp_ret_det_disposicion.filler2           = "00000000"
          LET r_tmp_ret_det_disposicion.diagnostico_reg   = "000"
          LET r_tmp_ret_det_disposicion.filler3           = "00000000"
          LET r_tmp_ret_det_disposicion.result_operacion  = "02"
          LET r_tmp_ret_det_disposicion.motivo_rech2      = "000"

          --se forma el detalle a escribir en el archivo}
          
          LET v_f_inicio_pension = YEAR(r_tmp_ret_det_disposicion.f_inicio_pension) USING "&&&&",MONTH(r_tmp_ret_det_disposicion.f_inicio_pension) USING "&&",DAY(r_tmp_ret_det_disposicion.f_inicio_pension) USING "&&"
          LET v_f_resolucion     = YEAR(r_tmp_ret_det_disposicion.f_emision_resol) USING "&&&&",MONTH(r_tmp_ret_det_disposicion.f_emision_resol) USING "&&",DAY(r_tmp_ret_det_disposicion.f_emision_resol) USING "&&"
          LET v_f_nacimiento     = YEAR(r_tmp_ret_det_disposicion.f_nacimiento) USING "&&&&",MONTH(r_tmp_ret_det_disposicion.f_nacimiento) USING "&&",DAY(r_tmp_ret_det_disposicion.f_nacimiento) USING "&&"
          LET v_f_solicitud_trab = YEAR(r_tmp_ret_det_disposicion.f_solicitud_trab) USING "&&&&",MONTH(r_tmp_ret_det_disposicion.f_solicitud_trab) USING "&&",DAY(r_tmp_ret_det_disposicion.f_solicitud_trab) USING "&&"
          
          LET v_s_detalle = r_tmp_ret_det_disposicion.tpo_registro,
          r_tmp_ret_det_disposicion.id_servicio,
          r_tmp_ret_det_disposicion.id_operacion,
          r_tmp_ret_det_disposicion.sec_pension,
      	  r_tmp_ret_det_disposicion.curp          , 
      	  r_tmp_ret_det_disposicion.nombre_afore  ,
          r_tmp_ret_det_disposicion.paterno_afore ,
          r_tmp_ret_det_disposicion.materno_afore ,
          v_f_inicio_pension  , 
          v_f_resolucion      ,  
          r_tmp_ret_det_disposicion.porc_valuacion    ,  
          r_tmp_ret_det_disposicion.sem_cotizadas     ,  
          r_tmp_ret_det_disposicion.cve_doc_probatorio, 
          v_f_nacimiento  , 
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
          r_tmp_ret_det_disposicion.motivo_rech1,
          r_tmp_ret_det_disposicion.tpo_registro ,
          r_tmp_ret_det_disposicion.id_servicio  ,  
          r_tmp_ret_det_disposicion.id_operacion ,
          r_tmp_ret_det_disposicion.tpo_retiro       ,
          r_tmp_ret_det_disposicion.regimen          ,
          r_tmp_ret_det_disposicion.tpo_seguro       ,
          r_tmp_ret_det_disposicion.tpo_pension      ,
          r_tmp_ret_det_disposicion.tpo_prestacion   ,
          v_f_solicitud_trab,
          r_tmp_ret_det_disposicion.acciones_ret97   ,
          r_tmp_ret_det_disposicion.acciones_cv      ,
          r_tmp_ret_det_disposicion.acciones_cuotsol ,
          r_tmp_ret_det_disposicion.filler1          ,
          r_tmp_ret_det_disposicion.acciones_ret92   ,
          r_tmp_ret_det_disposicion.filler2          ,
          r_tmp_ret_det_disposicion.diagnostico_reg  ,
          r_tmp_ret_det_disposicion.filler3          ,
          r_tmp_ret_det_disposicion.result_operacion ,
          r_tmp_ret_det_disposicion.motivo_rech2
        
        --se escribe el detalle en el archivo 
        CALL v_ch_arch_solTransf.writeLine([v_s_detalle])
        --se incrementa el contador de registros 
        LET v_i_contador_registros = v_i_contador_registros + 1 

      END FOREACH
      FREE cur_salidadetalledisposicion

      --se asigana valores segun Layout 
      LET  r_tmp_ret_sum_disposicion.tpo_registro       = "09"
      LET  r_tmp_ret_sum_disposicion.id_servicio        = "04"
      LET  r_tmp_ret_sum_disposicion.tpo_entidad_origen = "00"
      LET  r_tmp_ret_sum_disposicion.cve_entidad_origen = "000"
      LET  r_tmp_ret_sum_disposicion.tpo_entidad_destino= "00"
      LET  r_tmp_ret_sum_disposicion.cve_entidad_destino= "000" 
      LET  v_f_operacion = YEAR(r_tmp_ret_sum_disposicion.f_operacion ) USING "&&&&",MONTH(r_tmp_ret_sum_disposicion.f_operacion ) USING "&&",DAY(r_tmp_ret_sum_disposicion.f_operacion ) USING "&&"
      LET  r_tmp_ret_sum_disposicion.f_operacion        = r_tmp_ret_cza_dispo.f_operacion USING "yyyymmdd"
      LET  r_tmp_ret_sum_disposicion.total_registros    = v_d_total_registro
      LET  r_tmp_ret_sum_disposicion.filler             = "0000000000000000000000000"
      --se forma el suamario
      LET v_s_sumario =  r_tmp_ret_sum_disposicion.tpo_registro       ,
           r_tmp_ret_sum_disposicion.id_servicio        ,
           r_tmp_ret_sum_disposicion.tpo_entidad_origen ,
           r_tmp_ret_sum_disposicion.cve_entidad_origen ,
           r_tmp_ret_sum_disposicion.tpo_entidad_destino,
           r_tmp_ret_sum_disposicion.cve_entidad_destino,
           v_f_operacion       ,
           r_tmp_ret_sum_disposicion.total_registros    ,
           r_tmp_ret_sum_disposicion.filler

      -- se escribe el sumario en el archivo                   
      CALL v_ch_arch_solTransf.writeLine([v_s_sumario])
   --se cierra el archivo   
   CALL v_ch_arch_solTransf.CLOSE()
   
END FUNCTION --fn_archivo_salida