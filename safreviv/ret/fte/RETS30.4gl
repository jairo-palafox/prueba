--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
##############################################################################################
#Modulo            =>RET                                                                     #
#Programa          =>RETS10                                                                  #
#Objetivo          =>Programa que ejecuta el proceso de generacion                           #
#                    de archivo de salida de retiros por                                     #
#                    tipo n                                                                  #
##############################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl" --Archivo que almacena las variables globales del modulo
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
   
   -- Llamado a función que genera el archivo de salida
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
DEFINE r_tmp_ret_det_tipo_n RECORD     
         tpo_registro         CHAR(2)  ,
         id_servicio          CHAR(2)  ,
         id_operacion         CHAR(2)  ,
         nss_icefa            CHAR(11) ,
         rfc_icefa            CHAR(13) ,
         nci_icefa            CHAR(30) ,
         cve_icefa            CHAR(13) ,
         nombre_icefa         CHAR(120),
         nss_id_presentada    CHAR(11) ,
         rfc_id_presentada    CHAR(13) ,
         nombre_id_presentada  CHAR(120)   ,
         docto_probatorio     CHAR(1) ,
         num_referencia       CHAR(18),
         origen_retiro        CHAR(1) ,
         tpo_seguro           CHAR(2) ,
         tpo_pension          CHAR(2) ,
         tpo_prestacion       CHAR(2) ,
         regimen              CHAR(8) ,
         f_inicio_pension     DATE ,
         f_resolucion         DATE ,
         porc_valuacion       DECIMAL(5,0) ,
         actuario             CHAR(7),
         registro_ppp         CHAR(8),
         importe_ret92        DECIMAL(15,0),
         aivs_viv92           DECIMAL(15,0),
         diagnostico          CHAR(3) ,
         cve_afore            CHAR(3) ,
         filler1              CHAR(16),
         result_operacion     CHAR(2) ,
         motivo_rech1         CHAR(3) ,
         motivo_rech2         CHAR(3) ,
         motivo_rech3         CHAR(3)
       END RECORD    

DEFINE r_tmp_ret_cza_tipo_n   RECORD 
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
END RECORD
DEFINE  r_tmp_ret_sum_tipo_n RECORD
          tpo_registro         CHAR(2)      ,
          id_servicio          CHAR(2)      ,
          total_registros      DECIMAL(6,0) ,
          total_ret92          DECIMAL(17,0),
          total_viv92          DECIMAL(17,0),
          filler               CHAR(426)
END RECORD
DEFINE v_s_encabezado           STRING,       
       v_s_detalle              STRING,       
       v_i_contador_registros   INTEGER,
       v_s_sumario              STRING,       
       v_i_secuencia            INTEGER,       
       v_f_operacion            CHAR(8),       
       v_f_inicio_pension       CHAR(8),
       v_f_resolucion           CHAR(8),
       v_total_importe_ret92    DECIMAL (17,0),
       v_total_aivs_viv92       DECIMAL (17,0)

  INITIALIZE v_s_encabezado TO NULL
  INITIALIZE v_s_sumario TO NULL
  INITIALIZE v_s_detalle TO NULL
  LET v_i_contador_registros = 0
  LET v_total_importe_ret92  = 0
  LET v_total_aivs_viv92     = 0


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
   --modificación realizada el día 21 de abril de 2012 por Rubén Haro Castro
   --se modifica el nombre del archivo que comprende el año-mes-dia-consecutivo.disposicion
   --LET v_v_nom_archivo = "/" ||"solicitudes_liq_tesoreria_" || p_folio || ".ret"}
   LET v_v_nom_archivo = "/",YEAR(TODAY) USING "&&&&",MONTH(TODAY) USING "&&",DAY(TODAY) using "&&",v_i_secuencia USING "&&&",".Rech_TipoN"     

   
   
   --DISPLAY "  v_v_nom_archivo = ",v_v_nom_archivo

   LET v_v_ruta_nomarch = v_c_ruta_env_acr CLIPPED || v_v_nom_archivo
   --DISPLAY "  v_v_ruta_nomarch = ",v_v_ruta_nomarch
   -- se crea el manejador de archivo
   LET v_ch_arch_solTransf = base.Channel.create()
   
   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_solTransf.openFile(v_v_ruta_nomarch, "w" )
   CALL v_ch_arch_solTransf.setDelimiter("")
 
  -- se obtienen el encabezado del archivo
      SELECT       
        f_operacion_procesar 
      INTO
        r_tmp_ret_cza_tipo_n.f_operacion
      FROM ret_cza_tipo_n
     WHERE folio = p_folio

  --se asignan variables segun layout
  LET r_tmp_ret_cza_tipo_n.tpo_registro         =  "01"
  LET r_tmp_ret_cza_tipo_n.id_servicio          =  "04"
  LET r_tmp_ret_cza_tipo_n.tpo_entidad_origen   =  "00"
  LET r_tmp_ret_cza_tipo_n.cve_entidad_origen   =  "000"
  LET r_tmp_ret_cza_tipo_n.tpo_entidad_destino  =  "00"
  LET r_tmp_ret_cza_tipo_n.cve_entidad_destino  =  "000"
  LET r_tmp_ret_cza_tipo_n.resultado_operacion  =  "02"
  LET r_tmp_ret_cza_tipo_n.motivo_rech_1        =  "111"  
  LET r_tmp_ret_cza_tipo_n.motivo_rech_2        =  "222"  
  LET r_tmp_ret_cza_tipo_n.motivo_rech_3        =  "333"  
  LET r_tmp_ret_cza_tipo_n.filler               =  "0000000000000000000"

  LET v_f_operacion  = YEAR(r_tmp_ret_cza_tipo_n.f_operacion) USING "&&&&",MONTH(r_tmp_ret_cza_tipo_n.f_operacion) USING "&&",DAY(r_tmp_ret_cza_tipo_n.f_operacion) USING "&&"

  --se asignan los datos del encabezado para escribir en el archivo
  LET v_s_encabezado =  r_tmp_ret_cza_tipo_n.tpo_registro,
  r_tmp_ret_cza_tipo_n.id_servicio,   
  r_tmp_ret_cza_tipo_n.tpo_entidad_origen  , 
  r_tmp_ret_cza_tipo_n.cve_entidad_origen  , 
  r_tmp_ret_cza_tipo_n.tpo_entidad_destino , 
  r_tmp_ret_cza_tipo_n.cve_entidad_destino ,  
  v_f_operacion, 
  r_tmp_ret_cza_tipo_n.resultado_operacion ,
  r_tmp_ret_cza_tipo_n.motivo_rech_1 ,
  r_tmp_ret_cza_tipo_n.motivo_rech_2 ,
  r_tmp_ret_cza_tipo_n.motivo_rech_3 ,
  r_tmp_ret_cza_tipo_n.filler        
                        
  --DISPLAY "v_s_encabezado =  ", v_s_encabezado
  CALL v_ch_arch_solTransf.writeLine([v_s_encabezado])
                   
  DECLARE cur_salidadetalledisposicion CURSOR FOR    
      SELECT
       nss_icefa                     ,
       rfc_icefa                     ,
       num_ctr_interno               ,   
       cve_icefa                     ,
       rfc                           ,
       nombre                        ,
       cve_doc_probatorio            ,
       num_referencia                ,
       origen_retiro                 ,
       f_inicio_pension              ,  
       f_resolucion                  ,  
       porcentaje_valuacion          ,
       actuario                      ,   
       num_plan_privado              ,
       importe_sar92                 ,
       aivs_viv92                    ,
       cve_afore                     ,
       cod_rechazo

      FROM ret_tipo_n
     WHERE folio = p_folio
      AND estado_solicitud = 100 -- RECHAZADA
    	--cur_salidadetalledisposicion INTO   
      -- se escribe cada registro de detalle
      FOREACH  cur_salidadetalledisposicion INTO 

         r_tmp_ret_det_tipo_n.nss_icefa           ,
         r_tmp_ret_det_tipo_n.rfc_icefa           ,
         r_tmp_ret_det_tipo_n.nci_icefa           ,
         r_tmp_ret_det_tipo_n.cve_icefa           ,
         r_tmp_ret_det_tipo_n.rfc_id_presentada   ,
         r_tmp_ret_det_tipo_n.nombre_icefa        ,
         r_tmp_ret_det_tipo_n.docto_probatorio    ,
         r_tmp_ret_det_tipo_n.num_referencia      ,
         r_tmp_ret_det_tipo_n.origen_retiro       ,
         r_tmp_ret_det_tipo_n.f_inicio_pension    ,
         r_tmp_ret_det_tipo_n.f_resolucion        ,
         r_tmp_ret_det_tipo_n.porc_valuacion      ,
         r_tmp_ret_det_tipo_n.actuario            ,
         r_tmp_ret_det_tipo_n.registro_ppp        ,
         r_tmp_ret_det_tipo_n.importe_ret92       ,
         r_tmp_ret_det_tipo_n.aivs_viv92          ,
         r_tmp_ret_det_tipo_n.cve_afore           ,
         r_tmp_ret_det_tipo_n.motivo_rech1


         -- se hace la suma de totales 
         LET v_total_importe_ret92  = v_total_importe_ret92 +  r_tmp_ret_det_tipo_n.importe_ret92
         LET v_total_aivs_viv92     = v_total_aivs_viv92    +  r_tmp_ret_det_tipo_n.aivs_viv92
         
         --se asignan variables segun  layout 
         LET v_f_inicio_pension = YEAR(r_tmp_ret_det_tipo_n.f_inicio_pension) USING "&&&&",MONTH(r_tmp_ret_det_tipo_n.f_inicio_pension) USING "&&",DAY(r_tmp_ret_det_tipo_n.f_inicio_pension) USING "&&"
         LET v_f_resolucion     = YEAR( r_tmp_ret_det_tipo_n.f_resolucion ) USING "&&&&",MONTH( r_tmp_ret_det_tipo_n.f_resolucion ) USING "&&",DAY( r_tmp_ret_det_tipo_n.f_resolucion ) USING "&&"          
         
         LET r_tmp_ret_det_tipo_n.tpo_registro         = "02"  
         LET r_tmp_ret_det_tipo_n.id_servicio          = "04"  
         LET r_tmp_ret_det_tipo_n.id_operacion         = "23"
         LET r_tmp_ret_det_tipo_n.diagnostico          = "000"
         LET r_tmp_ret_det_tipo_n.filler1              = "00000000"
         LET r_tmp_ret_det_tipo_n.result_operacion     = "02"
         LET r_tmp_ret_det_tipo_n.motivo_rech2         = "000"
         LET r_tmp_ret_det_tipo_n.motivo_rech3         = "000"
         LET r_tmp_ret_det_tipo_n.tpo_registro         = "02"
         LET r_tmp_ret_det_tipo_n.id_servicio          = "04"
         LET r_tmp_ret_det_tipo_n.id_operacion         = "23"
         
         --se construye la cadena de detalle 
         LET v_s_detalle = r_tmp_ret_det_tipo_n.tpo_registro,
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
           v_f_inicio_pension                         ,
           v_f_resolucion                             ,
           r_tmp_ret_det_tipo_n.porc_valuacion        ,
           r_tmp_ret_det_tipo_n.actuario              ,
           r_tmp_ret_det_tipo_n.registro_ppp          ,
           r_tmp_ret_det_tipo_n.importe_ret92         ,
           r_tmp_ret_det_tipo_n.aivs_viv92            ,
           r_tmp_ret_det_tipo_n.diagnostico           ,
           r_tmp_ret_det_tipo_n.cve_afore             ,
           r_tmp_ret_det_tipo_n.filler1               ,
           r_tmp_ret_det_tipo_n.result_operacion      ,
           r_tmp_ret_det_tipo_n.motivo_rech1          ,
           r_tmp_ret_det_tipo_n.motivo_rech2          ,
           r_tmp_ret_det_tipo_n.motivo_rech3          
         
         
          
          --se forma el detalle a escribir en el archivo
        CALL v_ch_arch_solTransf.writeLine([v_s_detalle])

         --se incrementa el contador de registros 
        LET v_i_contador_registros = v_i_contador_registros + 1 

      END FOREACH
     
      FREE cur_salidadetalledisposicion
      
          --se asigana variables segun Layout
       LET r_tmp_ret_sum_tipo_n.tpo_registro     = "09"
       LET r_tmp_ret_sum_tipo_n.id_servicio      = "04"
       LET r_tmp_ret_sum_tipo_n.total_registros  = v_i_contador_registros
       LET r_tmp_ret_sum_tipo_n.total_ret92      = v_total_importe_ret92
       LET r_tmp_ret_sum_tipo_n.total_viv92      = v_total_aivs_viv92
       LET r_tmp_ret_sum_tipo_n.filler           = "0000000000000000000000000"

      --se asigana valores a la cadena de sumario 
      LET v_s_sumario =
          r_tmp_ret_sum_tipo_n.tpo_registro     ,
          r_tmp_ret_sum_tipo_n.id_servicio      ,
          r_tmp_ret_sum_tipo_n.total_registros  ,
          r_tmp_ret_sum_tipo_n.total_ret92      ,
          r_tmp_ret_sum_tipo_n.total_viv92      ,
          r_tmp_ret_sum_tipo_n.filler
     
      -- se escribe el registro
      CALL v_ch_arch_solTransf.writeLine([v_s_sumario])

   --se cierra e archivo
   CALL v_ch_arch_solTransf.CLOSE()

END FUNCTION --fn_archivo_salida