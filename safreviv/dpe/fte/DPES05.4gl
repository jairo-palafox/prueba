--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 03/01/2013
--===============================================================

####################################################################
#Modulo            =>DPE                                           #
#Programa          =>DPES05                                        #
#Objetivo          =>Programa que ejecuta el proceso de generación #
#                    de archivo de la integración complementaria   #
#Fecha inicio      => 03/01/2013                                   #
####################################################################
GLOBALS "DPEG01.4gl"
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod, -- codigo de operacion
       v_layout       LIKE cat_operacion.layout_cod,
       v_ruta_rescate STRING,
       v_usuario      LIKE seg_modulo.usuario,
       v_proceso_desc LIKE cat_proceso.proceso_desc,
       v_extension    LIKE cat_operacion.extension,
       v_opera_desc   LIKE cat_operacion.opera_desc,
       v_ruta_listados         LIKE seg_modulo.ruta_listados,
       g_folio_sol_trabajador  DECIMAL(9,0),
       c_pago_total_nss                   SMALLINT, -- = 0               
       c_pago_parcial_nss                 SMALLINT, -- = 1             
       c_pago_por_preliquidar_total       SMALLINT, -- = 2   
       c_pago_por_preliquidar_parcial     SMALLINT, -- = 3 
       c_pago_preliquidado_total          SMALLINT, -- = 4      
       c_pago_preliquidado_parcial        SMALLINT, -- = 5    
       c_pago_liquidado_total             SMALLINT, -- = 6         
       c_pago_liquidado_parcial           SMALLINT, -- = 7       
       c_pago_enviado_dependecia_total    SMALLINT, -- = 8  
       c_pago_enviado_dependencia_parcial SMALLINT  -- = 9 
END GLOBALS

MAIN
DEFINE p_pid            LIKE bat_ctr_operacion.pid, -- PID del proceso
       p_proceso_cod    LIKE bat_ctr_operacion.proceso_cod, -- codigo del proceso
       p_opera_cod      LIKE bat_ctr_operacion.opera_cod, -- codigo de la operacion
       p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_folio          LIKE deo_preliquida.folio_liquida, -- folio de la operacion
       p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo, -- nombre dle archivo
       v_s_sql          STRING, -- cadena con una instruccion SQL
       v_i_resultado    INTEGER, -- resultado del proceso
       r_bnd_fin_oper   SMALLINT       

   -- se recuperan los parametros que envia el programa lanzador
   -- usuario, pid, proceso_cod, opera_cod, folio, nombre_archivo
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)

   DISPLAY "FOLIO:", p_folio

   --Recupera folio de integracion IMSS
   SELECT MAX (folio)
   INTO   g_folio_sol_trabajador 
   FROM   dpe_sol_trabajador 
   WHERE  folio_respuesta = p_folio
   --GROUP BY 1
   
   CALL fn_recupera_inf_proceso(p_proceso_cod, p_opera_cod) 
                               RETURNING v_proceso_desc,
                                         v_extension, 
                                         v_opera_desc,
                                         v_layout, 
                                         v_ruta_rescate,
                                         v_ruta_listados,
                                         v_usuario
   
   -- se asigna proceso y operacion
   LET g_pid         = p_pid      
   LET g_proceso_cod = p_proceso_cod -- devolucion 
   LET g_opera_cod   = p_opera_cod   -- genera archivo procesar

   -- se obtiene el PID del proceso
   SELECT MAX(pid)
     INTO g_pid
     FROM bat_ctr_proceso
    WHERE proceso_cod = g_proceso_cod
      -- Llamado a función que genera el archivo de salida
      CALL fn_archivo_salida_IMSS(p_usuario_cod, p_folio)
      
      -- se invoca la finalizacion de la operacion
      CALL fn_actualiza_opera_fin(g_pid,    --- Identificador del proceso
           g_proceso_cod, --- Clave del proceso
           g_opera_cod) --- Clave de la operación
         RETURNING r_bnd_fin_oper
END MAIN

{
======================================================================
Clave: 
Nombre: fn_archivo_salida_INFONAVIT
Fecha creacion: 03/05/2012
Narrativa del proceso que realiza:
Genera el archivo de salida para dpe solo INFONAVIT
======================================================================
}
FUNCTION fn_archivo_salida_IMSS(p_usuario_cod, p_folio)
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_folio, v_folio  LIKE glo_folio.folio, -- folio para preliquidar
       r_bnd_fin_oper   SMALLINT,
       p_titulo       STRING, -- titulo del mensaje enviado en el correo
       p_mensaje      STRING, -- cuerpo del mensaje enviado
       v_r_dpe_cza_solicitud RECORD --Record almacena encabezado de solicitud
          v_d_tpo_registro        CHAR(2),
          v_c_ide_servicio        CHAR(2),
          v_c_ide_operacion       CHAR(2),
          v_c_tpo_entidad_origen  CHAR(2),
          v_c_cve_entidad_origen  CHAR(3),
          v_c_tpo_entidad_destino CHAR(2),
          v_c_cve_entidad_destino CHAR(3),
          v_d_f_transferencia     DATE,
          v_s_num_consecutivo     SMALLINT,
          v_c_modalidad_archivo   CHAR(2),
          v_c_filler              CHAR(266)
       END RECORD,
       v_r_dpe_cza_notificacion RECORD --Record almacena encabezado notificación
          v_d_tpo_registro        CHAR(2),                                   
          v_c_ide_servicio        CHAR(2),                                   
          v_c_ide_operacion       CHAR(2),                                   
          v_d_f_envio_solicitud   CHAR(8),--DATE,                                      
          v_c_filler              CHAR(281)                                  
       END RECORD,                                                           
       v_r_dpe_patron RECORD --Record almacena encabezado del patrón
           tipo_reg 	     CHAR(2),
           num_reg_pat_imss  CHAR(11),
           rfc_patron        CHAR(13),
           per_pago          CHAR(6),
           folio_sua 		 DECIMAL(6,0),
           nombre_razon_soc  CHAR(50),
           num_solicitud     CHAR(13),
           tipo_cotizacion   CHAR(1),
           num_trab_sol      DECIMAL(7,0),
           fec_pago 		 CHAR(8),--DATE,
           fec_valor_viv 	 CHAR(8),--DATE,
           fec_valor_rcv     CHAR(8),--DATE,
           resultado_op      CHAR(2),
           delegacion        CHAR(2),
           subdelegacion 	 CHAR(2),
           filler 		     CHAR(156)
       END RECORD,
          v_r_dpe_sol_trabajador RECORD --Record almacena detalle del trabajador
             v_tipo_reg 	          CHAR(2), 
             v_id_servicio 	          CHAR(2), 
             v_num_reg_pat_imss       CHAR(11), 
             v_rfc_patron 	          CHAR(13), 
             v_per_pago 	          CHAR(6), 
             v_folio_sua 	          DECIMAL(6,0), 
             v_nss_aportacion 	      CHAR(11), 
             v_rfc_trabajador 	      CHAR(13), 
             v_curp 	              CHAR(18), 
             v_imp_ret_devolver       DECIMAL(7,2), 
             v_imp_act_recargos       DECIMAL(7,2), 
             v_imp_ces_vejez 	      DECIMAL(7,2), 
             v_imp_act_ces_vejez      DECIMAL(7,2),
             v_nss_unificador         CHAR(11), 
             v_imp_plusv_retiro       DECIMAL(7,2), 
             v_imp_minus_retiro       DECIMAL(7,2), 
             v_imp_comisiones_afore   DECIMAL(7,2), 
             v_imp_total_apo_patronal DECIMAL(7,2), 
             v_imp_plusv_apo_patronal DECIMAL(7,2), 
             v_clave_afore_trabajador CHAR(3),      
             v_num_aplicaciones_inter DECIMAL(18,6),  
             v_resultado_operacion    CHAR(2),       
             v_diagnostico1 	      CHAR(3),       
             v_diagnostico2 	      CHAR(3),       
             v_diagnostico3 	      CHAR(3),       
             v_filler 	              CHAR(92) 
          END RECORD,
          v_r_dpe_sum_patron RECORD 
             v_c_tpo_registro           CHAR(02),     
             v_num_reg_pat_imss         CHAR(11),     
             v_rfc_patron               CHAR(13),     
             v_per_pago                 CHAR(6),      
             v_folio_sua                DECIMAL(6,0) ,
             v_imp_ret_devolver         DECIMAL(9,2), 
             v_imp_act_recargos         DECIMAL(9,2), 
             v_imp_ces_vejez            DECIMAL(9,2), 
             v_imp_act_ces_vejez        DECIMAL(9,2), 
             v_imp_tot_plusv_retiro     DECIMAL(9,2), 
             v_imp_tot_minus_retiro     DECIMAL(9,2), 
             v_imp_comisiones_afore     DECIMAL(9,2), 
             v_imp_total_apo_patronal   DECIMAL(9,2),             
             v_imp_plusv_apo_patronal   DECIMAL(9,2), 
             v_total_regs_solicitud     DECIMAL(7,0) ,
             v_total_regs_aceptados     DECIMAL(7,0) ,
             v_total_regs_rechazados    DECIMAL(7,0) ,
             v_total_regs_dev_parcial   DECIMAL(7,0) ,
             v_total_regs_dev_pendiente DECIMAL(7,0) ,
             v_num_aplicaciones_inter   DECIMAL(18,6),
             v_filler                   CHAR(105)     
          END RECORD,
          v_r_dpe_sum_exceso RECORD 
             v_tipo_reg                 CHAR(2)      ,
             v_id_servicio              CHAR(2)      ,
             v_id_operacion             CHAR(2)      ,
             v_fec_envio_original       CHAR(8),--DATE ,
             v_tot_solicitudes_lote     DECIMAL(7,0) ,
             v_tot_registros_lote       DECIMAL(7,0) ,
             v_tot_regs_aceptados       DECIMAL(7,0) ,
             v_tot_regs_rechazados      DECIMAL(7,0) ,
             v_tot_regs_dev_parcial     DECIMAL(7,0) ,
             v_tot_regs_dev_pendiente   DECIMAL(7,0) ,
             v_imp_liquidar_retiro      DECIMAL(15,2),
             v_imp_liquidar_plusv_rcv   DECIMAL(15,2),
             v_imp_liquidar_minus_rcv   DECIMAL(15,2),
             v_imp_comisiones_rcv       DECIMAL(15,2),
             v_imp_liquidar_vivienda    DECIMAL(15,2),
             v_imp_liquidar_plusv_viv   DECIMAL(15,2),
             v_num_aplicaciones_inter   DECIMAL(18,6),
             v_filler                   CHAR(131)
          END RECORD,
          v_r_dpe_sum_notificacion RECORD 
             v_tipo_reg 	          CHAR(2),         
             v_id_servicio            CHAR(2),       
             v_id_operacion           CHAR(2),       
             v_tipo_ent_origen  	  CHAR(2),         
             v_cve_ent_origen         CHAR(3),       
             v_tipo_ent_destino       CHAR(2),       
             v_cve_ent_destino 	      CHAR(3),       
             v_fec_transferencia      DATE   ,       
             v_consecutivo_dia        SMALLINT,      
             v_total_lotes            SMALLINT,      
             v_imp_liquidar_retiro    DECIMAL(13,2), 
             v_imp_liquidar_plusv_rcv DECIMAL(13,2), 
             v_imp_minusvalia_rcv     DECIMAL(13,2), 
             v_imp_comisiones_rcv     DECIMAL(13,2), 
             v_imp_liquidar_vivienda  DECIMAL(13,2), 
             v_imp_liquidar_plusv_viv DECIMAL(13,2), 
             v_num_aplicaciones_inter DECIMAL(12,6), 
             v_filler                 CHAR(153)
          END RECORD 

   DEFINE v_id_dpe_referencia         DECIMAL(9,0)
          
DEFINE v_v_nom_archivo     CHAR(40), -- nombre del archivo de salida
       v_v_ruta_nomarch    VARCHAR(100), -- ruta y nombre del archivo de salida
       v_c_ruta_env_dpe    LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
       v_ch_arch_solTransf BASE.CHANNEL, -- manejador de apuntador hacia archivo
       v_s_sql                         VARCHAR(1000),-- Cadena que contiene las consultas a ejecutar
       cont_cza_solicitud              INTEGER, -- Contador de encabezado de solicitudes
       cont_cza_notificacion           INTEGER, -- Contador de encabezado de solicitudes
       v_s_tot_solicitudes             INTEGER, -- Total de patrones solo registros 02
       v_s_tot_trabajadores_aceptadas  INTEGER, -- Total de trabajadores solo registros 03 aceptados
       v_s_tot_trabajadores_rechazadas INTEGER, -- Total de trabajadores solo registros 03 rechazados
       v_s_tot_trabajadores_aux        INTEGER,-- Total de trabajadores solo registros 03
       v_s_tot_registros               INTEGER, -- Total de registros solo registros <> 01 y 09
       v_s_registro                    STRING, -- registro a insertar
       v_d_hoy                  CHAR(8),
       v_busca_archivo          STRING,
       v_cont_dia               SMALLINT, -- consecutivo por dia de archivo generado
       v_reg_dia                SMALLINT -- Parametro consecutivo de registro por dia
DEFINE v_tot_exceso  INTEGER
DEFINE v_tot_sumario INTEGER,
       v_convierte_archivo STRING,
       FECHA_VIV CHAR(8)
   -- Se asignan valores costantes a las viables de totales o parciales
   ------valida si son pagos totales o parciales
   LET c_pago_total_nss                   = 0
   LET c_pago_parcial_nss                 = 1
   LET c_pago_por_preliquidar_total       = 2
   LET c_pago_por_preliquidar_parcial     = 3
   LET c_pago_preliquidado_total          = 4
   LET c_pago_preliquidado_parcial        = 5
   --------- Valida solo aceptados
   LET c_pago_liquidado_total             = 6
   LET c_pago_liquidado_parcial           = 7
   --------Actualizar a estos estados
   LET c_pago_enviado_dependecia_total    = 8
   LET c_pago_enviado_dependencia_parcial = 9
   
   -- se crear el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".DPES05.log")

-- se obtienen la ruta envio del modulo
    SELECT ruta_envio 
      INTO v_c_ruta_env_dpe
      FROM seg_modulo
     WHERE modulo_cod = 'dpe'
  
    -- se crea el nombre del archivo y posteriormente se concatena con la ruta
   LET v_d_hoy  = TODAY USING "DDMMYYYY"
   LET v_busca_archivo = "RES_COMPL"||v_d_hoy
   --Obtine consecutivo para archivo por día
   CALL fn_crea_nombre_archivo(v_c_ruta_env_dpe,v_busca_archivo)
        RETURNING v_cont_dia

   LET v_reg_dia = v_cont_dia USING "&"
   LET v_v_nom_archivo = "/"||v_busca_archivo||v_reg_dia CLIPPED||".dpe"
   LET v_v_ruta_nomarch = v_c_ruta_env_dpe CLIPPED || v_v_nom_archivo
   -- se crea el manejador de archivo
   LET v_ch_arch_solTransf = base.Channel.create()
   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_solTransf.openFile(v_v_ruta_nomarch, "w" )
   CALL v_ch_arch_solTransf.setDelimiter("")
   
   -- Se ingresan los registros del encabezado de la solicitud
   LET cont_cza_solicitud = 1
---------------------------------------------
   --Encabezado lote notificación a inst DPE  ,tmp_dpe_cza_lote_notificacion
  
      -- Asigna valores en codigo duro según especificación
      LET v_r_dpe_cza_solicitud.v_d_tpo_registro = "00"
      LET v_r_dpe_cza_solicitud.v_c_ide_servicio = "03"
      LET v_r_dpe_cza_solicitud.v_c_ide_operacion = "57"
      LET v_r_dpe_cza_solicitud.v_c_tpo_entidad_origen = "03"
      LET v_r_dpe_cza_solicitud.v_c_cve_entidad_origen = "001"
      LET v_r_dpe_cza_solicitud.v_c_tpo_entidad_destino = "04"
      LET v_r_dpe_cza_solicitud.v_c_cve_entidad_destino = "001"

      LET v_r_dpe_cza_solicitud.v_c_cve_entidad_destino = "002"
      LET v_r_dpe_cza_solicitud.v_d_f_transferencia     =  TODAY
      LET v_r_dpe_cza_solicitud.v_s_num_consecutivo     =  1
      LET v_r_dpe_cza_solicitud.v_c_modalidad_archivo   =  "02"
      LET v_r_dpe_cza_solicitud.v_c_filler              =  NULL
           
      -- Se llena la cadena para escribir en el archivo
      LET v_s_registro = v_r_dpe_cza_solicitud.v_d_tpo_registro,
                         v_r_dpe_cza_solicitud.v_c_ide_servicio,
                         v_r_dpe_cza_solicitud.v_c_ide_operacion,
                         v_r_dpe_cza_solicitud.v_c_tpo_entidad_origen,
                         v_r_dpe_cza_solicitud.v_c_cve_entidad_origen,
                         v_r_dpe_cza_solicitud.v_c_tpo_entidad_destino,
                         v_r_dpe_cza_solicitud.v_c_cve_entidad_destino,
                         v_r_dpe_cza_solicitud.v_d_f_transferencia USING "yyyymmdd",
                         v_r_dpe_cza_solicitud.v_s_num_consecutivo USING "&&&",
                         v_r_dpe_cza_solicitud.v_c_modalidad_archivo,
                         v_r_dpe_cza_solicitud.v_c_filler

      CALL v_ch_arch_solTransf.write([v_s_registro])
      
      LET cont_cza_solicitud = cont_cza_solicitud + 1
-----------------------------------
      LET cont_cza_notificacion = 0
      --Encabezado notificación de DAPE          ,tmp_dpe_cza_notificacion 
      LET v_s_sql = "\n SELECT fec_transferencia,' ' ",
                    "\n   FROM safre_tmp:tmp_dpe_cza_lote_notificacion"

      PREPARE prp_cza_notificacion FROM v_s_sql CLIPPED
      DECLARE cur_cza_notificacion CURSOR FOR prp_cza_notificacion 
      FOREACH cur_cza_notificacion  INTO v_r_dpe_cza_notificacion.v_d_f_envio_solicitud,
                                         v_r_dpe_cza_notificacion.v_c_filler

         LET v_r_dpe_cza_notificacion.v_d_tpo_registro  = "01"
         LET v_r_dpe_cza_notificacion.v_c_ide_servicio  = "03"
         LET v_r_dpe_cza_notificacion.v_c_ide_operacion = "57"

         LET v_s_registro = v_r_dpe_cza_notificacion.v_d_tpo_registro,
                            v_r_dpe_cza_notificacion.v_c_ide_servicio,
                            v_r_dpe_cza_notificacion.v_c_ide_operacion,
                            v_r_dpe_cza_notificacion.v_d_f_envio_solicitud,-- USING "yyyymmdd",
                            v_r_dpe_cza_notificacion.v_c_filler
                             
         CALL v_ch_arch_solTransf.write([v_s_registro])
         
         LET cont_cza_notificacion = cont_cza_notificacion + 1
------   ------------------
         LET v_s_tot_solicitudes = 0 -- Se inicializa el contador de patrones
         --Encabezado pago patronal notificación DPE,tmp_dpe_cza_pago_patronal    
         DECLARE cur_dpe_patron CURSOR FOR 
            SELECT "","02",               -- tipo_reg
                   a.num_reg_pat_imss, -- num_reg_pat_imss
                   a.rfc_patron,       -- rfc_patron
                   a.per_pago,         -- per_pago
                   a.folio_sua,        -- folio_sua
                   a.nombre_razon_soc, -- nombre_razon_soc
                   a.num_solicitud,    -- num_solicitud
                   a.tipo_cotizacion,  -- tipo_cotizacion
                   a.num_trab_sol,     -- num_trab_sol
                   a.fec_pago,         -- fec_pago
                   a.fec_valor_viv,    -- fec_valor_viv
                   "" ,                -- fec_valor_rcv,
                   "01",               --diagnostico
                   a.delegacion,       --delegacion 
                   a.subdelegacion,    --subdelegacion 
                   " "
            FROM   safre_tmp:tmp_dpe_cza_pago_patronal a
      
         LET v_s_tot_registros = 0
         
         -- Comienza el conteo de encabezado de patrones
         FOREACH cur_dpe_patron 
            INTO v_id_dpe_referencia,
                 v_r_dpe_patron.tipo_reg,
                 v_r_dpe_patron.num_reg_pat_imss, 
                 v_r_dpe_patron.rfc_patron      , 
                 v_r_dpe_patron.per_pago        , 
                 v_r_dpe_patron.folio_sua 		  ,  
                 v_r_dpe_patron.nombre_razon_soc, 
                 v_r_dpe_patron.num_solicitud   , 
                 v_r_dpe_patron.tipo_cotizacion , 
                 v_r_dpe_patron.num_trab_sol    , 
                 v_r_dpe_patron.fec_pago 		,
                 v_r_dpe_patron.fec_valor_viv 	,  
                 v_r_dpe_patron.fec_valor_rcv   , 
                 v_r_dpe_patron.resultado_op    , 
                 v_r_dpe_patron.delegacion      , 
                 v_r_dpe_patron.subdelegacion 	,  
                 v_r_dpe_patron.filler
                 
            LET v_r_dpe_patron.fec_valor_viv = v_r_dpe_patron.fec_valor_viv + 1
            
            LET v_s_registro = v_r_dpe_patron.tipo_reg,
                               v_r_dpe_patron.num_reg_pat_imss, 
                               v_r_dpe_patron.rfc_patron      , 
                               v_r_dpe_patron.per_pago        , 
                               v_r_dpe_patron.folio_sua       USING "&&&&&&",
                               v_r_dpe_patron.nombre_razon_soc, 
                               v_r_dpe_patron.num_solicitud   , 
                               v_r_dpe_patron.tipo_cotizacion USING "&", 
                               v_r_dpe_patron.num_trab_sol    USING "&&&&&&&", 
                               v_r_dpe_patron.fec_pago 		  ,--USING "yyyymmdd",
                               v_r_dpe_patron.fec_valor_viv   ,--USING "yyyymmdd",  
                               v_r_dpe_patron.fec_valor_rcv   ,--USING "yyyymmdd", 
                               v_r_dpe_patron.resultado_op    USING "&&" , 
                               v_r_dpe_patron.delegacion      USING "&&" , 
                               v_r_dpe_patron.subdelegacion   USING "&&",  
                               v_r_dpe_patron.filler 	
            
            CALL v_ch_arch_solTransf.write([v_s_registro])
            
            LET v_s_tot_registros = v_s_tot_registros + 1
            
            -- Se llena el detalle del trabajador aceptado
            DECLARE cur_dpe_sol_trabajador_aceptadas CURSOR FOR 
            SELECT "03",               -- tipo_reg,               
                   "03",               -- id_servicio,            
                   a.num_reg_pat_imss, -- num_reg_pat_imss,      
                   a.rfc_patron,       -- rfc_patron,            
                   a.per_pago,         -- per_pago,              
                   a.folio_sua,        -- folio_sua,             
                   a.nss_aportacion,   -- nss_aportacion,        
                   a.rfc_trabajador,   -- rfc_trabajador,        
                   a.curp,             -- curp,                  
                   a.imp_ret_devolver, -- imp_ret_devolver,      
                   a.imp_act_recargos, -- imp_act_recargos,      
                   a.imp_ces_vejez,    -- imp_ces_vejez,         
                   a.imp_act_ces_vejez,-- imp_act_ces_vejez,     
                   "           ",      -- v_nss_unificador       
                   a.imp_plusv_retiro, -- imp_plusv_retiro,      
                   0,                  -- imp_minus_retiro,      
                   0,                  -- imp_comisiones_afore,  
                   0,                  -- imp_total_apo_patronal,
                   0,                  -- imp_plusv_apo_patronal,
                   "000",              -- clave_afore_trabajador,
                   a.num_aplicaciones_inter, -- num_aplicaciones_inter,
                   a.resul_operacion_compl,  -- resultado_operacion,   
                   a.diagnostico,      -- diagnostico1,          
                   "000",              -- diagnostico2,          
                   "000",              -- diagnostico3,          
                   " "                 -- filler                 
            FROM  dpe_sol_trab_complementario a
            WHERE a.num_reg_pat_imss = v_r_dpe_patron.num_reg_pat_imss
            AND   a.per_pago         = v_r_dpe_patron.per_pago
            AND   a.folio_sua        = v_r_dpe_patron.folio_sua
            AND   a.folio_integra    = p_folio

            FOREACH cur_dpe_sol_trabajador_aceptadas 
               INTO v_r_dpe_sol_trabajador.v_tipo_reg,
                    v_r_dpe_sol_trabajador.v_id_servicio,
                    v_r_dpe_sol_trabajador.v_num_reg_pat_imss,
                    v_r_dpe_sol_trabajador.v_rfc_patron,
                    v_r_dpe_sol_trabajador.v_per_pago,
                    v_r_dpe_sol_trabajador.v_folio_sua,
                    v_r_dpe_sol_trabajador.v_nss_aportacion,
                    v_r_dpe_sol_trabajador.v_rfc_trabajador,
                    v_r_dpe_sol_trabajador.v_curp,
                    v_r_dpe_sol_trabajador.v_imp_ret_devolver,
                    v_r_dpe_sol_trabajador.v_imp_act_recargos,
                    v_r_dpe_sol_trabajador.v_imp_ces_vejez,
                    v_r_dpe_sol_trabajador.v_imp_act_ces_vejez,
                    v_r_dpe_sol_trabajador.v_nss_unificador,
                    v_r_dpe_sol_trabajador.v_imp_plusv_retiro,       
                    v_r_dpe_sol_trabajador.v_imp_minus_retiro,       
                    v_r_dpe_sol_trabajador.v_imp_comisiones_afore,
                    v_r_dpe_sol_trabajador.v_imp_total_apo_patronal,
                    v_r_dpe_sol_trabajador.v_imp_plusv_apo_patronal,
                    v_r_dpe_sol_trabajador.v_clave_afore_trabajador,
                    v_r_dpe_sol_trabajador.v_num_aplicaciones_inter,
                    v_r_dpe_sol_trabajador.v_resultado_operacion,
                    v_r_dpe_sol_trabajador.v_diagnostico1, 	          
                    v_r_dpe_sol_trabajador.v_diagnostico2, 	          
                    v_r_dpe_sol_trabajador.v_diagnostico3, 	          
                    v_r_dpe_sol_trabajador.v_filler         
               IF  v_r_dpe_sol_trabajador.v_resultado_operacion = 2 THEN
               --IF  v_r_dpe_sol_trabajador.v_diagnostico1 = 21 THEN
                   LET v_r_dpe_sol_trabajador.v_diagnostico1 = 382
               END IF  

               --Cambia resultaod y diagnostico en detalle archivo    
               IF  v_r_dpe_sol_trabajador.v_resultado_operacion = 1 AND 
                   v_r_dpe_sol_trabajador.v_diagnostico1 = 1 THEN
                   LET v_r_dpe_sol_trabajador.v_resultado_operacion = 4
                   LET v_r_dpe_sol_trabajador.v_diagnostico1 = 0
               END IF  

               --DISPLAY "Actualiza los registros totales a 8 enviados a dependencia"
               -- Actualiza los registros totales a 8 enviados a dependencia
               UPDATE dpe_sol_trab_parcial
                  SET diagnostico = c_pago_enviado_dependecia_total
                WHERE folio = p_folio
                  AND reg_patronal_imss = v_r_dpe_sol_trabajador.v_num_reg_pat_imss
                  AND periodo_pago = v_r_dpe_sol_trabajador.v_per_pago
                  AND diagnostico = c_pago_liquidado_total

               UPDATE dpe_sol_trab_parcial
                 SET diagnostico = c_pago_enviado_dependencia_parcial
               WHERE folio = p_folio
                 AND reg_patronal_imss = v_r_dpe_sol_trabajador.v_num_reg_pat_imss
                 AND periodo_pago = v_r_dpe_sol_trabajador.v_per_pago
                 AND diagnostico = c_pago_liquidado_total
            
               LET v_s_registro = v_r_dpe_sol_trabajador.v_tipo_reg,
                                  v_r_dpe_sol_trabajador.v_id_servicio,
                                  v_r_dpe_sol_trabajador.v_num_reg_pat_imss,
                                  v_r_dpe_sol_trabajador.v_rfc_patron,
                                  v_r_dpe_sol_trabajador.v_per_pago,
                                  v_r_dpe_sol_trabajador.v_folio_sua USING "&&&&&&",
                                  v_r_dpe_sol_trabajador.v_nss_aportacion,
                                  v_r_dpe_sol_trabajador.v_rfc_trabajador,
                                  v_r_dpe_sol_trabajador.v_curp,
                                  v_r_dpe_sol_trabajador.v_imp_ret_devolver  * 100 USING  "&&&&&&&&&",
                                  v_r_dpe_sol_trabajador.v_imp_act_recargos  * 100 USING  "&&&&&&&&&",
                                  v_r_dpe_sol_trabajador.v_imp_ces_vejez 	 * 100 USING  "&&&&&&&&&",
                                  v_r_dpe_sol_trabajador.v_imp_act_ces_vejez * 100 USING  "&&&&&&&&&",
                                  v_r_dpe_sol_trabajador.v_nss_unificador,
                                  v_r_dpe_sol_trabajador.v_imp_plusv_retiro  * 100 USING  "&&&&&&&&&",       
                                  v_r_dpe_sol_trabajador.v_imp_minus_retiro  * 100 USING  "&&&&&&&&&",       
                                  v_r_dpe_sol_trabajador.v_imp_comisiones_afore * 100 USING  "&&&&&&&&&",
                                  v_r_dpe_sol_trabajador.v_imp_total_apo_patronal * 100 USING  "&&&&&&&&&",
                                  v_r_dpe_sol_trabajador.v_imp_plusv_apo_patronal * 100 USING  "&&&&&&&&&",
                                  v_r_dpe_sol_trabajador.v_clave_afore_trabajador USING "&&&",
                                  v_r_dpe_sol_trabajador.v_num_aplicaciones_inter * 1000000 USING  "&&&&&&&&&&&&&&&",
                                  v_r_dpe_sol_trabajador.v_resultado_operacion USING  "&&" ,
                                  v_r_dpe_sol_trabajador.v_diagnostico1        USING  "&&&", 	          
                                  v_r_dpe_sol_trabajador.v_diagnostico2 	   USING  "&&&", 	                 
                                  v_r_dpe_sol_trabajador.v_diagnostico3 	   USING  "&&&", 	                 
                                  v_r_dpe_sol_trabajador.v_filler 	               
                               
               LET v_s_tot_trabajadores_aceptadas = v_s_tot_trabajadores_aceptadas + 1
               
               LET v_s_tot_registros = v_s_tot_registros + 1
               
               CALL v_ch_arch_solTransf.write([v_s_registro])

            END FOREACH -- Detalle trabajador aceptados

            -- Se llena el sumario del patron
            DECLARE cur_dpe_sum_patron CURSOR FOR 
            SELECT num_reg_pat_imss,  -- num_reg_pat_imss,	      
                   rfc_patron,                -- rfc_patron,	            
                   per_pago,       -- per_pago,	              
                   folio_sua,          -- folio_sua,	              
                   0.0,                -- imp_ret_devolver,        
                   0.0,                -- imp_act_recargos,        
                   0.0,                -- imp_ces_vejez,	          
                   0.0,                -- imp_act_ces_vejez,       
                   0.0,                -- imp_tot_plusv_retiro,    
                   0.0,                -- imp_tot_minus_retiro,    
                   0.0,                -- imp_comisiones_afore ,   
                   0.0,                -- imp_total_apo_patronal,  
                   0.0,                -- imp_plusv_apo_patronal,  
                   0,                  -- total_regs_solicitud,    
                   0,                  -- total_regs_aceptados,    
                   0,                  -- total_regs_rechazados,   
                   0,                  -- total_regs_dev_parcial,  
                   0,                  -- total_regs_dev_pendiente,
                   0.0,                -- num_aplicaciones_inter,  
                   " "                 -- filler
              FROM safre_tmp:tmp_dpe_sum_pago_patronal
             WHERE per_pago = v_r_dpe_sol_trabajador.v_per_pago
               AND folio_sua = v_r_dpe_sol_trabajador.v_folio_sua
              
            FOREACH cur_dpe_sum_patron 
               INTO v_r_dpe_sum_patron.v_num_reg_pat_imss        ,
                    v_r_dpe_sum_patron.v_rfc_patron              ,
                    v_r_dpe_sum_patron.v_per_pago                ,
                    v_r_dpe_sum_patron.v_folio_sua               ,
                    v_r_dpe_sum_patron.v_imp_ret_devolver        ,
                    v_r_dpe_sum_patron.v_imp_act_recargos        ,
                    v_r_dpe_sum_patron.v_imp_ces_vejez           ,
                    v_r_dpe_sum_patron.v_imp_act_ces_vejez       ,
                    v_r_dpe_sum_patron.v_imp_tot_plusv_retiro    ,
                    v_r_dpe_sum_patron.v_imp_tot_minus_retiro    ,
                    v_r_dpe_sum_patron.v_imp_comisiones_afore    ,
                    v_r_dpe_sum_patron.v_imp_total_apo_patronal  ,
                    v_r_dpe_sum_patron.v_imp_plusv_apo_patronal  ,
                    v_r_dpe_sum_patron.v_total_regs_solicitud    ,
                    v_r_dpe_sum_patron.v_total_regs_aceptados    ,
                    v_r_dpe_sum_patron.v_total_regs_rechazados   ,
                    v_r_dpe_sum_patron.v_total_regs_dev_parcial  ,
                    v_r_dpe_sum_patron.v_total_regs_dev_pendiente,
                    v_r_dpe_sum_patron.v_num_aplicaciones_inter  ,
                    v_r_dpe_sum_patron.v_filler                  

                 
               SELECT count(*)
               INTO   v_r_dpe_sum_patron.v_total_regs_solicitud
               FROM   dpe_sol_trab_complementario
               WHERE  num_reg_pat_imss = v_r_dpe_patron.num_reg_pat_imss
               AND    per_pago         = v_r_dpe_patron.per_pago
               AND    folio_sua        = v_r_dpe_patron.folio_sua

               SELECT count(*)                                  
               INTO   v_r_dpe_sum_patron.v_total_regs_aceptados
               FROM   dpe_sol_trab_complementario
               WHERE  num_reg_pat_imss = v_r_dpe_patron.num_reg_pat_imss
               AND    per_pago         = v_r_dpe_patron.per_pago
               AND    folio_sua        = v_r_dpe_patron.folio_sua
               AND    resul_operacion_compl = 1
               AND    diagnostico = 0      
               
               SELECT count(*)                                  
               INTO   v_r_dpe_sum_patron.v_total_regs_rechazados 
               FROM   dpe_sol_trab_complementario
               WHERE  num_reg_pat_imss = v_r_dpe_patron.num_reg_pat_imss
               AND    per_pago         = v_r_dpe_patron.per_pago
               AND    folio_sua        = v_r_dpe_patron.folio_sua
               AND    resul_operacion_compl = 2

               SELECT count(*)                                  
               INTO   v_r_dpe_sum_patron.v_total_regs_dev_parcial
               FROM   dpe_sol_trab_complementario
               WHERE  num_reg_pat_imss = v_r_dpe_patron.num_reg_pat_imss
               AND    per_pago         = v_r_dpe_patron.per_pago
               AND    folio_sua        = v_r_dpe_patron.folio_sua
               AND    resul_operacion_compl = 1
               AND    diagnostico = 1             

               SELECT SUM(num_aplicaciones_inter)
               INTO   v_r_dpe_sum_patron.v_num_aplicaciones_inter
               FROM   dpe_sol_trab_complementario
               WHERE  num_reg_pat_imss = v_r_dpe_patron.num_reg_pat_imss
               AND    per_pago         = v_r_dpe_patron.per_pago
               AND    folio_sua        = v_r_dpe_patron.folio_sua

               LET v_r_dpe_sum_patron.v_c_tpo_registro = "04"

               LET v_s_registro = v_r_dpe_sum_patron.v_c_tpo_registro,
                                  v_r_dpe_sum_patron.v_num_reg_pat_imss,
                                  v_r_dpe_sum_patron.v_rfc_patron,
                                  v_r_dpe_sum_patron.v_per_pago,
                                  v_r_dpe_sum_patron.v_folio_sua USING "&&&&&&",
                                  v_r_dpe_sum_patron.v_imp_ret_devolver * 100 USING "&&&&&&&&&&&",
                                  v_r_dpe_sum_patron.v_imp_act_recargos * 100 USING "&&&&&&&&&&&",
                                  v_r_dpe_sum_patron.v_imp_ces_vejez * 100 USING "&&&&&&&&&&&",
                                  v_r_dpe_sum_patron.v_imp_act_ces_vejez * 100 USING "&&&&&&&&&&&",
                                  v_r_dpe_sum_patron.v_imp_tot_plusv_retiro * 100 USING "&&&&&&&&&&&",
                                  v_r_dpe_sum_patron.v_imp_tot_minus_retiro * 100 USING "&&&&&&&&&&&",
                                  v_r_dpe_sum_patron.v_imp_comisiones_afore * 100 USING "&&&&&&&&&&&",
                                  v_r_dpe_sum_patron.v_imp_total_apo_patronal * 100 USING "&&&&&&&&&&&",
                                  v_r_dpe_sum_patron.v_imp_plusv_apo_patronal * 100 USING "&&&&&&&&&&&",
                                  v_r_dpe_sum_patron.v_total_regs_solicitud   USING "&&&&&&&",
                                  v_r_dpe_sum_patron.v_total_regs_aceptados   USING "&&&&&&&",
                                  v_r_dpe_sum_patron.v_total_regs_rechazados  USING "&&&&&&&",
                                  v_r_dpe_sum_patron.v_total_regs_dev_parcial USING "&&&&&&&",
                                  v_r_dpe_sum_patron.v_total_regs_dev_pendiente USING "&&&&&&&",
                                  v_r_dpe_sum_patron.v_num_aplicaciones_inter * 1000000 USING "&&&&&&&&&&&&&&&&&&" ,
                                  v_r_dpe_sum_patron.v_filler

               CALL v_ch_arch_solTransf.write([v_s_registro])

               LET v_s_tot_registros = v_s_tot_registros + 1  

            END FOREACH -- Sumario patron
         
            LET v_s_tot_solicitudes = v_s_tot_solicitudes + 1
                 
         END FOREACH -- Encabezado patronal

      LET v_tot_exceso = 0
      
      -- Se llena el sumario de la solicitud en exceso
      DECLARE cur_dpe_sum_exceso CURSOR FOR 
      SELECT fec_envio_original,	  
             0,-- tot_solicitudes_lote,	
             0,-- tot_registros_lote,    
             0,-- tot_regs_aceptados,    
             0,-- tot_regs_rechazados,	  
             0,-- tot_regs_dev_parcial,  
             0,-- tot_regs_dev_pendiente,
             0.0,-- imp_liquidar_retiro,   
             0.0,-- imp_liquidar_plusv_rcv,
             0.0,-- imp_liquidar_minus_rcv,
             0.0,-- imp_comisiones_rcv,    
             0.0,-- imp_liquidar_vivienda, 
             0.0,-- imp_liquidar_plusv_viv,
             0.0,-- num_aplicaciones_inter,
             " " -- filler
        FROM safre_tmp:tmp_dpe_sum_notificacion
        
      FOREACH cur_dpe_sum_exceso 
         INTO v_r_dpe_sum_exceso.v_fec_envio_original    ,
              v_r_dpe_sum_exceso.v_tot_solicitudes_lote  ,
              v_r_dpe_sum_exceso.v_tot_registros_lote    ,
              v_r_dpe_sum_exceso.v_tot_regs_aceptados    ,
              v_r_dpe_sum_exceso.v_tot_regs_rechazados   ,
              v_r_dpe_sum_exceso.v_tot_regs_dev_parcial  ,
              v_r_dpe_sum_exceso.v_tot_regs_dev_pendiente,
              v_r_dpe_sum_exceso.v_imp_liquidar_retiro   ,
              v_r_dpe_sum_exceso.v_imp_liquidar_plusv_rcv,
              v_r_dpe_sum_exceso.v_imp_liquidar_minus_rcv,
              v_r_dpe_sum_exceso.v_imp_comisiones_rcv    ,
              v_r_dpe_sum_exceso.v_imp_liquidar_vivienda ,
              v_r_dpe_sum_exceso.v_imp_liquidar_plusv_viv,
              v_r_dpe_sum_exceso.v_num_aplicaciones_inter,
              v_r_dpe_sum_exceso.v_filler

         LET v_r_dpe_sum_exceso.v_tipo_reg     = "09"
         LET v_r_dpe_sum_exceso.v_id_servicio  = "03"
         LET v_r_dpe_sum_exceso.v_id_operacion = "57"

         LET v_r_dpe_sum_exceso.v_tot_registros_lote = v_s_tot_registros

         SELECT COUNT(*)
         INTO   v_r_dpe_sum_exceso.v_tot_solicitudes_lote
         FROM   dpe_sol_trab_complementario
         WHERE  folio_integra = g_folio_sol_trabajador

         SELECT count(*)                                  
         INTO   v_r_dpe_sum_exceso.v_tot_regs_aceptados
         FROM   dpe_sol_trab_complementario                        
         WHERE  folio_integra = g_folio_sol_trabajador
         AND    resul_operacion_compl = 1
         AND    diagnostico = 0      

         SELECT count(*)                                  
         INTO   v_r_dpe_sum_exceso.v_tot_regs_rechazados
         FROM   dpe_sol_trab_complementario                        
         WHERE  folio_integra = g_folio_sol_trabajador
         AND    resul_operacion_compl = 2

         SELECT count(*)                                  
         INTO   v_r_dpe_sum_exceso.v_tot_regs_dev_parcial
         FROM   dpe_sol_trab_complementario                        
         WHERE  folio_integra = g_folio_sol_trabajador
         AND    resul_operacion_compl = 1
         AND    diagnostico = 1             

         SELECT SUM(num_aplicaciones_inter)
         INTO   v_r_dpe_sum_exceso.v_num_aplicaciones_inter
         FROM   dpe_sol_trab_complementario
         WHERE  num_reg_pat_imss = v_r_dpe_patron.num_reg_pat_imss
         AND    per_pago         = v_r_dpe_patron.per_pago
         AND    folio_sua        = v_r_dpe_patron.folio_sua

         LET v_s_registro = v_r_dpe_sum_exceso.v_tipo_reg,
                            v_r_dpe_sum_exceso.v_id_servicio,
                            v_r_dpe_sum_exceso.v_id_operacion,
                            v_r_dpe_sum_exceso.v_fec_envio_original ,--USING "yyyymmdd",
                            v_r_dpe_sum_exceso.v_tot_solicitudes_lote USING "&&&&&&&" ,
                            v_r_dpe_sum_exceso.v_tot_registros_lote USING "&&&&&&&" ,
                            v_r_dpe_sum_exceso.v_tot_regs_aceptados USING "&&&&&&&" ,
                            v_r_dpe_sum_exceso.v_tot_regs_rechazados USING "&&&&&&&" ,
                            v_r_dpe_sum_exceso.v_tot_regs_dev_parcial USING "&&&&&&&" ,
                            v_r_dpe_sum_exceso.v_tot_regs_dev_pendiente USING "&&&&&&&",
                            v_r_dpe_sum_exceso.v_imp_liquidar_retiro * 100 USING "&&&&&&&&&&&&&&&" ,
                            v_r_dpe_sum_exceso.v_imp_liquidar_plusv_rcv * 100 USING "&&&&&&&&&&&&&&&" ,
                            v_r_dpe_sum_exceso.v_imp_liquidar_minus_rcv * 100 USING "&&&&&&&&&&&&&&&" ,
                            v_r_dpe_sum_exceso.v_imp_comisiones_rcv * 100 USING "&&&&&&&&&&&&&&&" ,
                            v_r_dpe_sum_exceso.v_imp_liquidar_vivienda * 100 USING "&&&&&&&&&&&&&&&" ,
                            v_r_dpe_sum_exceso.v_imp_liquidar_plusv_viv * 100 USING "&&&&&&&&&&&&&&&" ,
                            v_r_dpe_sum_exceso.v_num_aplicaciones_inter * 1000000 USING "&&&&&&&&&&&&&&&&&&" ,
                            v_r_dpe_sum_exceso.v_filler

         CALL v_ch_arch_solTransf.write([v_s_registro])
         LET v_tot_exceso = v_tot_exceso + 1

         LET v_tot_sumario = 0
         
         LET v_r_dpe_sum_notificacion.v_tipo_reg 	            = "10" 
         LET v_r_dpe_sum_notificacion.v_id_servicio             = "03" 
         LET v_r_dpe_sum_notificacion.v_id_operacion            = "57" 
         LET v_r_dpe_sum_notificacion.v_tipo_ent_origen  	    = "03" 
         LET v_r_dpe_sum_notificacion.v_cve_ent_origen          = "001"
         LET v_r_dpe_sum_notificacion.v_tipo_ent_destino        = "04" 
         LET v_r_dpe_sum_notificacion.v_cve_ent_destino 	    = "002"
         
         LET v_r_dpe_sum_notificacion.v_fec_transferencia       = TODAY
         LET v_r_dpe_sum_notificacion.v_consecutivo_dia         = 1
         LET v_r_dpe_sum_notificacion.v_total_lotes             = 1
         LET v_r_dpe_sum_notificacion.v_imp_liquidar_retiro     = 0.0
         LET v_r_dpe_sum_notificacion.v_imp_liquidar_plusv_rcv  = 0.0
         LET v_r_dpe_sum_notificacion.v_imp_minusvalia_rcv      = 0.0
         LET v_r_dpe_sum_notificacion.v_imp_comisiones_rcv      = 0.0
         LET v_r_dpe_sum_notificacion.v_imp_liquidar_vivienda   = 0.0
         LET v_r_dpe_sum_notificacion.v_imp_liquidar_plusv_viv  = 0.0
         LET v_r_dpe_sum_notificacion.v_num_aplicaciones_inter  = v_r_dpe_sum_exceso.v_num_aplicaciones_inter
         LET v_r_dpe_sum_notificacion.v_filler                  = " "               

                LET v_s_registro = v_r_dpe_sum_notificacion.v_tipo_reg,
                                   v_r_dpe_sum_notificacion.v_id_servicio,
                                   v_r_dpe_sum_notificacion.v_id_operacion,
                                   v_r_dpe_sum_notificacion.v_tipo_ent_origen,
                                   v_r_dpe_sum_notificacion.v_cve_ent_origen,
                                   v_r_dpe_sum_notificacion.v_tipo_ent_destino,
                                   v_r_dpe_sum_notificacion.v_cve_ent_destino,
                                   v_r_dpe_sum_notificacion.v_fec_transferencia USING "yyyymmdd",
                                   v_r_dpe_sum_notificacion.v_consecutivo_dia USING "&&&",
                                   v_r_dpe_sum_notificacion.v_total_lotes USING "&&&&&&&",
                                   v_r_dpe_sum_notificacion.v_imp_liquidar_retiro    * 100 USING "&&&&&&&&&&&&&&&",
                                   v_r_dpe_sum_notificacion.v_imp_liquidar_plusv_rcv * 100 USING "&&&&&&&&&&&&&&&",
                                   v_r_dpe_sum_notificacion.v_imp_minusvalia_rcv     * 100 USING "&&&&&&&&&&&&&&&",
                                   v_r_dpe_sum_notificacion.v_imp_comisiones_rcv     * 100 USING "&&&&&&&&&&&&&&&",
                                   v_r_dpe_sum_notificacion.v_imp_liquidar_vivienda  * 100 USING "&&&&&&&&&&&&&&&",
                                   v_r_dpe_sum_notificacion.v_imp_liquidar_plusv_viv * 100 USING "&&&&&&&&&&&&&&&",
                                   v_r_dpe_sum_notificacion.v_num_aplicaciones_inter * 1000000 USING "&&&&&&&&&&&&&&&&&&",
                                   v_r_dpe_sum_notificacion.v_filler

                CALL v_ch_arch_solTransf.write([v_s_registro])
                LET v_tot_sumario = v_tot_sumario + 1 
      END FOREACH -- Sumario lote solicitud                                              
   END FOREACH --Encabezado lote   

   IF cont_cza_solicitud = 1 THEN
      DISPLAY "No existe información para generar el archivo"
      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
                  RETURNING r_bnd_fin_oper
      LET p_mensaje = " --- ERROR ---\n",
                      " El proceso de generación de archivo no terminó correctamente.\n",
                      " Código de error : ", r_bnd_fin_oper,"\n ",
                      " FECHA           : ",TODAY,"\n",
                      " HORA            : ",CURRENT HOUR TO SECOND,"\n"
   ELSE
      DISPLAY "# # # # # # # # # # # # # # # # # # # # # # # # # #"
      DISPLAY "#    "
      DISPLAY "#   El archivo se creo satisfactoriamente"
      DISPLAY "#    "
      DISPLAY "#   Ruta y nombre del archivo: ",v_v_ruta_nomarch
      DISPLAY "#    "
      DISPLAY "#  Total de patrones    : "
      DISPLAY "#  Total de solicitudes : ",v_s_tot_registros
      DISPLAY "#  Total de aceptadas   : ",v_s_tot_trabajadores_aceptadas
      DISPLAY "#  Total de rechazadas  : ",v_s_tot_trabajadores_rechazadas
      DISPLAY "#    "
      DISPLAY "# # # # # # # # # # # # # # # # # # # # # # # # # #"
      
      LET p_mensaje = "# # # # # # # # # # # # # # # # # # # # # # # # # # \n",
                      "#   \n ",
                      "#   El archivo se creo satisfactoriamente \n",
                      "#  \n",
                      "#   Ruta y nombre del archivo: ",v_v_ruta_nomarch,"\n",
                      "#   \n",
                      "#  Total de solicitudes : "||v_s_tot_registros,"\n",
                      "#  Total de aceptadas   : "||v_s_tot_trabajadores_aceptadas,"\n",
                      "#  Total de rechazadas  : "||v_s_tot_trabajadores_rechazadas,"\n",
                      "#  \n",
                      "# # # # # # # # # # # # # # # # # # # # # # # # # #"
   END IF
  
   CALL v_ch_arch_solTransf.close()

   --Convierte archivo a DOS
   LET v_v_nom_archivo = v_busca_archivo||v_reg_dia CLIPPED||".dpe"
   LET v_convierte_archivo = "unix2dos "||" "||v_c_ruta_env_dpe CLIPPED||" "||v_v_nom_archivo
   RUN v_convierte_archivo 

   DISPLAY "Convierte archivo a DOS:", v_convierte_archivo

   
   LET p_titulo = "Finalización de proceso - " || v_proceso_desc CLIPPED || " - GENERA ARCHIVO"
   
   CALL fn_correo_proceso(g_pid,g_proceso_cod,g_opera_cod,
                          "/ds/safreviv/ret/bin/correo.txt", -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)
   
   
END FUNCTION --fn_archivo_salida_INFONAVIT

#Objetivo: genera el número consecutivo por día para el archivo de salida
FUNCTION fn_crea_nombre_archivo(p_ruta_envio,p_busca_nom_archivo)
DEFINE p_ruta_envio     LIKE seg_modulo.ruta_envio,
       p_busca_nom_archivo  VARCHAR(40),
       v_cmd                STRING,
       v_consecutivo        INTEGER
DEFINE fn CHAR(18)
DEFINE ch base.Channel

    --DISPLAY "p_busca_nom_archivo: ", p_busca_nom_archivo CLIPPED
    LET v_cmd = "ls -lrt ",p_ruta_envio CLIPPED,"/ | grep -i '",p_busca_nom_archivo CLIPPED,"' |awk '{print $9}'"
    --DISPLAY "v_cmd", v_cmd CLIPPED

    LET ch = base.Channel.create()
    CALL ch.setDelimiter(".")
    CALL ch.openPipe(v_cmd,"r")
    WHILE ch.read([fn])
       LET v_consecutivo = fn[18,18]
    END WHILE
    CALL ch.close()
    LET v_consecutivo = v_consecutivo + 1

    IF length(v_consecutivo) = 0 THEN
       LET v_consecutivo = 1
    END IF

    RETURN v_consecutivo

END FUNCTION