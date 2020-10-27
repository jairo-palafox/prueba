--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 18/04/2012
--===============================================================

####################################################################
#Modulo            =>DPE                                           #
#Programa          =>DPES01                                        #
#Objetivo          =>Programa que ejecuta el proceso de generación #
#                    de archivo                                    #
#Fecha inicio => Febrero 28, 2012                                  #
####################################################################
GLOBALS "DPEG01.4gl"
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_usuario_cod LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       c_pago_total_nss SMALLINT, -- = 0               
       c_pago_parcial_nss SMALLINT, -- = 1             
       c_pago_por_preliquidar_total SMALLINT, -- = 2   
       c_pago_por_preliquidar_parcial SMALLINT, -- = 3 
       c_pago_preliquidado_total SMALLINT, -- = 4      
       c_pago_preliquidado_parcial SMALLINT, -- = 5    
       c_pago_liquidado_total SMALLINT, -- = 6         
       c_pago_liquidado_parcial SMALLINT, -- = 7       
       c_pago_enviado_procedente_total SMALLINT, -- = 8  
       c_pago_enviado_procedente_parcial SMALLINT, -- = 9
       c_pago_enviado_procesar_total SMALLINT, -- = 100
       c_pago_enviado_procesar_parcial SMALLINT -- = 11
END GLOBALS
MAIN
DEFINE p_pid            LIKE bat_ctr_operacion.pid, -- PID del proceso
       p_proceso_cod    LIKE bat_ctr_operacion.proceso_cod, -- codigo del proceso
       p_opera_cod      LIKE bat_ctr_operacion.opera_cod, -- codigo de la operacion
       p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_folio          LIKE deo_preliquida.folio_liquida, -- folio de la operacion
       p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo, -- nombre dle archivo
       r_bnd_fin_oper SMALLINT,
       -- 
       p_d_inicial, -- Fecha inicial del periodo para el archivo de salida 
       p_d_final    DATE, -- Fecha final del periodo para el archivo de salida
       p_cad_folios VARCHAR(1000) -- Cadena que contiene los folios a consultar
       
   -- se recuperan los parametros que envia el programa lanzador
   -- usuario, pid, proceso_cod, opera_cod, folio, nombre_archivo
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   LET p_d_inicial      = ARG_VAL(7)
   LET p_d_final        = ARG_VAL(8)
   LET p_cad_folios     = ARG_VAL(9)
   
   -- se asigna proceso y operacion
   LET g_pid         = p_pid      
   LET g_proceso_cod = p_proceso_cod -- devolucion 
   LET g_opera_cod   = p_opera_cod   -- genera archivo procesar
   LET g_usuario_cod = p_usuario_cod -- Usuario firmado  

   -- se obtiene el PID del proceso
   SELECT MAX(pid)
     INTO g_pid
     FROM bat_ctr_proceso
    WHERE proceso_cod = g_proceso_cod
    
      -- Llamado a función que genera el archivo de salida
      CALL fn_archivo_salida(p_usuario_cod, p_d_inicial, p_d_final, p_cad_folios)

      -- se invoca la finalizacion de la operacion
      CALL fn_actualiza_opera_fin(g_pid,    --- Identificador del proceso
           g_proceso_cod, --- Clave del proceso
           g_opera_cod) --- Clave de la operación
         RETURNING r_bnd_fin_oper
         
      IF r_bnd_fin_oper <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         DISPLAY "ERROR en fn_actualiza_opera_fin"
      END IF

END MAIN

{
======================================================================
Clave: 
Nombre: fn_archivo_salida
Fecha creacion: Marzo 16, 2012
Autor: Jose Soto
Narrativa del proceso que realiza:
Genera el archivo de salida para PROCESAR
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_archivo_salida(p_usuario_cod, v_d_inicial, v_d_final, v_cad_folios)
DEFINE p_usuario_cod         LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       v_r_dpe_cza_solicitud RECORD --Record almacena encabezado de solicitud
          v_d_folio               DECIMAL(9,0),
          v_d_id_dpe_referencia   DECIMAL(9,0),
          v_d_f_transferencia     DATE,
          v_s_num_consecutivo     SMALLINT,
          v_c_modalidad_archivo   CHAR(2),
          v_d_tpo_registro        CHAR(2),
          v_c_ide_servicio        CHAR(2),
          v_c_ide_operacion       CHAR(2),
          v_c_tpo_entidad_origen  CHAR(2),
          v_c_cve_entidad_origen  CHAR(3),
          v_c_tpo_entidad_destino CHAR(2),
          v_c_cve_entidad_destino CHAR(3),
          v_c_resultado_operacion SMALLINT,
          v_c_diagnostico_1       SMALLINT,
          v_c_diagnostico_2       SMALLINT,
          v_c_diagnostico_3       SMALLINT,
          v_c_filler              CHAR(255)
       END RECORD,
       v_r_dpe_patron RECORD --Record almacena encabezado del patrón
             v_d_folio              DECIMAL(9,0),
             v_d_id_dpe_referencia  DECIMAL(9,0),
             v_c_reg_patronal_imss  CHAR(11),
             v_c_rfc_patron         CHAR(13),
             v_c_periodo_pago       CHAR(6),
             v_i_folio_sua          INTEGER,
             v_c_razon_social       CHAR(50),
             v_c_numero_solicitud   CHAR(13),
             v_s_tipo_cotizacion    SMALLINT,
             v_i_tot_dias_cotizados INTEGER,
             v_i_tot_tra_solicitud  INTEGER,
             v_d_f_pago             DATE,
             v_d_f_valor_viv        DATE,
             v_d_f_valor_rcv        DATE,
             v_c_clave_entidad_rec  CHAR(3),
             v_c_delegacion         CHAR(2),
             v_c_subdelegacion      CHAR(2),
             v_c_result_op          CHAR(2),
             v_i_sec_registro_lote  INTEGER,
             v_d_tpo_registro       CHAR(2),
             v_c_diagnostico_1      SMALLINT,
             v_c_diagnostico_2      SMALLINT,
             v_c_diagnostico_3      SMALLINT,
             v_c_filler             CHAR(128)
          END RECORD,
          v_r_dpe_sol_trabajador RECORD --Record almacena detalle del trabajador
             v_d_id_dpe_referencia  DECIMAL(9,0),
             v_d_folio              DECIMAL(9,0),
             v_c_reg_patronal_imss  CHAR(11),
             v_d_id_derechohabiente DECIMAL(9,0),
             v_c_nss_trabajador     CHAR(11),
             v_c_periodo_pago       CHAR(6),
             v_c_rfc                CHAR(13),
             v_c_curp               CHAR(18),
             v_c_nombre             CHAR(50),
             v_s_dias_cotizados     SMALLINT,
             v_d_imp_retiro_dev     DECIMAL(16,6), -- Importe 9
             v_d_imp_act_retiro_dev DECIMAL(16,6), -- Importe 10
             v_d_imp_cv_pat_dev     DECIMAL(16,6), -- Importe 11
             v_d_imp_cv_trab_dev    DECIMAL(16,6), -- Importe 12
             v_d_imp_act_cv_dev     DECIMAL(16,6), -- Importe 13
             v_d_imp_viv_dev        DECIMAL(16,6), -- Importe 14
             v_d_avis_viv_dev       DECIMAL(16,6), -- Importe 15
             v_d_porcentaje_dev     DECIMAL(5,2),
             v_s_estado_solicitud   SMALLINT,
             v_s_diagnostico        SMALLINT,
             v_s_resul_op           SMALLINT,
             v_s_diag_procesa       SMALLINT,
             v_s_folio_respuesta    SMALLINT,
             v_d_tpo_registro       CHAR(2),
             v_c_filler             CHAR(113)
          END RECORD,
          v_r_dpe_sum_patron RECORD --Record almacena sumario del patrón
             v_d_folio             DECIMAL(9,0),
             v_d_id_dpe_referencia DECIMAL(9,0),
             v_c_reg_patronal_imss CHAR(11),
             v_c_rfc               CHAR(13),
             v_c_periodo_pago      CHAR(6),
             v_c_folio_sua         CHAR(6),
             v_d_tpo_registro      CHAR(2),
             v_c_filler            CHAR(195)
          END RECORD,
          v_r_dpe_sum_subtot_patron RECORD -- Record almacena subtotales del sumario patron
             v_d_subtot_imp_retiro_dev     DECIMAL(16,6), -- Suma detalle trabajador imp 9
             v_d_subtot_imp_act_retiro_dev DECIMAL(16,6), -- Suma detalle trabajador imp 11 o 12
             v_d_subtot_imp_cv_pat_dev     DECIMAL(16,6),
             v_d_subtot_imp_cv_trab_dev    DECIMAL(16,6),     
             v_d_subtot_imp_act_cv_dev     DECIMAL(16,6), -- Suma detalle trabajador imp 10 y 13
             v_d_imp_viv_dev               DECIMAL(16,6), -- Suma detalle trabajador imp 14
             v_d_avis_viv_dev              DECIMAL(16,6) -- Suma detalle trabajador imp 15
          END RECORD,
          v_r_dpe_sum_exceso RECORD --Record almacena sumario de la solicitud
             v_d_folio                DECIMAL(9,0),
             v_c_tipo_ent_origen      CHAR(2),
             v_c_cve_ent_origen       CHAR(3),
             v_c_tipo_ent_destino     CHAR(2),
             v_c_cve_ent_destino      CHAR(3),
             v_d_f_transferencia      DATE,
             v_s_consecutivo_dia      SMALLINT,
             v_d_tot_sol_patronales   DECIMAL(9,0),
             v_d_tot_reg_trabajadores DECIMAL(9,0),
             v_d_tot_registros        DECIMAL(9,0),
             v_d_total_retiro_cv      DECIMAL(22,6),
             v_d_total_vivienda       DECIMAL(22,6),
             v_d_total_avis           DECIMAL(22,6),
             v_d_suma_total_avis      DECIMAL(24,6),
             v_d_tpo_registro         CHAR(2),
             v_c_filler               CHAR(199)
          END RECORD,

       v_v_nom_archivo     CHAR(40), -- nombre del archivo de salida
       v_v_ruta_nomarch    VARCHAR(100), -- ruta y nombre del archivo de salida
       v_c_ruta_env_dpe    LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
       v_ch_arch_solTransf BASE.CHANNEL, -- manejador de apuntador hacia archivo
       v_d_inicial, -- Fecha inicial del periodo para el archivo de salida 
       v_d_final                DATE, -- Fecha final del periodo para el archivo de salida 
       v_cad_folios             VARCHAR(1000), -- Cadena que contiene los folios a consultar
       v_sql_cadena             VARCHAR(1000),-- Cadena que contiene los folios a consultar
       v_s_sql                  VARCHAR(1000),-- Cadena que contiene las consultas a ejecutar
       cont_cza_solicitud       SMALLINT, -- Contador de encabezado de solicitudes
       v_s_tot_solicitudes      SMALLINT, -- Total de patrones solo registros 02
       v_s_tot_trabajadores     SMALLINT, -- Total de trabajadores solo registros 03
       v_s_tot_trabajadores_aux SMALLINT,-- Total de trabajadores solo registros 03
       v_s_tot_registros        SMALLINT, -- Total de registros solo registros <> 01 y 09
       v_s_registro             STRING, -- registro a insertar
       v_d_hoy                  CHAR(8),
       v_busca_archivo          STRING,
       v_cont_dia               SMALLINT, -- consecutivo por dia de archivo generado
       v_reg_dia                SMALLINT, -- Parametro consecutivo de registro por dia
       v_convierte_archivo      STRING,
       v_QryTxt                 STRING,
       v_s_total_01     INTEGER, --Total de encabezado lote     
       v_s_total_02     INTEGER, --Total encabezados patrón
       v_s_total_03     INTEGER, --Total trabajadores
       v_s_total_04     INTEGER, --Total sumarios patrón
       v_s_total_09     INTEGER, --Total de sumario lote     
       tot_regs_archivo INTEGER, --Total registros por archivo
       v_QryInsert      STRING

   -- Se asignan valores costantes a las viables de totales o parciales
   ------valida si son pagos totales o parciales
   LET c_pago_total_nss                  = 0
   LET c_pago_parcial_nss                = 1
   LET c_pago_por_preliquidar_total      = 2
   LET c_pago_por_preliquidar_parcial    = 3
   LET c_pago_preliquidado_total         = 4
   LET c_pago_preliquidado_parcial       = 5
   --------- Valida solo aceptados
   LET c_pago_liquidado_total            = 6
   LET c_pago_liquidado_parcial          = 7
   --------Actualizar a estos estados
   LET c_pago_enviado_procedente_total   = 8
   LET c_pago_enviado_procedente_parcial = 9
   -----------Actualiza a estados a procesar
   LET c_pago_enviado_procesar_total     = 100
   LET c_pago_enviado_procesar_parcial   = 11

   LET tot_regs_archivo = 0
   --
   LET v_cont_dia = 1

   -- se crear el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".DPES01.log")

    -- se obtienen la ruta envio del modulo
    SELECT ruta_envio 
      INTO v_c_ruta_env_dpe
      FROM seg_modulo
     WHERE modulo_cod = 'dpe'
     
   -- Se verifica la longitud de la cadena que contiene los folios disponibles
   IF LENGTH(v_cad_folios CLIPPED) = 0 THEN
   	  LET v_sql_cadena = "\n AND 1 = 1"
   ELSE
   	  LET v_sql_cadena = "\n AND a.folio IN (",v_cad_folios,")"
   END IF

   --DISPLAY "FOLIOS ENTRADA: ", v_sql_cadena

   -- se crea el nombre del archivo y posteriormente se concatena con la ruta
   LET v_d_hoy  = TODAY USING "DDMMYYYY"
   LET v_busca_archivo = "SOLPRC"||v_d_hoy
   --Obtine consecutivo para archivo por día
   CALL fn_crea_nombre_archivo(v_c_ruta_env_dpe,v_busca_archivo)
        RETURNING v_cont_dia

   LET v_reg_dia = v_cont_dia USING "&"
   LET v_v_nom_archivo = v_busca_archivo||v_reg_dia||".dpe"
   LET v_v_ruta_nomarch = v_c_ruta_env_dpe CLIPPED || "/"||v_v_nom_archivo
   -- se crea el manejador de archivo
   LET v_ch_arch_solTransf = base.Channel.create()

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_solTransf.openFile(v_v_ruta_nomarch, "w" )
   CALL v_ch_arch_solTransf.setDelimiter("")

   -- Se ingresan los registros del encabezado de la solicitud
   LET cont_cza_solicitud = 1

   -- Se llena el encabezado de la solicitud
   LET v_s_sql = "\n  SELECT a.folio, ",
                 "\n         a.id_dpe_referencia, ",
                 "\n         a.modalidad_archivo",
                 "\n  FROM   safre_viv:dpe_cza_solicitud a, ",
                 "\n         safre_viv:glo_folio b",
                 "\n  WHERE  b.f_actualiza BETWEEN ","'",v_d_inicial,"'",
                 "\n  AND    ","'", v_d_final,"'",
                 "\n  AND    b.proceso_cod = ", g_proceso_cod_dpe_disposicion,
                 "\n  AND    a.folio = b.folio",
                 v_sql_cadena

   --DISPLAY "CONSULTA ENCABEZADO: ", v_s_sql CLIPPED
   
   PREPARE Prpr_cza_solicitud FROM v_s_sql CLIPPED
   DECLARE cur_cza_solicitud CURSOR FOR Prpr_cza_solicitud

   --Inicia contadores total sumario
   LET v_s_total_01 = 0
   LET v_s_total_02 = 0
   LET v_s_total_03 = 0
   LET v_s_total_04 = 0
   LET v_s_total_09 = 0


      --Total de Registros Lote // Suma encabezado
      --LET tot_regs_archivo = tot_regs_archivo + 1
      
      -- Asigna valores en codigo duro según especificación
      LET v_r_dpe_cza_solicitud.v_d_tpo_registro = "01"
      LET v_r_dpe_cza_solicitud.v_c_ide_servicio = "03"
      LET v_r_dpe_cza_solicitud.v_c_ide_operacion = "55"
      LET v_r_dpe_cza_solicitud.v_c_tpo_entidad_origen = "04"
      LET v_r_dpe_cza_solicitud.v_c_cve_entidad_origen = "002"
      LET v_r_dpe_cza_solicitud.v_c_tpo_entidad_destino = "03"
      LET v_r_dpe_cza_solicitud.v_c_cve_entidad_destino = "001"
      LET v_r_dpe_cza_solicitud.v_d_f_transferencia = TODAY
      LET v_r_dpe_cza_solicitud.v_s_num_consecutivo = v_reg_dia
      LET v_r_dpe_cza_solicitud.v_c_modalidad_archivo = "02"

      -- Se llena la cadena para escribir en el archivo
      LET v_s_registro = v_r_dpe_cza_solicitud.v_d_tpo_registro USING "&&",
                         v_r_dpe_cza_solicitud.v_c_ide_servicio,
                         v_r_dpe_cza_solicitud.v_c_ide_operacion,
                         v_r_dpe_cza_solicitud.v_c_tpo_entidad_origen,
                         v_r_dpe_cza_solicitud.v_c_cve_entidad_origen,
                         v_r_dpe_cza_solicitud.v_c_tpo_entidad_destino,
                         v_r_dpe_cza_solicitud.v_c_cve_entidad_destino,
                         v_r_dpe_cza_solicitud.v_d_f_transferencia USING "YYYYMMDD",
                         v_r_dpe_cza_solicitud.v_s_num_consecutivo USING "&&&",
                         v_r_dpe_cza_solicitud.v_c_modalidad_archivo USING "&&",
                         v_r_dpe_cza_solicitud.v_c_resultado_operacion USING "&&",
                         v_r_dpe_cza_solicitud.v_c_diagnostico_1 USING "&&&",
                         v_r_dpe_cza_solicitud.v_c_diagnostico_2 USING "&&&",
                         v_r_dpe_cza_solicitud.v_c_diagnostico_3 USING "&&&",
                         v_r_dpe_cza_solicitud.v_c_filler
      
      CALL v_ch_arch_solTransf.write([v_s_registro])
   
   -- Comienza el conteo de solicitudes de patrones
   FOREACH cur_cza_solicitud 
      INTO v_r_dpe_cza_solicitud.v_d_folio,
           v_r_dpe_cza_solicitud.v_d_id_dpe_referencia,
           v_r_dpe_cza_solicitud.v_c_modalidad_archivo

      LET v_s_total_01 = v_s_total_01 + 1     
      LET cont_cza_solicitud = cont_cza_solicitud + 1     

      --Empieza encabezado de patrones
      LET v_s_tot_solicitudes = 0 -- Se inicializa el contador de patrones
      
      -- Se llena el encabezado del patrón
      DECLARE cur_dpe_patron CURSOR FOR 
         SELECT *
           FROM dpe_patron
          WHERE folio = v_r_dpe_cza_solicitud.v_d_folio
      
      LET v_s_tot_registros = 0

      -- Comienza el conteo de encabezado de patrones
      FOREACH cur_dpe_patron 
         INTO v_r_dpe_patron.v_d_folio,
              v_r_dpe_patron.v_d_id_dpe_referencia,
              v_r_dpe_patron.v_c_reg_patronal_imss,
              v_r_dpe_patron.v_c_rfc_patron,
              v_r_dpe_patron.v_c_periodo_pago,
              v_r_dpe_patron.v_i_folio_sua,
              v_r_dpe_patron.v_c_razon_social,
              v_r_dpe_patron.v_c_numero_solicitud,
              v_r_dpe_patron.v_s_tipo_cotizacion,
              v_r_dpe_patron.v_i_tot_dias_cotizados,
              v_r_dpe_patron.v_i_tot_tra_solicitud,
              v_r_dpe_patron.v_d_f_pago,
              v_r_dpe_patron.v_d_f_valor_viv,
              v_r_dpe_patron.v_d_f_valor_rcv,
              v_r_dpe_patron.v_c_clave_entidad_rec,
              v_r_dpe_patron.v_c_delegacion,
              v_r_dpe_patron.v_c_subdelegacion,
              v_r_dpe_patron.v_c_result_op,
              v_r_dpe_patron.v_i_sec_registro_lote
              
         LET v_r_dpe_patron.v_d_tpo_registro = "02"

         SELECT COUNT(*)
         INTO v_r_dpe_patron.v_i_tot_tra_solicitud
         FROM dpe_sol_trabajador
         WHERE id_dpe_patron = v_r_dpe_patron.v_d_id_dpe_referencia
        
         LET v_s_registro = v_r_dpe_patron.v_d_tpo_registro,
                            v_r_dpe_patron.v_c_reg_patronal_imss,
                            v_r_dpe_patron.v_c_rfc_patron,
                            v_r_dpe_patron.v_c_periodo_pago,
                            v_r_dpe_patron.v_i_folio_sua  USING "&&&&&&",
                            v_r_dpe_patron.v_c_razon_social,
                            v_r_dpe_patron.v_c_numero_solicitud,
                            v_r_dpe_patron.v_s_tipo_cotizacion USING "&",
                            v_r_dpe_patron.v_i_tot_dias_cotizados USING "&&&&&&&",
                            v_r_dpe_patron.v_i_tot_tra_solicitud USING "&&&&&&&",
                            v_r_dpe_patron.v_d_f_pago USING "YYYYMMDD",
                            v_r_dpe_patron.v_d_f_valor_viv USING "YYYYMMDD",
                            v_r_dpe_patron.v_d_f_valor_rcv USING "YYYYMMDD",
                            v_r_dpe_patron.v_c_clave_entidad_rec USING "&&&",
                            v_r_dpe_patron.v_c_delegacion USING "&&",
                            v_r_dpe_patron.v_c_subdelegacion USING "&&",
                            v_r_dpe_patron.v_c_result_op USING "&&",
                            v_r_dpe_patron.v_c_diagnostico_1 USING "&&&",
                            v_r_dpe_patron.v_c_diagnostico_2 USING "&&&",
                            v_r_dpe_patron.v_c_diagnostico_3 USING "&&&",
                            v_r_dpe_patron.v_i_sec_registro_lote USING "&&&&&&&&&",
                            v_r_dpe_patron.v_c_filler
         
            CALL v_ch_arch_solTransf.write([v_s_registro])

            LET v_s_total_02 = v_s_total_02  + 1
            LET tot_regs_archivo = tot_regs_archivo + 1
            
            LET v_s_tot_registros = v_s_tot_registros + 1
         
         -- Se llena el detalle del trabajador
         DECLARE cur_dpe_sol_trabajador CURSOR FOR 
            SELECT id_dpe_referencia ,
                   folio             ,
                   reg_patronal_imss ,
                   id_derechohabiente,
                   nss               ,
                   periodo_pago      ,
                   rfc               ,
                   curp              ,
                   nombre            ,
                   dias_cotizados    ,
                   imp_retiro_dev    ,
                   imp_act_retiro_dev,
                   imp_cv_pat_dev    ,
                   imp_cv_trab_dev   ,
                   imp_act_cv_dev    ,
                   imp_viv_dev       ,
                   avis_viv_dev      ,
                   porcentaje_dev    ,
                   estado_solicitud  ,
                   diagnostico       ,
                   resul_op          ,
                   diag_procesa      ,
                   folio_respuesta
              FROM dpe_sol_trabajador
             WHERE folio = v_r_dpe_cza_solicitud.v_d_folio
               AND reg_patronal_imss = v_r_dpe_patron.v_c_reg_patronal_imss
               AND periodo_pago = v_r_dpe_patron.v_c_periodo_pago
               AND id_dpe_patron = v_r_dpe_patron.v_d_id_dpe_referencia
         
         -- Inicializa a cero las variables para la sumatoria parcial por patrón
         LET v_r_dpe_sum_subtot_patron.v_d_subtot_imp_retiro_dev = 0
         LET v_r_dpe_sum_subtot_patron.v_d_subtot_imp_act_retiro_dev = 0
         LET v_r_dpe_sum_subtot_patron.v_d_subtot_imp_cv_pat_dev = 0
         LET v_r_dpe_sum_subtot_patron.v_d_subtot_imp_cv_trab_dev = 0
         LET v_r_dpe_sum_subtot_patron.v_d_subtot_imp_act_cv_dev = 0
         LET v_r_dpe_sum_subtot_patron.v_d_imp_viv_dev = 0
         LET v_r_dpe_sum_subtot_patron.v_d_avis_viv_dev = 0
         
         LET v_s_tot_trabajadores = 0
         
         FOREACH cur_dpe_sol_trabajador 
            INTO v_r_dpe_sol_trabajador.v_d_id_dpe_referencia,
                 v_r_dpe_sol_trabajador.v_d_folio,
                 v_r_dpe_sol_trabajador.v_c_reg_patronal_imss,
                 v_r_dpe_sol_trabajador.v_d_id_derechohabiente,
                 v_r_dpe_sol_trabajador.v_c_nss_trabajador,
                 v_r_dpe_sol_trabajador.v_c_periodo_pago,
                 v_r_dpe_sol_trabajador.v_c_rfc,
                 v_r_dpe_sol_trabajador.v_c_curp,
                 v_r_dpe_sol_trabajador.v_c_nombre,
                 v_r_dpe_sol_trabajador.v_s_dias_cotizados,
                 v_r_dpe_sol_trabajador.v_d_imp_retiro_dev, -- Importe 9 detalle trabajador
                 v_r_dpe_sol_trabajador.v_d_imp_act_retiro_dev, -- Importe 10 detalle trabajador
                 v_r_dpe_sol_trabajador.v_d_imp_cv_pat_dev, -- Importe 11 detalle trabajador
                 v_r_dpe_sol_trabajador.v_d_imp_cv_trab_dev, -- Importe 12 detalle trabajador
                 v_r_dpe_sol_trabajador.v_d_imp_act_cv_dev, -- Importe 13 detalle trabajador
                 v_r_dpe_sol_trabajador.v_d_imp_viv_dev, -- Importe 14 detalle trabajador
                 v_r_dpe_sol_trabajador.v_d_avis_viv_dev, -- Importe 15 detalle trabajador
                 v_r_dpe_sol_trabajador.v_d_porcentaje_dev,
                 v_r_dpe_sol_trabajador.v_s_estado_solicitud,
                 v_r_dpe_sol_trabajador.v_s_diagnostico,
                 v_r_dpe_sol_trabajador.v_s_resul_op,
                 v_r_dpe_sol_trabajador.v_s_diag_procesa,
                 v_r_dpe_sol_trabajador.v_s_folio_respuesta
            
               -- Sum Importe total retiro CV sumario solicitud
               LET v_r_dpe_sum_subtot_patron.v_d_imp_viv_dev = 
                   v_r_dpe_sum_subtot_patron.v_d_imp_viv_dev +
                   v_r_dpe_sol_trabajador.v_d_imp_retiro_dev +
                   v_r_dpe_sol_trabajador.v_d_imp_act_retiro_dev +
                   v_r_dpe_sol_trabajador.v_d_imp_cv_pat_dev +
                   v_r_dpe_sol_trabajador.v_d_imp_cv_trab_dev +
                   v_r_dpe_sol_trabajador.v_d_imp_act_cv_dev
                   
               
               LET v_r_dpe_sum_subtot_patron.v_d_avis_viv_dev =
                   v_r_dpe_sum_subtot_patron.v_d_avis_viv_dev +
                   v_r_dpe_sol_trabajador.v_d_avis_viv_dev    
                
               -- Suma Importe retiro Sumario patronal
               LET v_r_dpe_sum_subtot_patron.v_d_subtot_imp_retiro_dev = 
                   v_r_dpe_sum_subtot_patron.v_d_subtot_imp_retiro_dev + 
                   v_r_dpe_sol_trabajador.v_d_imp_retiro_dev
               
                -- Suma act y rec CV Sumario patronal
               LET v_r_dpe_sum_subtot_patron.v_d_subtot_imp_act_retiro_dev =
                   v_r_dpe_sum_subtot_patron.v_d_subtot_imp_act_retiro_dev + 
                   v_r_dpe_sol_trabajador.v_d_imp_act_retiro_dev + 
                   v_r_dpe_sol_trabajador.v_d_imp_act_cv_dev
               
               -- Suma Importe Censatia y vejez Sumario patronal
               LET v_r_dpe_sum_subtot_patron.v_d_subtot_imp_cv_pat_dev = 
                   v_r_dpe_sum_subtot_patron.v_d_subtot_imp_cv_pat_dev + 
                   v_r_dpe_sol_trabajador.v_d_imp_cv_pat_dev
                   
               ---- Suma Importe aportacion patronal info a devolver Sumario patronal
               LET v_r_dpe_sum_subtot_patron.v_d_subtot_imp_cv_trab_dev = 
                   v_r_dpe_sum_subtot_patron.v_d_subtot_imp_cv_trab_dev + 
                   v_r_dpe_sol_trabajador.v_d_imp_viv_dev
               
               ---- Suma Importe apli intereses de vivienda a devolver Sumario patronal
               LET v_r_dpe_sum_subtot_patron.v_d_subtot_imp_act_cv_dev = 
                   v_r_dpe_sum_subtot_patron.v_d_subtot_imp_act_cv_dev + 
                   v_r_dpe_sol_trabajador.v_d_avis_viv_dev
               
               -- Actualiza los registros totales a 8 enviados a procesar
               UPDATE safre_viv:dpe_sol_trab_parcial
               SET    diagnostico = c_pago_enviado_procesar_total
               WHERE  folio = v_r_dpe_sol_trabajador.v_d_folio
               AND    reg_patronal_imss = v_r_dpe_sol_trabajador.v_c_reg_patronal_imss
               --AND id_dpe_referencia = v_r_dpe_sol_trabajador.v_d_id_dpe_referencia
               AND    periodo_pago = v_r_dpe_sol_trabajador.v_c_periodo_pago
               AND    diagnostico = c_pago_enviado_procedente_total

               -- Actualiza los registros totales a 9 enviados a procesar
               UPDATE safre_viv:dpe_sol_trab_parcial
               SET diagnostico = c_pago_enviado_procesar_parcial
               WHERE folio = v_r_dpe_sol_trabajador.v_d_folio
               AND reg_patronal_imss = v_r_dpe_sol_trabajador.v_c_reg_patronal_imss
               --AND id_dpe_referencia = v_r_dpe_sol_trabajador.v_d_id_dpe_referencia
               AND periodo_pago = v_r_dpe_sol_trabajador.v_c_periodo_pago
               AND diagnostico = c_pago_enviado_procedente_parcial

            LET v_r_dpe_sol_trabajador.v_d_tpo_registro = "03"           

            LET v_r_dpe_sol_trabajador.v_d_imp_viv_dev = 0
            
            LET v_s_registro = v_r_dpe_sol_trabajador.v_d_tpo_registro,
                               v_r_dpe_sol_trabajador.v_c_reg_patronal_imss,
                               v_r_dpe_sol_trabajador.v_c_periodo_pago,
                               v_r_dpe_sol_trabajador.v_c_nss_trabajador,
                               v_r_dpe_sol_trabajador.v_c_rfc,
                               v_r_dpe_sol_trabajador.v_c_curp,
                               v_r_dpe_sol_trabajador.v_c_nombre,
                               v_r_dpe_sol_trabajador.v_s_dias_cotizados  USING "&&",
                               v_r_dpe_sol_trabajador.v_d_imp_retiro_dev * 100 USING "&&&&&&&&&",
                               v_r_dpe_sol_trabajador.v_d_imp_act_retiro_dev * 100 USING "&&&&&&&&&",
                               v_r_dpe_sol_trabajador.v_d_imp_cv_pat_dev * 100 USING "&&&&&&&&&",
                               v_r_dpe_sol_trabajador.v_d_imp_cv_trab_dev * 100 USING "&&&&&&&&&",
                               v_r_dpe_sol_trabajador.v_d_imp_act_cv_dev * 100 USING "&&&&&&&&&",
                               v_r_dpe_sol_trabajador.v_d_imp_viv_dev * 100 USING "&&&&&&&&&",
                               v_r_dpe_sol_trabajador.v_d_avis_viv_dev * 1000000 USING "&&&&&&&&&&&&&&&",
                               v_r_dpe_sol_trabajador.v_c_filler
                               
            LET v_s_tot_trabajadores = v_s_tot_trabajadores + 1
            LET v_s_tot_registros = v_s_tot_registros + 1

            LET v_s_total_03 = v_s_total_03  + 1
            LET tot_regs_archivo = tot_regs_archivo + 1
            
            CALL v_ch_arch_solTransf.write([v_s_registro])

         END FOREACH -- Detalle trabajador
         
         LET v_s_tot_trabajadores_aux = v_s_tot_trabajadores_aux + v_s_tot_trabajadores
         
            -- Se llena el sumario del patron
            DECLARE cur_dpe_sum_patron CURSOR FOR 
               SELECT folio,
                      id_dpe_referencia,                
                      reg_patronal_imss,    
                      rfc,                  
                      periodo_pago,         
                      folio_sua             
                 FROM dpe_sum_patron
                WHERE folio = v_r_dpe_cza_solicitud.v_d_folio
                  AND reg_patronal_imss = v_r_dpe_sol_trabajador.v_c_reg_patronal_imss
                  AND periodo_pago = v_r_dpe_sol_trabajador.v_c_periodo_pago
                  AND folio_sua = v_r_dpe_patron.v_i_folio_sua --Se agrega folio SUA para evitar duplicidad de sumario de patrones
                 
            FOREACH cur_dpe_sum_patron 
               INTO v_r_dpe_sum_patron.v_d_folio,
                    v_r_dpe_sum_patron.v_d_id_dpe_referencia,
                    v_r_dpe_sum_patron.v_c_reg_patronal_imss,
                    v_r_dpe_sum_patron.v_c_rfc,
                    v_r_dpe_sum_patron.v_c_periodo_pago,
                    v_r_dpe_sum_patron.v_c_folio_sua
                    
               LET v_r_dpe_sum_patron.v_d_tpo_registro = "04"
               LET v_r_dpe_sum_subtot_patron.v_d_subtot_imp_cv_trab_dev = 0
               
               LET v_s_registro = v_r_dpe_sum_patron.v_d_tpo_registro,
                                  v_r_dpe_sum_patron.v_c_reg_patronal_imss,
                                  v_r_dpe_sum_patron.v_c_rfc,
                                  v_r_dpe_sum_patron.v_c_periodo_pago,
                                  v_r_dpe_sum_patron.v_c_folio_sua USING "&&&&&&",
                                  v_r_dpe_sum_subtot_patron.v_d_subtot_imp_retiro_dev * 100 USING "&&&&&&&&&&&", -- Sum 9 detalle trabajador
                                  v_r_dpe_sum_subtot_patron.v_d_subtot_imp_cv_pat_dev * 100 USING "&&&&&&&&&&&", -- Sum 11 o 12 detalle trabajador
                                  v_r_dpe_sum_subtot_patron.v_d_subtot_imp_act_retiro_dev * 100 USING "&&&&&&&&&&&", -- Sum 10 y 13 detalle trabajador
                                  v_r_dpe_sum_subtot_patron.v_d_subtot_imp_cv_trab_dev * 100 USING "&&&&&&&&&&&", -- Sum 14 detalle trabajador
                                  v_r_dpe_sum_subtot_patron.v_d_subtot_imp_act_cv_dev * 1000000 USING "&&&&&&&&&&&&&&&&&&", -- Sum 15 detalle trabajador
                                  v_r_dpe_sum_patron.v_c_filler
                                  
               CALL v_ch_arch_solTransf.write([v_s_registro])

               LET v_s_total_04 = v_s_total_04  + 1
               LET tot_regs_archivo = tot_regs_archivo + 1
            END FOREACH -- Sumario patron

         LET v_s_tot_solicitudes = v_s_tot_solicitudes + 1
         LET v_s_tot_registros = v_s_tot_registros + 1

      END FOREACH -- Encabezado patronal
   END FOREACH  --Termina foreach de encabezado de lote

        -- Se llena el sumario de la solicitud en exceso
        DECLARE cur_dpe_sum_exceso CURSOR FOR 
           SELECT folio,
                  tipo_ent_origen,
                  cve_ent_origen,
                  tipo_ent_destino,
                  cve_ent_destino,
                  f_transferencia,
                  consecutivo_dia,
                  tot_sol_patronales,
                  tot_reg_trabajadores,
                  tot_registros,
                  total_retiro_cv,
                  total_vivienda
             FROM dpe_sum_exceso
             WHERE folio = v_r_dpe_cza_solicitud.v_d_folio
             
        FOREACH cur_dpe_sum_exceso 
           INTO v_r_dpe_sum_exceso.v_d_folio,
                v_r_dpe_sum_exceso.v_c_tipo_ent_origen,
                v_r_dpe_sum_exceso.v_c_cve_ent_origen,
                v_r_dpe_sum_exceso.v_c_tipo_ent_destino,
                v_r_dpe_sum_exceso.v_c_cve_ent_destino,
                v_r_dpe_sum_exceso.v_d_f_transferencia,
                v_r_dpe_sum_exceso.v_s_consecutivo_dia,
                v_r_dpe_sum_exceso.v_d_tot_sol_patronales,
                v_r_dpe_sum_exceso.v_d_tot_reg_trabajadores,
                v_r_dpe_sum_exceso.v_d_tot_registros,
                v_r_dpe_sum_exceso.v_d_total_retiro_cv,
                v_r_dpe_sum_exceso.v_d_total_vivienda
              
              LET v_r_dpe_sum_exceso.v_d_tpo_registro = "09"
              LET v_r_dpe_sum_exceso.v_d_f_transferencia = TODAY

              LET v_QryTxt = "\n SELECT SUM(total_avis)* 100000000",
                             "\n FROM dpe_sum_exceso",
                             "\n WHERE folio IN ", "(", v_cad_folios, ")"
--DISPLAY "Total AIVS sumario", v_QryTxt
             PREPARE prp_tot_aivs FROM v_QryTxt 
             EXECUTE prp_tot_aivs INTO v_r_dpe_sum_exceso.v_d_suma_total_avis
        END FOREACH -- Sumario lote solicitud

              LET v_s_registro = v_r_dpe_sum_exceso.v_d_tpo_registro,
                                 v_r_dpe_cza_solicitud.v_c_ide_servicio,
                                 v_r_dpe_cza_solicitud.v_c_ide_operacion,
                                 v_r_dpe_cza_solicitud.v_c_tpo_entidad_origen,
                                 v_r_dpe_cza_solicitud.v_c_cve_entidad_origen,
                                 v_r_dpe_cza_solicitud.v_c_tpo_entidad_destino,
                                 v_r_dpe_cza_solicitud.v_c_cve_entidad_destino,
                                 v_r_dpe_sum_exceso.v_d_f_transferencia USING "YYYYMMDD",
                                 v_r_dpe_cza_solicitud.v_s_num_consecutivo USING "&&&",
                                 v_s_total_02 USING "&&&&&&&", --Total de registros 02
                                 v_s_total_03 USING "&&&&&&&", --Total de registros 03
                                 tot_regs_archivo USING "&&&&&&&", --Total de 02 + 03 + 04 
                                 v_r_dpe_sum_subtot_patron.v_d_imp_viv_dev * 100 USING "&&&&&&&&&&&&&&&", -- Sum Imp 9,10,11,12,13 Detalle Trabajador 
                                 v_r_dpe_sum_subtot_patron.v_d_subtot_imp_cv_trab_dev * 100 USING "&&&&&&&&&&&&&&&", -- Sum Imp 14 Detalle trabajador
                                 v_r_dpe_sum_exceso.v_d_suma_total_avis USING "&&&&&&&&&&&&&&&&&&",
                                 v_r_dpe_sum_exceso.v_c_filler

              CALL v_ch_arch_solTransf.write([v_s_registro])
   --Almacena los folios utilizados para generar el archivo
   --y nombre del archivo en tabla

   LET v_QryTxt = "EXECUTE PROCEDURE sp_dpe_inserta_folios_archivo(?, ?, ?, ?, ?)"
   PREPARE prp_inserta_folios FROM v_QryTxt CLIPPED
   EXECUTE prp_inserta_folios USING g_proceso_cod,
                                    g_opera_cod,
                                    v_v_nom_archivo,
                                    v_cad_folios,
                                    g_usuario_cod

   IF cont_cza_solicitud = 1 THEN
      DISPLAY "No existe información para el periodo solicitado"
   ELSE
      DISPLAY "El archivo se creo satisfactoriamente"
      DISPLAY "Dentro de la siguiente ruta: ", v_v_ruta_nomarch 

      --Convierte archivo a DOS
      LET v_v_nom_archivo = v_busca_archivo||v_reg_dia CLIPPED||".dpe"
      LET v_convierte_archivo = "unix2dos "||" "||v_c_ruta_env_dpe CLIPPED||" "||v_v_nom_archivo
      RUN v_convierte_archivo 

      DISPLAY "Convierte archivo a DOS:", v_convierte_archivo
   END IF

   CALL v_ch_arch_solTransf.close()
   
END FUNCTION --fn_archivo_salida

#Objetivo: genera el número consecutivo por día para el archivo de salida
FUNCTION fn_crea_nombre_archivo(p_ruta_envio,p_busca_nom_archivo)
DEFINE p_ruta_envio     LIKE seg_modulo.ruta_envio,
       p_busca_nom_archivo  VARCHAR(40),
       v_cmd                STRING,
       v_consecutivo        INTEGER
DEFINE fn CHAR(19)
DEFINE ch base.Channel

    LET v_cmd = "ls -lrt ",p_ruta_envio CLIPPED,"/ | grep -i '",p_busca_nom_archivo CLIPPED,"' |awk '{print $9}'"

    LET ch = base.Channel.create()
    CALL ch.setDelimiter(".")
    CALL ch.openPipe(v_cmd,"r")
    WHILE ch.read([fn])
       LET v_consecutivo = fn[15,15]
    END WHILE
    CALL ch.close()
    LET v_consecutivo = v_consecutivo + 1

    IF length(v_consecutivo) = 0 THEN
       LET v_consecutivo = 1
    END IF

    RETURN v_consecutivo
 END FUNCTION