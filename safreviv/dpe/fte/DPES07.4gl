--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 11/03/2013
--===============================================================

#####################################################################
#Modulo            => DPE                                           #
#Programa          => DPES06                                        #
#Objetivo          => Programa que ejecuta el proceso de generación #
#Fecha inicio      => 11/03/2013                                    #
#####################################################################
--Lanzador: DPEL23
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
       v_ruta_listados LIKE seg_modulo.ruta_listados

DEFINE g_reg_modulo   RECORD
          ruta_exp      CHAR(40),
          ruta_rescate  CHAR(40),
          ruta_listados CHAR(40)
END RECORD

END GLOBALS

MAIN
DEFINE p_pid            LIKE bat_ctr_operacion.pid, -- PID del proceso
       p_proceso_cod    LIKE bat_ctr_operacion.proceso_cod, -- codigo del proceso
       p_opera_cod      LIKE bat_ctr_operacion.opera_cod, -- codigo de la operacion
       p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_folio          DECIMAL(9,0),
       p_estado         SMALLINT,
       p_relacion       SMALLINT

   -- se recuperan los parametros que envia el programa lanzador
   -- usuario, pid, proceso_cod, opera_cod, folio, nombre_archivo
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_estado         = ARG_VAL(6)
   LET p_relacion       = ARG_VAL(7)

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
   CALL fn_archivo_salida_saci_procesar(p_usuario_cod, p_folio, p_estado, p_relacion)
   
END MAIN

#Objetivo: Genera la estructura del archivo de salida con las diferencias 
FUNCTION fn_archivo_salida_saci_procesar(p_usuario_cod, p_folio, p_estado, p_relacion)
DEFINE p_usuario_cod  LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_folio        LIKE glo_folio.folio, -- folio para preliquidar
       p_estado       SMALLINT,
       p_relacion     SMALLINT

DEFINE v_r_encabezado RECORD 
       v_descripcion               CHAR(11), 
       v_procesar                  CHAR(9),
       v_proc_nrp                  CHAR(4),
       v_proc_nss                  CHAR(4),
       v_proc_periodo              CHAR(8),
       v_proc_folio_sua            CHAR(10),
       v_proc_aivs_devueltas       CHAR(15),
       v_proc_pesos                CHAR(6),
       v_proc_estatus              CHAR(8),
       v_proc_diagnostico          CHAR(12),
       v_saci                      CHAR(5),
       v_saci_folio_lote           CHAR(6),
       v_saci_nrp                  CHAR(4),
       v_saci_nss                  CHAR(4),
       v_saci_periodo              CHAR(8),
       v_saci_folio_sua            CHAR(10),
       v_saci_aivs_devueltas       CHAR(15),
       v_saci_pesos                CHAR(6),
       v_saci_estatus              CHAR(8),
       v_saci_clave_de_diagnostico CHAR(21),
       v_diferencias               CHAR(12),
       v_dif_aivs_devueltas        CHAR(15),
       v_dif_pesos                 CHAR(6)
END RECORD

DEFINE rec_detalles RECORD 
   v_tipo_registro    CHAR(10), --Tipo de registro ACEPTADO, RECHAZADO, PENDIENTE
   v_PROCESAR         CHAR(8),  --Etiqueta de resultados PROCESAR
   v_proc_nrp         CHAR(11), 
   v_proc_nss         CHAR(11),
   v_proc_periodo     CHAR(6),
   v_proc_folio_sua   DECIMAL(9,0),
   v_proc_aivs_dev    DECIMAL(16,6),
   v_proc_pesos       DECIMAL(16,6),
   v_proc_estatus     SMALLINT,
   v_proc_diagnostico SMALLINT,
   v_SACI             CHAR(4),  --Etiquete de resultados SACI 
   v_saci_folio_lote  DECIMAL(9,0), 
   v_saci_nrp         CHAR(11),     
   v_saci_nss         CHAR(11),     
   v_saci_periodo     CHAR(6),      
   v_saci_folio_sua   DECIMAL(9,0), 
   v_saci_aivs_dev    DECIMAL(16,6),
   v_saci_pesos       DECIMAL(16,6),
   v_saci_estatus     SMALLINT,     
   v_saci_diagnostico SMALLINT,     
   v_DIFERENCIAS      CHAR(11), 
   v_dif_aivs_dev     DECIMAL(16,6),
   v_dif_pesos        DECIMAL(16,6),
   v_id_derechohabiente DECIMAL(9,0),
   v_folio_liquida    DECIMAL(9,0)
END RECORD    

DEFINE rec_det_compl RECORD
   v_tipo_registro    CHAR(10), --Tipo de registro ACEPTADO, RECHAZADO, PENDIENTE
   v_PROCESAR         CHAR(8),  --Etiqueta de resultados PROCESAR
   v_proc_nrp         CHAR(11),
   v_proc_nss         CHAR(11),
   v_proc_periodo     CHAR(6),
   v_proc_folio_sua   DECIMAL(9,0),
   v_proc_aivs_dev    DECIMAL(16,6),
   v_proc_pesos       DECIMAL(16,6),
   v_proc_estatus     SMALLINT,
   v_proc_diagnostico SMALLINT,
   v_SACI             CHAR(8),  --Etiquete de resultados SACI
   v_saci_folio_lote  DECIMAL(9,0),
   v_saci_nrp         CHAR(11),
   v_saci_nss         CHAR(11),
   v_saci_periodo     CHAR(6),
   v_saci_folio_sua   DECIMAL(9,0),
   v_saci_aivs_dev    DECIMAL(16,6),
   v_saci_pesos       DECIMAL(16,6),
   v_saci_estatus     SMALLINT,
   v_saci_diagnostico SMALLINT,
   v_DIFERENCIAS      CHAR(11),
   v_dif_aivs_dev     DECIMAL(16,6),
   v_dif_pesos        DECIMAL(16,6)
END RECORD

DEFINE v_v_nom_archivo     CHAR(40),     -- nombre del archivo de salida
       v_v_ruta_nomarch    VARCHAR(100), -- ruta y nombre del archivo de salida
       v_c_ruta_env_dpe    LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
       v_s_tot_solicitudes INTEGER,      -- Total de patrones solo registros 02
       v_s_tot_registros   INTEGER,      -- Total de registros solo registros <> 01 y 09
       v_s_registro        STRING, 
       v_fecha_dia         CHAR(8),
       v_separador         CHAR(1),
       v_s_tot_registros_compl INTEGER,
       v_fecha_liquida     DATE,
       v_precio_accion_dia DECIMAL(16,6),
       v_ch_arch_solTransf BASE.CHANNEL, -- manejador de apuntador hacia archivo
       v_folio             DECIMAL (9,0),
       v_QryTxt            STRING,
       v_busca_archivo     STRING,
       v_cont_dia          SMALLINT, -- consecutivo por dia de archivo generado
       v_reg_dia           SMALLINT, -- Parametro consecutivo de registro por dia
       v_mensaje           STRING

   -- se crear el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".DPES07.log")

    -- se obtienen la ruta envio del modulo
   SELECT ruta_envio 
   INTO   v_c_ruta_env_dpe
   FROM   seg_modulo
   WHERE  modulo_cod = 'dpe'
     
   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
   INTO   g_reg_modulo.*
   FROM   seg_modulo s
   WHERE  s.modulo_cod = 'dpe'
DISPLAY "------------ E M P I E Z A   L A    G E N E R A C I Ó N    D E    A R C H I V O ----------"
    -- se crea el nombre del archivo y posteriormente se concatena con la ruta
   LET v_fecha_dia = TODAY USING "DDMMYYYY"
   LET v_busca_archivo = "CONCILIACION_MASIVA_"||v_fecha_dia||"_"
   
   --Obtine consecutivo para archivo por día
   CALL fn_crea_nombre_archivo(v_c_ruta_env_dpe,v_busca_archivo)
        RETURNING v_cont_dia
   
   LET v_reg_dia = v_cont_dia USING "&"

   LET v_v_nom_archivo = "/"||v_busca_archivo||v_reg_dia||".txt"
   LET v_v_ruta_nomarch = v_c_ruta_env_dpe CLIPPED||v_v_nom_archivo

   DISPLAY "ruta", v_v_ruta_nomarch 
   -- se crea el manejador de archivo
   LET v_ch_arch_solTransf = base.Channel.create()

   DISPLAY v_ch_arch_solTransf 
   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_solTransf.openFile(v_v_ruta_nomarch, "w" )
   CALL v_ch_arch_solTransf.setDelimiter(" ")

   LET v_s_registro = "1.- REGISTROS CORRESPONDIENTES AL LOTE PROCESADO"
   CALL v_ch_arch_solTransf.write([v_s_registro])
   
   -- Se ingresan los registros del encabezado
---------------------------------------------
   LET v_r_encabezado.v_descripcion               = "DESCRIPCION"
   LET v_r_encabezado.v_procesar                  = "|PROCESAR"
   LET v_r_encabezado.v_proc_nrp                  = "|NRP"
   LET v_r_encabezado.v_proc_nss                  = "|NSS"
   LET v_r_encabezado.v_proc_periodo              = "|PERIODO"
   LET v_r_encabezado.v_proc_folio_sua            = "|FOLIO_SUA"
   LET v_r_encabezado.v_proc_aivs_devueltas       = "|AIVS_DEVUELTAS"
   LET v_r_encabezado.v_proc_pesos                = "|PESOS"
   LET v_r_encabezado.v_proc_estatus              = "|ESTATUS"
   LET v_r_encabezado.v_proc_diagnostico          = "|DIAGNOSTICO"
   LET v_r_encabezado.v_saci                      = "|SACI"
   LET v_r_encabezado.v_saci_folio_lote           = "|FOLIO"
   LET v_r_encabezado.v_saci_nrp                  = "|NRP"
   LET v_r_encabezado.v_saci_nss                  = "|NSS"
   LET v_r_encabezado.v_saci_periodo              = "|PERIODO"
   LET v_r_encabezado.v_saci_folio_sua            = "|FOLIO_SUA"
   LET v_r_encabezado.v_saci_aivs_devueltas       = "|AIVS_DEVUELTAS"
   LET v_r_encabezado.v_saci_pesos                = "|PESOS"
   LET v_r_encabezado.v_saci_estatus              = "|ESTATUS"
   LET v_r_encabezado.v_saci_clave_de_diagnostico = "|CLAVE_DE_DIAGNOSTICO"
   LET v_r_encabezado.v_diferencias               = "|DIFERENCIAS"
   LET v_r_encabezado.v_dif_aivs_devueltas        = "|AIVS_DEVUELTAS"
   LET v_r_encabezado.v_dif_pesos                 = "|PESOS"
   
   LET v_s_registro = v_r_encabezado.v_descripcion,
                      v_r_encabezado.v_procesar,
                      v_r_encabezado.v_proc_nrp,
                      v_r_encabezado.v_proc_nss,
                      v_r_encabezado.v_proc_periodo,
                      v_r_encabezado.v_proc_folio_sua,
                      v_r_encabezado.v_proc_aivs_devueltas,
                      v_r_encabezado.v_proc_pesos,
                      v_r_encabezado.v_proc_estatus,
                      v_r_encabezado.v_proc_diagnostico,
                      v_r_encabezado.v_saci,
                      v_r_encabezado.v_saci_folio_lote,
                      v_r_encabezado.v_saci_nrp,
                      v_r_encabezado.v_saci_nss,
                      v_r_encabezado.v_saci_periodo,
                      v_r_encabezado.v_saci_folio_sua,
                      v_r_encabezado.v_saci_aivs_devueltas,
                      v_r_encabezado.v_saci_pesos,
                      v_r_encabezado.v_saci_estatus,
                      v_r_encabezado.v_saci_clave_de_diagnostico,
                      v_r_encabezado.v_diferencias,
                      v_r_encabezado.v_dif_aivs_devueltas,
                      v_r_encabezado.v_dif_pesos
   
   CALL v_ch_arch_solTransf.write([v_s_registro])
------------------------------------------------------------------------------------------------------------------------------------------------
   --Se consultan detalles de las solicitudes SIN RELACION para calcular las diferencias 
   LET v_s_tot_solicitudes = 0

   LET v_QryTxt = "\n SELECT 'PROCESAR', ",
                  "\n        b.reg_patronal_imss, ",
                  "\n        b.nss, ",
                  "\n        b.periodo_pago, ",
                  "\n        c.folio_sua, ",
                  "\n        b.aivs_viv_dev, ",
                  "\n        b.imp_viv_dev, ",
                  "\n        a.estado_solicitud, ",
                  "\n        a.diag_procesa, ",
                  "\n        'SACI', ",
                  "\n        d.folio, ",
                  "\n        d.reg_patronal_imss, ",
                  "\n        a.nss, ",
                  "\n        d.periodo_pago, ",
                  "\n        c.folio_sua, ",
                  "\n        d.avis_viv_dev, ",
                  "\n        d.imp_viv_dev, ",
                  "\n        a.estado_solicitud, ",
                  "\n        d.diagnostico, ",
                  "\n        'DIFERENCIAS', ",
                  "\n        d.folio_liquida ",
                  "\n FROM   dpe_sol_trabajador a, ",
                  "\n        dpe_resp_procesar b, ",
                  "\n        dpe_patron c, ",
                  "\n        dpe_sol_trab_parcial d ",
                  "\n WHERE  a.folio_respuesta = b.folio ",
                  "\n AND    a.reg_patronal_imss = b.reg_patronal_imss  ",
                  "\n AND    a.nss               = b.nss ",
                  "\n AND    a.periodo_pago      = b.periodo_pago ",
                  "\n AND    c.id_dpe_referencia = a.id_dpe_patron ",
                  "\n AND    a.id_dpe_referencia = d.id_dpe_referencia  "

      IF p_folio IS NOT NULL THEN 
         LET v_QryTxt = v_QryTxt ||"\n AND    b.folio = ",p_folio    
      END IF 

      IF p_estado IS NULL THEN 
         LET v_QryTxt = v_QryTxt || "\n AND    b.resul_op           IN (1,2,4)"
      ELSE
         CASE p_estado

         WHEN 1 --Aceptados   
            LET v_QryTxt = v_QryTxt || "\n  AND    b.resul_op           = 1"

         WHEN 2 --Rechazados
            LET v_QryTxt = v_QryTxt || "\n  AND    b.resul_op           = 2"
       
         WHEN 4 --Pendientes
            LET v_QryTxt = v_QryTxt || "\n  AND    b.resul_op           = 4"
         END CASE  
      END IF 

      LET v_QryTxt = v_QryTxt,
                     "\n GROUP BY 16,2,3,4,5,6,7,8,9,11,12,13,14,15,17,18,19,21 ",
                     "\n ORDER BY 16,2,3,4,5,6,7,8,9,11,12,13,14,15,17,18,19,21 "
--DISPLAY v_QryTxt

   LET v_s_tot_registros = 0
   PREPARE prp_detalles FROM v_QryTxt
   DECLARE cur_detalles CURSOR FOR prp_detalles
   
   FOREACH cur_detalles INTO rec_detalles.v_PROCESAR        ,
                             rec_detalles.v_proc_nrp        ,
                             rec_detalles.v_proc_nss        ,
                             rec_detalles.v_proc_periodo    ,
                             rec_detalles.v_proc_folio_sua  ,
                             rec_detalles.v_proc_aivs_dev   ,
                             rec_detalles.v_proc_pesos      ,
                             rec_detalles.v_proc_estatus    ,
                             rec_detalles.v_proc_diagnostico,
                             rec_detalles.v_SACI            ,
                             rec_detalles.v_saci_folio_lote ,
                             rec_detalles.v_saci_nrp        ,
                             rec_detalles.v_saci_nss        ,
                             rec_detalles.v_saci_periodo    ,
                             rec_detalles.v_saci_folio_sua  ,
                             rec_detalles.v_saci_aivs_dev   ,
                             rec_detalles.v_saci_pesos      ,
                             rec_detalles.v_saci_estatus    ,
                             rec_detalles.v_saci_diagnostico,
                             rec_detalles.v_DIFERENCIAS,
                             rec_detalles.v_folio_liquida

      --Se agrega descripción del resultado de la operación 
      CASE
         WHEN rec_detalles.v_saci_estatus = 0
            LET rec_detalles.v_tipo_registro = "PARCIAL"
         WHEN rec_detalles.v_saci_estatus = 1
            LET rec_detalles.v_tipo_registro = "ACEPTADO"
         WHEN rec_detalles.v_saci_estatus = 2
            LET rec_detalles.v_tipo_registro = "RECHAZADO"
         OTHERWISE
            LET rec_detalles.v_tipo_registro = "NO DISP"
      END CASE

      --Se valida si registro es RECHAZADO las AIVS de quien rechace = 0.00 
      IF rec_detalles.v_proc_estatus = 2 THEN 
         LET rec_detalles.v_proc_aivs_dev = 0.00
         LET rec_detalles.v_proc_pesos    = 0.00
      END IF  
      
      IF rec_detalles.v_saci_estatus = 2 THEN 
         LET rec_detalles.v_saci_aivs_dev = 0.00
         LET rec_detalles.v_saci_pesos    = 0.00
      END IF

      --Si no se ha liquidado los pesos = 0
      IF rec_detalles.v_folio_liquida IS NULL THEN
         LET rec_detalles.v_proc_pesos = 0
      END IF  

      --Si respuesta de procesar no tienene monto en pesos
      IF rec_detalles.v_proc_pesos = 0 THEN
         SELECT f_actualiza
         INTO   v_fecha_liquida  
         FROM   glo_folio
         WHERE  folio =  rec_detalles.v_folio_liquida
         GROUP BY 1

         --Identificar precio de Acción del día
         SELECT precio_fondo
         INTO   v_precio_accion_dia
         FROM   glo_valor_fondo
         WHERE  fondo       = 11  --Fondo 11 AIVS 
         AND    f_valuacion = v_fecha_liquida

         LET rec_detalles.v_proc_pesos = rec_detalles.v_proc_aivs_dev * v_precio_accion_dia

      END IF

      LET rec_detalles.v_dif_aivs_dev = rec_detalles.v_saci_aivs_dev - rec_detalles.v_proc_aivs_dev 
      LET rec_detalles.v_dif_pesos    = rec_detalles.v_saci_pesos    - rec_detalles.v_proc_pesos    

      LET v_separador = "|"

      LET v_folio = rec_detalles.v_saci_folio_lote      

      LET v_s_registro = rec_detalles.v_tipo_registro CLIPPED,
                         v_separador,
                         rec_detalles.v_PROCESAR,
                         v_separador, 
                         rec_detalles.v_proc_nrp,
                         v_separador,
                         rec_detalles.v_proc_nss,
                         v_separador,
                         rec_detalles.v_proc_periodo,
                         v_separador,
                         rec_detalles.v_proc_folio_sua USING "&&&&&&" ,
                         v_separador,
                         rec_detalles.v_proc_aivs_dev USING "&&&&&&&&.&&",
                         v_separador,
                         rec_detalles.v_proc_pesos USING "&&&&&&&&.&&",
                         v_separador,
                         rec_detalles.v_proc_estatus USING "&&",
                         v_separador,
                         rec_detalles.v_proc_diagnostico USING "&&&",
                         v_separador,
                         rec_detalles.v_SACI,
                         v_separador,
                         v_folio,
                         v_separador, 
                         rec_detalles.v_saci_nrp,
                         v_separador,
                         rec_detalles.v_saci_nss,
                         v_separador,
                         rec_detalles.v_saci_periodo,
                         v_separador,
                         rec_detalles.v_saci_folio_sua USING "&&&&&&",
                         v_separador,
                         rec_detalles.v_saci_aivs_dev USING "&&&&&&&&.&&",
                         v_separador,
                         rec_detalles.v_saci_pesos USING "&&&&&&&&.&&",
                         v_separador,
                         rec_detalles.v_saci_estatus USING "&&",
                         v_separador,
                         rec_detalles.v_saci_diagnostico USING "&&&",
                         v_separador,
                         rec_detalles.v_DIFERENCIAS,
                         v_separador, 
                         rec_detalles.v_dif_aivs_dev USING "&&&&&&&&.&&",
                         v_separador, 
                         rec_detalles.v_dif_pesos USING "&&&&&&&&.&&"    

      CALL v_ch_arch_solTransf.write([v_s_registro])

      LET v_s_tot_registros = v_s_tot_registros + 1
   END FOREACH
------------------------------------------------------------------------------------------------------------------------------------------------
   LET v_s_registro = "2.- REGISTROS CORRESPONDIENTES A  OTROS PROCESOS ANTERIORES"
   CALL v_ch_arch_solTransf.write([v_s_registro])

   --Se consultan detalles de las solicitudes CON RELACION para calcular las diferencias 
   LET v_s_tot_solicitudes = 0

   DECLARE cur_cons_detalles_sin_relacion CURSOR FOR 
      SELECT 'PROCESAR',          
             b.reg_patronal_imss, 
             b.nss,               
             b.periodo_pago,      
             c.folio_sua,         
             b.aivs_viv_dev,      
             b.imp_viv_dev,       
             b.resul_op,          
             a.resul_operacion,--respuesta procesar
             'SACI',
             a.folio_integra,             
             a.num_reg_pat_imss,  
             a.nss_aportacion,    
             a.per_pago,          
             a.folio_sua,         
             a.num_aplicaciones_inter, 
             a.monto_aplicado,    
             a.resul_operacion_compl, 
             a.diagnostico,         --dpe_sol_complementario
             'DIFERENCIAS'        
      FROM   dpe_sol_trab_complementario a, 
             dpe_resp_procesar b,  
             dpe_patron c          
      WHERE  a.num_reg_pat_imss = b.reg_patronal_imss 
      AND    a.nss_aportacion   = b.nss 
      AND    a.per_pago         = b.periodo_pago 
      AND    a.num_reg_pat_imss = c.reg_patronal_imss 
      AND    a.per_pago         = c.periodo_pago 
      AND    a.folio_sua        = c.folio_sua 
      GROUP BY 16,2,3,4,5,6,7,8,9,11,12,13,14,15,17,18,19
      ORDER BY 16,2,3,4,5,6,7,8,9,11,12,13,14,15,17,18,19
 
   LET v_s_tot_registros_compl = 0

   FOREACH cur_cons_detalles_sin_relacion INTO rec_det_compl.v_PROCESAR        ,
                                               rec_det_compl.v_proc_nrp        ,
                                               rec_det_compl.v_proc_nss        ,
                                               rec_det_compl.v_proc_periodo    ,
                                               rec_det_compl.v_proc_folio_sua  ,
                                               rec_det_compl.v_proc_aivs_dev   ,
                                               rec_det_compl.v_proc_pesos      ,
                                               rec_det_compl.v_proc_estatus    ,
                                               rec_det_compl.v_proc_diagnostico,
                                               rec_det_compl.v_SACI            ,
                                               rec_det_compl.v_saci_folio_lote , 
                                               rec_det_compl.v_saci_nrp        ,
                                               rec_det_compl.v_saci_nss        ,
                                               rec_det_compl.v_saci_periodo    ,
                                               rec_det_compl.v_saci_folio_sua  ,
                                               rec_det_compl.v_saci_aivs_dev   ,
                                               rec_det_compl.v_saci_pesos      ,
                                               rec_det_compl.v_saci_estatus    ,
                                               rec_det_compl.v_saci_diagnostico,
                                               rec_det_compl.v_DIFERENCIAS

      CASE
         WHEN rec_det_compl.v_saci_estatus = 0
            LET rec_det_compl.v_tipo_registro = "ACEPTADO"
         WHEN rec_det_compl.v_saci_estatus = 1
            LET rec_det_compl.v_tipo_registro = "PARCIAL"
         WHEN rec_det_compl.v_saci_estatus = 2
            LET rec_det_compl.v_tipo_registro = "RECHAZADO"
      END CASE

      LET rec_det_compl.v_dif_aivs_dev = rec_det_compl.v_saci_aivs_dev - rec_det_compl.v_proc_aivs_dev 
      LET rec_det_compl.v_dif_pesos    = rec_det_compl.v_saci_pesos    - rec_det_compl.v_proc_pesos    

      LET v_separador = "|"

      LET v_s_registro = rec_det_compl.v_tipo_registro CLIPPED,
                         v_separador,
                         rec_det_compl.v_PROCESAR,
                         v_separador, 
                         rec_det_compl.v_proc_nrp,
                         v_separador,
                         rec_det_compl.v_proc_nss,
                         v_separador,
                         rec_det_compl.v_proc_periodo,
                         v_separador,
                         rec_det_compl.v_proc_folio_sua USING "&&&&&&" ,
                         v_separador,
                         rec_det_compl.v_proc_aivs_dev USING "&&&&&&&&.&&",
                         v_separador,
                         rec_det_compl.v_proc_pesos USING "&&&&&&&&.&&",
                         v_separador,
                         rec_det_compl.v_proc_estatus USING "&&",
                         v_separador,
                         rec_det_compl.v_proc_diagnostico USING "&&&",
                         v_separador,
                         rec_det_compl.v_SACI,
                         v_separador,
                         rec_det_compl.v_saci_folio_lote,
                         v_separador,
                         rec_det_compl.v_saci_nrp,
                         v_separador,
                         rec_det_compl.v_saci_nss,
                         v_separador,
                         rec_det_compl.v_saci_periodo,
                         v_separador,
                         rec_det_compl.v_saci_folio_sua USING "&&&&&&",
                         v_separador,
                         rec_det_compl.v_saci_aivs_dev USING "&&&&&&&&.&&",
                         v_separador,
                         rec_det_compl.v_saci_pesos USING "&&&&&&&&.&&",
                         v_separador,
                         rec_det_compl.v_saci_estatus USING "&&",
                         v_separador,
                         rec_det_compl.v_saci_diagnostico USING "&&&",
                         v_separador,
                         rec_det_compl.v_DIFERENCIAS,
                         v_separador, 
                         rec_det_compl.v_dif_aivs_dev USING "&&&&&&&&.&&",
                         v_separador, 
                         rec_det_compl.v_dif_pesos USING "&&&&&&&&.&&"    

      CALL v_ch_arch_solTransf.write([v_s_registro])

      LET v_s_tot_registros_compl = v_s_tot_registros_compl + 1
   END FOREACH
   IF v_s_tot_registros < 1 THEN
      CALL fn_mensaje("Atención","No existe información para generar el reporte","info")
   ELSE
      LET v_mensaje = "Se ha generado el reporte, verificar la ruta \n \n" || v_v_ruta_nomarch
      CALL fn_mensaje("Atención",v_mensaje,"info" )
   
      DISPLAY "# # # # # # # # # # # # # # # # # # # # # # # # # #"
      DISPLAY "#  "
      DISPLAY "#  El archivo se creo satisfactoriamente"
      DISPLAY "#  "
      DISPLAY "#  Ruta y nombre del archivo: ",v_v_ruta_nomarch
      DISPLAY "#  "
      DISPLAY "#  Total de solicitudes lote actual     : ",v_s_tot_registros
      DISPLAY "#  "
      DISPLAY "#  Total de solicitudes lotes anteriores: ", v_s_tot_registros_compl
      DISPLAY "#  "
      DISPLAY "# # # # # # # # # # # # # # # # # # # # # # # # # #"
   END IF
  
   CALL v_ch_arch_solTransf.close()

   --Convierte archivo a DOS
   --LET v_v_nom_archivo = v_busca_archivo||v_reg_dia CLIPPED||".dpe"
   --LET v_convierte_archivo = "unix2dos "||" "||v_c_ruta_env_dpe CLIPPED||" "||v_v_nom_archivo
   --RUN v_convierte_archivo 
  
END FUNCTION

#Objetivo: genera el número consecutivo por día para el archivo de salida
FUNCTION fn_crea_nombre_archivo(p_ruta_envio,p_busca_nom_archivo)
DEFINE p_ruta_envio     LIKE seg_modulo.ruta_envio,
       p_busca_nom_archivo  VARCHAR(40),
       v_cmd                STRING,
       v_consecutivo        INTEGER
DEFINE fn CHAR(31)
DEFINE ch base.Channel

    --DISPLAY "p_busca_nom_archivo: ", p_busca_nom_archivo CLIPPED
    LET v_cmd = "ls -lrt ",p_ruta_envio CLIPPED,"/ | grep -i '",p_busca_nom_archivo CLIPPED,"' |awk '{print $9}'"

    DISPLAY "v_cmd", v_cmd CLIPPED

    LET ch = base.Channel.create()
    CALL ch.setDelimiter(".")
    CALL ch.openPipe(v_cmd,"r")
    WHILE ch.read([fn])
       LET v_consecutivo = fn[30,31]
    END WHILE
    CALL ch.close()
    LET v_consecutivo = v_consecutivo + 1

    IF length(v_consecutivo) = 0 THEN
       LET v_consecutivo = 1
    END IF

    RETURN v_consecutivo

END FUNCTION

