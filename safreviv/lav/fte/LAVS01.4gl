################################################################################
#Modulo       => LAV                                                           #
#Programa     => LAVS01                                                        #
#Objetivo     => Lanzado generar archivo de notificación relevantes            #
#Fecha inicio => 30/12/2014                                                    #
################################################################################
--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 
--==============================================================================
DATABASE safre_viv

GLOBALS
DEFINE g_usuario_cod    LIKE seg_usuario.usuario_cod,        -- Usuario firmado 
       g_pid            LIKE bat_ctr_proceso.pid,            -- ID del proceso
       g_proceso_cod    LIKE cat_proceso.proceso_cod,        -- codigo del proceso
       g_opera_cod      LIKE cat_operacion.opera_cod,        -- codigo de operacion,
       v_folio          LIKE glo_folio.folio,                -- Folio de proceso
       p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo  -- Nombre Archivo Procesado
END GLOBALS

MAIN
DEFINE r_bnd_fin_oper   SMALLINT

   LET g_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET v_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)

   -- se crear el archivo log
   CALL STARTLOG(g_usuario_cod CLIPPED|| ".LAVS01.log")

   -- se obtiene el PID del proceso
   SELECT MAX(pid)
   INTO   g_pid
   FROM   bat_ctr_proceso
   WHERE  proceso_cod = g_proceso_cod
    
   -- Llamado a función que genera el archivo de salida
   CALL fn_archivo_salida(g_usuario_cod,v_folio)

   -- se invoca la finalizacion de la operacion
   CALL fn_actualiza_opera_fin(g_pid, g_proceso_cod, g_opera_cod)
        RETURNING r_bnd_fin_oper
         
   IF r_bnd_fin_oper <> 0 THEN
      DISPLAY "ERROR en fn_actualiza_opera_fin"
   END IF

END MAIN

#OBJETIVO: Generar el archivo de salida de DAE en base al folio seleccionado
FUNCTION fn_archivo_salida(p_usuario_cod, p_folio)
DEFINE p_usuario_cod       LIKE seg_usuario.usuario_cod,
       r_ruta_nombre       VARCHAR(100),
       v_ch_arch_solTransf BASE.CHANNEL,
       v_tot_registros     INTEGER,
       v_registro          STRING,
       p_folio             DECIMAL(9,0)
DEFINE arr_detalles_relevante RECORD
          v_id_lav_det_lavado  DECIMAL (9,0),
          v_folio              DECIMAL (9,0),
          v_nss                CHAR(11) ,
          v_id_tipo_reporte    DECIMAL(9,0),
          v_id_tipo_validacion DECIMAL(9,0),
          v_consecutivo        DECIMAL(9,0),
          v_periodo_reporte    CHAR(4),   
          v_tipo_operacion     SMALLINT,
          v_inst_monetario     SMALLINT,
          v_pesos              DECIMAL(16,2),
          v_id_tipo_cambio     DECIMAL (9,0),
          v_f_operacion        DATE,
          v_f_deteccion        DATE,
          v_f_registro         DATE,
          v_estado             SMALLINT
END RECORD

DEFINE v_id_lav_det_lavado  STRING,
       v_folio              STRING,
       v_nss                STRING,
       v_id_tipo_reporte    STRING,
       v_id_tipo_validacion STRING,
       v_consecutivo        STRING,
       v_periodo_reporte    STRING,
       v_tipo_operacion     STRING,
       v_inst_monetario     STRING,
       v_pesos              STRING,
       v_id_tipo_cambio     STRING,
       v_f_operacion        STRING,
       v_f_deteccion        STRING,
       v_f_registro         STRING,
       v_estado             STRING 

   CALL fn_crea_nombre_archivo()
        RETURNING r_ruta_nombre

   DISPLAY "Folio para generar archivo : ", p_folio

   -- se crea el manejador de archivo
   LET v_ch_arch_solTransf = base.Channel.create()
   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_solTransf.openFile(r_ruta_nombre, "w" )

   -- Se llena el detalle del trabajador
   DECLARE cur_det_lavado CURSOR FOR SELECT id_lav_det_lavado ,
                                            folio             ,
                                            nss               ,
                                            id_tipo_reporte   ,
                                            id_tipo_validacion,
                                            consecutivo       ,
                                            periodo_reporte   ,
                                            tipo_operacion    ,
                                            inst_monetario    ,
                                            pesos             ,
                                            id_tipo_cambio    ,
                                            f_operacion       ,
                                            f_deteccion       ,
                                            f_registro        ,
                                            estado
                                     FROM   lav_det_lavado
                                     WHERE  folio = p_folio

   LET v_tot_registros = 0

   FOREACH cur_det_lavado INTO arr_detalles_relevante.*
      LET v_id_lav_det_lavado  = arr_detalles_relevante.v_id_lav_det_lavado
      LET v_folio              = arr_detalles_relevante.v_folio
      LET v_nss                = arr_detalles_relevante.v_nss
      LET v_id_tipo_reporte    = arr_detalles_relevante.v_id_tipo_reporte
      LET v_id_tipo_validacion = arr_detalles_relevante.v_id_tipo_validacion
      LET v_consecutivo        = arr_detalles_relevante.v_consecutivo
      LET v_periodo_reporte    = arr_detalles_relevante.v_periodo_reporte
      LET v_tipo_operacion     = arr_detalles_relevante.v_tipo_operacion
      LET v_inst_monetario     = arr_detalles_relevante.v_inst_monetario
      LET v_pesos              = arr_detalles_relevante.v_pesos
      LET v_id_tipo_cambio     = arr_detalles_relevante.v_id_tipo_cambio 
      LET v_f_operacion        = arr_detalles_relevante.v_f_operacion    USING "ddmmyyyy"   
      LET v_f_deteccion        = arr_detalles_relevante.v_f_deteccion    USING "ddmmyyyy"   
      LET v_f_registro         = arr_detalles_relevante.v_f_registro     USING "ddmmyyyy"   
      LET v_estado             = arr_detalles_relevante.v_estado
      
      LET v_id_lav_det_lavado  = v_id_lav_det_lavado.trim()
      LET v_folio              = v_folio.trim()
      LET v_nss                = v_nss.trim()
      LET v_id_tipo_reporte    = v_id_tipo_reporte.trim()
      LET v_id_tipo_validacion = v_id_tipo_validacion.trim()
      LET v_consecutivo        = v_consecutivo.trim()
      LET v_periodo_reporte    = v_periodo_reporte.trim()
      LET v_tipo_operacion     = v_tipo_operacion.trim()
      LET v_inst_monetario     = v_inst_monetario.trim()
      LET v_pesos              = v_pesos.trim()
      LET v_id_tipo_cambio     = v_id_tipo_cambio.trim()
      LET v_f_operacion        = v_f_operacion.trim()
      LET v_f_deteccion        = v_f_deteccion.trim()
      LET v_f_registro         = v_f_registro.trim()
      LET v_estado             = v_estado.trim()

      LET v_registro =  v_id_lav_det_lavado,ASCII(59), 
                          v_id_lav_det_lavado,ASCII(59),
                          v_folio,ASCII(59),
                          v_nss,ASCII(59),
                          v_id_tipo_reporte,ASCII(59),
                          v_id_tipo_validacion,ASCII(59), 
                          v_consecutivo,ASCII(59),
                          v_periodo_reporte,ASCII(59),
                          v_tipo_operacion,ASCII(59),
                          v_inst_monetario,ASCII(59),
                          v_pesos,ASCII(59),
                          v_id_tipo_cambio,ASCII(59),
                          v_f_operacion,ASCII(59),
                          v_f_deteccion,ASCII(59),
                          v_f_registro,ASCII(59),
                          v_estado,ASCII(59)

      LET v_tot_registros = v_tot_registros + 1

      CALL v_ch_arch_solTransf.writeline(v_registro)
   END FOREACH 

   DISPLAY " Total de Registros  : ", v_tot_registros

   IF v_tot_registros < 1 THEN
      DISPLAY " No existe información para el folio solicitado"
   ELSE
      UPDATE lav_det_lavado 
      SET    estado = 4
      WHERE  folio = p_folio
      
      DISPLAY "\n El archivo se creo satisfactoriamente dentro de la siguiente ruta: \n   ",
              r_ruta_nombre ,"\n"
   END IF

   CALL v_ch_arch_solTransf.close()
   
END FUNCTION --fn_archivo_salida

#Objetivo: Genera el nombre del archivo conforme al layout del nombre
FUNCTION fn_crea_nombre_archivo()
DEFINE v_nombre_archivo CHAR(40),
       r_ruta_nombre    VARCHAR(100),
       v_ruta_envio     LIKE seg_modulo.ruta_envio,
       v_year           CHAR(4),
       v_year_c         CHAR(2),
       v_mes            CHAR(2)

   SELECT ruta_envio 
   INTO   v_ruta_envio
   FROM   seg_modulo
   WHERE  modulo_cod = 'lav'

   LET v_year = YEAR (TODAY)
   LET v_mes  = MONTH(TODAY)

   LET v_year_c = v_year[3,4]
   LET v_mes = v_mes USING "&&"
   -- se crea el nombre del archivo y posteriormente se concatena con la ruta
   LET v_nombre_archivo = "/332-001" || v_year_c ||v_mes ||".004"

   LET r_ruta_nombre = v_ruta_envio CLIPPED || v_nombre_archivo

   RETURN r_ruta_nombre

END FUNCTION