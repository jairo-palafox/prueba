################################################################################
#Modulo       => LAV                                                           #
#Programa     => LAVP02                                                        #
#Objetivo     => Lanzado de validación de montos relevantes                    #
#Fecha inicio => 05/12/2014                                                    #
################################################################################
--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 
--==============================================================================
DATABASE safre_viv

GLOBALS
DEFINE g_usuario_cod       LIKE seg_usuario.usuario_cod,        -- Usuario firmado 
       g_pid               LIKE bat_ctr_proceso.pid,            -- ID del proceso
       g_proceso_cod       LIKE cat_proceso.proceso_cod,        -- codigo del proceso
       g_opera_cod         LIKE cat_operacion.opera_cod,        -- codigo de operacion,
       v_folio_lote        LIKE glo_folio.folio,                -- Folio de proceso
       p_nombre_archivo    LIKE glo_ctr_archivo.nombre_archivo, -- Nombre Archivo Procesado
       p_fecha_ini         DATE,
       p_fecha_fin         DATE,
       p_periodo           CHAR(6),
       v_ruta_bin          LIKE seg_modulo.ruta_bin,
       v_ruta_rescate      LIKE seg_modulo.ruta_rescate,     
       v_ruta_listados     LIKE seg_modulo.ruta_listados,
       v_ruta_listados_bat LIKE seg_modulo.ruta_listados
END GLOBALS

MAIN
DEFINE v_folio           DECIMAL(9,0),
       v_proceso_desc    LIKE cat_proceso.proceso_desc,
       v_extension       LIKE cat_operacion.extension,
       v_opera_desc      LIKE cat_operacion.opera_desc,
       v_layout          LIKE cat_operacion.layout_cod,
       v_usuario         LIKE seg_modulo.usuario,
       v_titulo          STRING,
       v_mensaje         STRING,
       v_QryTxt          STRING,
       v_resultado       INTEGER, 
       v_isam_error      INTEGER, 
       v_msj_sql         CHAR(200),
       r_bnd_error_opera SMALLINT,
       r_bnd_opera_fin   SMALLINT,
       v_tot_regs_detectados INTEGER,
       r_bnd_ini_oper    SMALLINT,
       r_bnd_fin_oper    SMALLINT

   LET g_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET v_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   LET p_fecha_ini      = ARG_VAL(7)
   LET p_fecha_fin      = ARG_VAL(8)
     
   -- se crear el archivo log
   CALL STARTLOG(g_usuario_cod CLIPPED|| ".LAVP02.log")

      -- se obtienen las rutas de control del modulo
   SELECT ruta_bin, 
          ruta_rescate, 
          ruta_listados
   INTO   v_ruta_bin,
          v_ruta_rescate, 
          v_ruta_listados
   FROM   seg_modulo s
   WHERE  s.modulo_cod = 'lav'

   SELECT ruta_listados
   INTO   v_ruta_listados_bat
   FROM   seg_modulo
   WHERE  modulo_cod = 'bat'

   CALL fn_recupera_inf_proceso(g_proceso_cod, g_opera_cod) 
        RETURNING v_proceso_desc,
                  v_extension, 
                  v_opera_desc,
                  v_layout, 
                  v_ruta_rescate,
                  v_ruta_listados,
                  v_usuario

   IF p_fecha_ini AND p_fecha_fin IS NULL THEN
      CALL fn_calcula_periodo ()
           RETURNING p_fecha_ini, p_fecha_fin, p_periodo
   END IF
   
   CALL fn_genera_folio (g_proceso_cod,g_opera_cod, g_usuario_cod)
        RETURNING v_folio

   DISPLAY "Folio: ", v_folio
   LET v_QryTxt = "EXECUTE FUNCTION fn_lav_detecta_movimientos(?,?,?,?,?,?,?,?) "

   -- se prepara la ejecucion del stored procedure para la integracion
   PREPARE sid_valida_dolar FROM v_QryTxt
   EXECUTE sid_valida_dolar USING g_usuario_cod, 
                                  g_pid, 
                                  g_proceso_cod,
                                  g_opera_cod,
                                  v_folio,
                                  p_fecha_ini,
                                  p_fecha_fin,
                                  p_periodo
                             INTO v_resultado, 
                                  v_isam_error,
                                  v_msj_sql,
                                  v_tot_regs_detectados

   CASE
      WHEN ( v_resultado = 0 )
         DISPLAY "ERROR   : ", v_resultado
         DISPLAY "ISAM    : ", v_isam_error
         DISPLAY "Mensaje : ", v_msj_sql

         LET v_mensaje = " Total de registros : ", v_tot_regs_detectados, " \n",
                         "  \n La detección de movimientos terminó correctamente"
                         
         CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
              RETURNING r_bnd_opera_fin

         IF (v_tot_regs_detectados = 0) THEN
            UPDATE glo_folio
            SET    STATUS = -1
            WHERE  folio  = v_folio

            LET v_mensaje = " Total de registros : ", v_tot_regs_detectados, " \n",
                            "  \n No se detectó ningún movimiento"

            CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,2,0,"LAVL04","",g_usuario_cod)
                 RETURNING r_bnd_ini_oper
            CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,3)
                 RETURNING r_bnd_fin_oper
         END IF

      WHEN (v_resultado = NOTFOUND )
         DISPLAY "ERROR   : ", v_resultado
         DISPLAY "ISAM    : ", v_isam_error
         DISPLAY "Mensaje : ", v_msj_sql

         CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
              RETURNING r_bnd_error_opera
         LET v_mensaje = "Error en la detección de movimientos"

         UPDATE glo_folio
         SET    STATUS = -1
         WHERE  folio  = v_folio
         
      WHEN ( v_resultado < 0 )
         LET v_mensaje = "Error en la detección de movimientos" 
         DISPLAY "ERROR   : ", v_resultado
         DISPLAY "ISAM    : ", v_isam_error
         DISPLAY "Mensaje : ", v_msj_sql

         CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
              RETURNING r_bnd_error_opera

         UPDATE glo_folio
         SET    STATUS = -1
         WHERE  folio  = v_folio

   END CASE

   LET v_titulo = "Finalización de proceso - " || v_proceso_desc CLIPPED || " - DETECCIÓN DE MOVIMIENTOS "

   CALL fn_correo_proceso(g_pid,
                          g_proceso_cod,
                          g_opera_cod,
                          "",
                          v_titulo,
                          v_mensaje)
END MAIN

#OBJETIVO : Calcular el periodo de los registros a detectar
FUNCTION fn_calcula_periodo()
DEFINE r_fecha_ini DATE,
       r_fecha_fin DATE,
       r_periodo   CHAR(6),
       v_mes       CHAR(2),
       v_year      CHAR(4)       

   LET v_mes = MONTH(TODAY)
   LET v_year = YEAR (TODAY)   

   CASE v_mes
   
   WHEN "04"
      LET r_fecha_ini = "01/01/" || v_year
      LET r_fecha_fin = "03/31/" || v_year
   WHEN "07"
      LET r_fecha_ini = "04/01/" || v_year
      LET r_fecha_fin = "06/30/" || v_year
   WHEN "10"
      LET r_fecha_ini = "07/01/" || v_year
      LET r_fecha_fin = "09/30/" || v_year
   WHEN "01"
      LET r_fecha_ini = "10/01/" || v_year
      LET r_fecha_fin = "12/31/" || v_year
   WHEN "12"
      LET r_fecha_ini = "10/01/" || v_year
      LET r_fecha_fin = "12/31/" || v_year
   END CASE

   LET r_periodo = v_mes || v_year

   RETURN r_fecha_ini, r_fecha_fin, r_periodo
END FUNCTION