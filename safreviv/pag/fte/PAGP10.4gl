-----------------------------------------------------------------------------------------
-- Modulo        => PAG
-- Programa      => PAGP10
-- Objetivo      => Programa que registra la información del LQINFO en el historico
-- Autor         => Hugo César Ramírez García
-- Fecha inicio  => 10 de Enero de 2012
-- Requerimiento =>
-----------------------------------------------------------------------------------------
-- Modificación => Colocar en monitor reporte de reg duplicados de cza pago patronal
-- Fehca        => 28 de Junio de 2018.
-- Autor        => GERARDO ALFONSO VEGA PAREDES
-- Clave cambio => saci2018-62
-----------------------------------------------------------------------------------------

DATABASE safre_viv

GLOBALS

   DEFINE
      g_pid                      LIKE bat_ctr_proceso.pid,
      g_proceso_cod              LIKE cat_proceso.proceso_cod,
      p_usuario_cod              LIKE seg_usuario.usuario_cod,
      g_opera_cod_integracion    LIKE cat_operacion.opera_cod,
      g_opera_cod_preliquidacion LIKE cat_operacion.opera_cod,
      p_nom_archivo              LIKE glo_ctr_archivo.nombre_archivo,
      v_comando                  STRING

END GLOBALS

MAIN

   DEFINE
      v_bnd_fin_proceso SMALLINT,
      p_num_folio       DECIMAL(9,0),
      v_estatus         SMALLINT,
      p_titulo          STRING,     -- titulo del mensaje enviado en el correo
      p_mensaje         STRING  

   LET p_usuario_cod = ARG_VAL(1)
   LET g_pid         = ARG_VAL(2)
   LET g_proceso_cod = ARG_VAL(3)
   LET g_opera_cod_integracion = ARG_VAL(4) -- Paso de información a las tablas históricas
   LET p_num_folio   = ARG_VAL(5)
   LET p_nom_archivo = ARG_VAL(6)
      
   LET v_bnd_fin_proceso = 0

   --Si el folio es cero se debe de obtener el folio, sino se rtespeta el que traiga
   LET p_num_folio   = fn_genera_folio(g_proceso_cod, g_opera_cod_integracion, p_usuario_cod)
   LET p_nom_archivo = fn_recupera_arch_cargado(g_proceso_cod,1)

   --Se ejecutan los displays
   CALL fn_display_proceso(0,"INTEGRACIÓN")
                              
   #Llamada a ejecución de procedimiento almacenado
   CALL fn_guarda_historicos_pag(p_num_folio) RETURNING v_estatus

   IF v_estatus = 0 THEN

      --Se actualiza el archivo como integrado
      CALL fn_act_edo_archivo(p_nom_archivo, p_num_folio, 2, p_usuario_cod ) RETURNING v_estatus

      --Se registra el FIN DE LA OPERACION COMO EXITOSA
      CALL fn_actualiza_opera_fin(g_pid,
                                  g_proceso_cod,
                                  g_opera_cod_integracion) RETURNING v_estatus

      LET p_mensaje = "Integración realizada con éxito.\nYa se puede continuar con la Preliquidación."
   
   ELSE
      
      LET p_mensaje = "El proceso de Integración ha finalizado pero con errores.\nNo se puede continuar con el proceso de Preliquidación."
      --Si ocurrio un error se actualiza el estatus como erroneo
      CALL fn_error_opera(g_pid, g_proceso_cod, g_opera_cod_integracion)  RETURNING v_estatus
   END IF
   
   --Se ejecutan los displays
   CALL fn_display_proceso(1,"INTEGRACIÓN")

   LET p_titulo = "Finalización de operación - LQINFO - Integración"
   CALL fn_correo_proceso(g_pid, 
                          g_proceso_cod, 
                          g_opera_cod_integracion, 
                          NULL, 
                          p_titulo,
                          p_mensaje)

END MAIN

FUNCTION fn_guarda_historicos_pag(p_folio)

   DEFINE 
      p_folio              DECIMAL(9,0),
      v_sql_procedure      STRING,
      v_estatus            SMALLINT,
      v_cod_error          SMALLINT,
      v_error_isam         INTEGER,
      v_mensaje_error      VARCHAR(255),
      v_det_trabajador_nss CHAR(11),
      v_ruta_ejecutable    LIKE seg_modulo.ruta_bin,
      v_ruta_vacia         LIKE seg_modulo.ruta_listados,
      v_folio              DECIMAL(9,0),
      v_periodo_pago       CHAR(06),
      v_nrp                CHAR(11),
      v_folio_sua          CHAR(06),
      v_cve_ent_recep      CHAR(03),
      v_f_pago             DATE,
      v_cuantos            INTEGER,
      v_sql                STRING,
      v_contador           INTEGER

   --sE OBTIENEN las rutas de los ejecutables
   CALL fn_rutas("pag") RETURNING v_ruta_ejecutable, v_ruta_vacia

   LET v_estatus = 0 --El cero indica que se jecuto con exito
   LET v_sql_procedure = "EXECUTE PROCEDURE sp_registro_historicos_pag(?,?,?)"

   -- se prepara la ejecucion del stored procedure para la integracion
   PREPARE prp_historicoPag FROM v_sql_procedure
   EXECUTE prp_historicoPag USING p_folio,g_pid,g_proceso_cod
      INTO v_cod_error, v_error_isam, v_mensaje_error, v_det_trabajador_nss

   IF v_cod_error = 0 THEN
      # Ejecucion sin error

      DISPLAY v_mensaje_error

      DISPLAY v_cod_error

      --saci2018-62
      LET v_sql = "SELECT folio,",
                         "periodo_pago,",
                         "nrp,",
                         "folio_sua,",
                         "cve_ent_recep,",
                         "f_pago,",
                         "count(*) ",
                 " FROM   pag_cza_pag_patronal ",
                 " WHERE  folio = ",p_folio,
                 " GROUP  BY 1,2,3,4,5,6 ",
                 " HAVING count(*) > 1 "

      LET v_contador = 0
      PREPARE cla01 FROM v_sql
      DECLARE cur_dups CURSOR FOR cla01
      FOREACH cur_dups
      	  INTO v_folio,
               v_periodo_pago,
               v_nrp,
               v_folio_sua,
               v_cve_ent_recep,
               v_f_pago,
               v_cuantos
         LET v_contador = v_contador + 1
         IF v_contador = 1 THEN
         	  EXIT FOREACH
         END IF
      END FOREACH 

      --saci2018-62
      IF v_cuantos IS NULL OR v_cuantos = 0 THEN
         DISPLAY ""
         DISPLAY "NO SE GENERA ALARMA-REPORTE POR NO ENCONTRAR REGISTROS" 
         DISPLAY "DUPLICADOS EN ENCABEZDO PAGO PATRONAL"
         DISPLAY ""
      ELSE
         DISPLAY ""
         DISPLAY "SE GENERA ALARMA-REPORTE EN EL MONITOR DE PROCESOS POR"
         DISPLAY "REGISTROS DUSPLICADOS EN ENCABEZADO PAGO PATRONAL"
         DISPLAY ""
         LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/PAGS03.42r ", --saci2018-62
                                                p_usuario_cod," ",                 --saci2018-62
                                                g_pid," ",                         --saci2018-62
                                                g_proceso_cod," ",                 --saci2018-62
                                                g_opera_cod_integracion," ",       --saci2018-62
                                                p_folio," ",                       --saci2018-62
                                                p_nom_archivo                      --saci2018-62
      END IF

      RUN v_comando
      RETURN FALSE
   ELSE 
      LET v_estatus = 1  --El uno indca que ocurrio un error al ejecutarse
      DISPLAY "\nError de ejecución en 'sp_registro_historicos_pag' (Código): ",v_cod_error
      DISPLAY "Error en 'sp_registro_historicos_pag' (Mensaje):",v_mensaje_error,"\n"
      DISPLAY "Error en 'sp_registro_historicos_pag' (Mensaje):",v_error_isam,"\n"
      DISPLAY "NSS en curso: ", v_det_trabajador_nss
      RETURN TRUE
   END IF
   
   RETURN v_estatus

END FUNCTION