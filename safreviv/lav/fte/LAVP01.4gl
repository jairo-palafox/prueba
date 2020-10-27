################################################################################
#Modulo       => LAV                                                           #
#Programa     => LAVP01                                                        #
#Objetivo     => Lanzado integración de archivo precio dólar                   #
#Fecha inicio => 05/12/2014                                                    #
################################################################################
--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 
-- 08/12/2014
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
DEFINE v_QryTxt           STRING,
       v_proceso_desc     LIKE cat_proceso.proceso_desc,
       v_extension        LIKE cat_operacion.extension,
       v_opera_desc       LIKE cat_operacion.opera_desc,
       v_layout           LIKE cat_operacion.layout_cod,
       v_ruta_rescate     LIKE seg_modulo.ruta_rescate,
       v_ruta_listados    LIKE seg_modulo.ruta_listados,
       v_usuario          LIKE seg_modulo.usuario,
       v_resultado        INTEGER, 
       v_isam_error       INTEGER, 
       v_msj_sql          CHAR(200), 
       v_total_registros  INTEGER,
       v_total_aceptados  INTEGER,
       v_total_rechazados INTEGER,
       v_titulo           STRING,
       v_mensaje          STRING,
       r_bnd_error_opera  SMALLINT,
       r_bnd_opera_fin    SMALLINT

   LET g_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET v_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)

   -- se crear el archivo log
   CALL STARTLOG(g_usuario_cod CLIPPED|| ".LAVP01.log")

   CALL fn_recupera_inf_proceso(g_proceso_cod, g_opera_cod) 
        RETURNING v_proceso_desc,
                  v_extension, 
                  v_opera_desc,
                  v_layout, 
                  v_ruta_rescate,
                  v_ruta_listados,
                  v_usuario

   SELECT MAX(pid)
   INTO   g_pid
   FROM   bat_ctr_proceso
   WHERE  proceso_cod = g_proceso_cod

   CALL fn_genera_folio (g_proceso_cod,g_opera_cod, g_usuario_cod)
        RETURNING v_folio

   LET v_QryTxt = "EXECUTE FUNCTION fn_lav_valida_carga_precio_dolar(?,?,?,?,?) "

   -- se prepara la ejecucion del stored procedure para la integracion
   PREPARE sid_valida_dolar FROM v_QryTxt
   EXECUTE sid_valida_dolar USING g_usuario_cod, 
                                  g_pid, 
                                  g_proceso_cod,
                                  g_opera_cod,
                                  v_folio
                             INTO v_resultado, 
                                  v_isam_error,
                                  v_msj_sql, 
                                  v_total_registros,
                                  v_total_aceptados,
                                  v_total_rechazados

   CASE
      WHEN ( v_resultado = 0 )
         DISPLAY "ERROR   : ", v_resultado
         DISPLAY "ISAM    : ", v_isam_error
         DISPLAY "Mensaje : ", v_msj_sql

         IF v_total_rechazados = 0 THEN
            LET v_mensaje = " \n Folio : ", v_folio,
                            " \n Total de registros : ", v_total_registros, " \n",
                            " Total de correctos : ", v_total_aceptados, " \n",
                            " \n La validación del archivo terminó correctamente"

         ELSE
            LET v_mensaje = " \n Folio : ", v_folio,
                            " \n Total de registros : ", v_total_registros, " \n",
                            " Total de correctos : ", v_total_aceptados, " \n",
                            " Total de rechazados: ", v_total_rechazados, " \n",
                            " \n La validación terminó correctamente con registros rechazados \n"

            CALL fn_consulta_rechazos(v_folio)
         END IF

         CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
              RETURNING r_bnd_opera_fin
      WHEN (v_resultado = NOTFOUND )
         LET v_mensaje = " Error en la validación de datos",
                         " \n ERROR   : ", v_resultado,
                         " \n ISAM    : ", v_isam_error,
                         " \n Mensaje : ", v_msj_sql,
                         " \n Folio   : ", v_folio

         CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
              RETURNING r_bnd_error_opera

         UPDATE glo_ctr_archivo
         SET    estado = -1
         WHERE  nombre_archivo = p_nombre_archivo

         UPDATE glo_folio
         SET    STATUS = -1
         WHERE  folio  = v_folio
         
      WHEN ( v_resultado < 0 )
         LET v_mensaje = " Error en la validación de datos",
                         " \n ERROR   : ", v_resultado,
                         " \n ISAM    : ", v_isam_error,
                         " \n Mensaje : ", v_msj_sql,
                         " \n Folio   : ", v_folio
         
         CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
              RETURNING r_bnd_error_opera       

         CALL fn_consulta_rechazos(v_folio)
              
         DELETE
         FROM   glo_ctr_archivo
         WHERE  nombre_archivo = p_nombre_archivo

         UPDATE glo_folio
         SET    STATUS = -1
         WHERE  folio  = v_folio
         
   END CASE
   
   LET v_titulo = "Finalización de proceso - " || v_proceso_desc CLIPPED || " - VALIDACIÓN DE DATOS"

   CALL fn_correo_proceso(g_pid,
                          g_proceso_cod,
                          g_opera_cod,
                          "",
                          v_titulo,
                          v_mensaje)
END MAIN

FUNCTION fn_consulta_rechazos(p_folio)
DEFINE p_folio DECIMAL(9,0)
DEFINE arr_rechazos RECORD  
       campo_valor CHAR(30),
       desc_rechazo CHAR(25)
END RECORD

   DECLARE cur_display_rch CURSOR FOR SELECT campo_valor, desc_rechazo
                                      FROM   lav_rch_precio_dolar
                                      WHERE  folio = v_folio

   DISPLAY " \n --------- Detalle de registros rechazados --------- \n"

   FOREACH cur_display_rch  INTO arr_rechazos.*
      DISPLAY arr_rechazos.campo_valor || " - " || arr_rechazos.desc_rechazo
   END FOREACH      

END FUNCTION 