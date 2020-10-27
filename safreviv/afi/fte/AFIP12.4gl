--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => AFI                                                                    #
#Programa     => AFIP12                                                                 #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la integracion    #
#                de actualizacion de RFC de afiliados                                   #
#Fecha inicio => Junio 22, 2012                                                         #
#########################################################################################

DATABASE safre_viv

GLOBALS "AFIG01.4gl"

GLOBALS

   DEFINE g_pid                    LIKE bat_ctr_proceso.pid           -- ID del proceso
   DEFINE g_proceso_cod            LIKE cat_proceso.proceso_cod       -- codigo del proceso
   DEFINE g_opera_cod              LIKE cat_operacion.opera_cod       -- codigo de operacion

END GLOBALS

MAIN

   DEFINE p_pid                    LIKE bat_ctr_operacion.pid         -- PID del proceso
   DEFINE p_proceso_cod            LIKE bat_ctr_operacion.proceso_cod -- codigo del proceso
   DEFINE p_opera_cod              LIKE bat_ctr_operacion.opera_cod   -- codigo de la operacion
   DEFINE p_usuario_cod            LIKE seg_usuario.usuario_cod       -- clave del usuario firmado
   DEFINE p_folio                  LIKE deo_preliquida.folio_liquida
   DEFINE p_nombre_archivo         LIKE glo_ctr_archivo.nombre_archivo
   DEFINE v_s_sql                  STRING                             -- cadena con una instruccion SQL
   DEFINE v_i_resultado            INTEGER                            -- resultado del proceso
   DEFINE r_bnd_fin_oper           SMALLINT
   DEFINE v_si_correcto_integra    SMALLINT
   DEFINE p_titulo                 STRING                             -- titulo del mensaje enviado en el correo
   DEFINE p_mensaje                STRING                             -- cuerpo del mensaje enviado
   DEFINE v_error_isam             INTEGER
   DEFINE v_mensaje                VARCHAR(250)
   DEFINE v_cuenta_registros       INTEGER

   DEFINE r_registro_rechazado     RECORD -- registro rechazado para reporte
      nss11                        CHAR(11),
      nss10                        CHAR(10),
      rfc                          CHAR(13),
      cod_rechazo                  SMALLINT
   END RECORD

-- variables para el reporte de integracion
   DEFINE v_ruta_reporte           STRING -- ruta del archivo del reporte
   DEFINE v_ruta_listados          VARCHAR(40) -- ruta de los listados
   DEFINE report_handler           om.SaxDocumentHandler -- handler para el reporte en PDF
   DEFINE v_nombre_usuario         CHAR (40)
   DEFINE v_usuario_cod_temp       CHAR (20)
   DEFINE v_nombre_reporte         STRING

   -- cifras de control
   DEFINE v_regs_procesados        DECIMAL(9,0);
   DEFINE v_regs_aceptados         DECIMAL(9,0);
   DEFINE v_regs_rechazados        DECIMAL(9,0);

   -- se recuperan los parametros la clave de usuario desde parametro 
   LET p_usuario_cod    =          ARG_VAL(1)
   LET p_pid            =          ARG_VAL(2)
   LET p_proceso_cod    =          ARG_VAL(3)
   LET p_opera_cod      =          ARG_VAL(4)
   LET p_folio          =          ARG_VAL(5)
   LET p_nombre_archivo =          ARG_VAL(6)

   -- se asigna proceso y operacion
   LET g_pid         = p_pid      
   LET g_proceso_cod = p_proceso_cod -- movimientos afiliatorios
   LET g_opera_cod   = p_opera_cod -- integracion

   CALL STARTLOG(p_usuario_cod CLIPPED||".AFIP12.log")

-- se obtiene el folio
   CALL fn_genera_folio(g_proceso_cod, g_opera_cod, p_usuario_cod)
        RETURNING p_folio
        DISPLAY p_folio

   -- se asume que el proceso termina correctamente
   LET v_i_resultado         = 0
   LET v_si_correcto_integra = 0

   -- se envia la cadena que indica el inicio de etapa
   CALL fn_display_proceso(0, "INTEGRACION")

   -- =============================================================================
   -- PREVALIDACION DE CONTENIDO DE ARCHIVO
   -- se verifican registros contra sumario
   -- =============================================================================
   DISPLAY "\n Ejecutando rutina de integración de actualización de RFC de Afiliación..."
   -- se contruye el enuncionado SQL
   LET v_s_sql = "EXECUTE FUNCTION fn_afi_integracion_actualiza_rfc(?,?,?,?,?)"

   -- se prepara la ejecucion del stored procedure para la integracion
   PREPARE sid_afi_actualiza_rfc FROM v_s_sql

   -- se ejecuta el stored procedure
   EXECUTE sid_afi_actualiza_rfc USING p_usuario_cod, p_folio, p_nombre_archivo, g_pid, p_proceso_cod
      INTO v_i_resultado, v_error_isam, v_mensaje, v_regs_procesados, v_regs_aceptados, v_regs_rechazados

   -- si el resultado fue correcto
   IF ( v_i_resultado <> 0 ) THEN
      -- se complementa el mensaje
      LET p_mensaje = p_mensaje || "Ocurrió un error al realizar la integración de actualizaciones de RFC.\n"
      DISPLAY "Ocurrió un error al realizar la integración de actualizaciones de RFC.\n"
      DISPLAY "Error (SQL)     : ", v_i_resultado
      DISPLAY "Error (ISAM)    : ", v_error_isam
      DISPLAY "Error (Mensaje) : ", v_mensaje

      -- se marca la operacion en error
      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
         RETURNING v_i_resultado
   ELSE
      -- se complementa el mensaje
      LET p_mensaje = p_mensaje || "Integración de actualización de RFC realizada con éxito."
      DISPLAY "Integración de actualización de RFC realizada con éxito."
      DISPLAY "Registros procesados: ", v_regs_procesados
      DISPLAY "Registros aceptados : ", v_regs_aceptados
      DISPLAY "Registros rechazados: ", v_regs_rechazados
      DISPLAY "folio               : ", p_folio

      -- REPORTE DE CIFRAS DE CONTROL DE LA INTEGRACIÓN
      -- se obtiene el módulo del proceso
      SELECT ruta_listados
        INTO v_ruta_listados
        FROM seg_modulo
       WHERE modulo_cod = "afi"

      LET v_ruta_reporte = v_ruta_listados CLIPPED, "/",
                           p_usuario_cod   CLIPPED, "-", -- usuario
                           "AFIP12-"                   , -- programa
                           g_pid           USING "&&&&&","-", -- PID
                           g_proceso_cod   USING "&&&&&", "-", -- codigo del proceso
                           g_opera_cod     USING "&&&&&",".pdf" -- codigo de la operación

      LET v_nombre_reporte = p_usuario_cod   CLIPPED, "-", -- usuario
                            "AFIP12-"                   , -- programa
                            g_pid           USING "&&&&&","-", -- PID
                            g_proceso_cod   USING "&&&&&", "-", -- codigo del proceso
                            g_opera_cod     USING "&&&&&",".pdf" -- codigo de la operación

      DISPLAY "Ruta del reporte   : ", v_ruta_reporte
      DISPLAY "Nombre del reporte : ",v_nombre_reporte

      -- se indica que el reporte usara la plantilla creada
      IF ( fgl_report_loadCurrentSettings("../../afi/bin/AFIP12.4rp") ) THEN  -- if  the file loaded OK
          -- sin preview
         CALL fgl_report_selectPreview(0)
         -- se indica que se escriba en archivo
         CALL fgl_report_setOutputFileName(v_ruta_reporte)

         LET report_handler = fgl_report_commitCurrentSettings()   -- commit the file settings

         -- se inicia el reporte
         START REPORT rpt_cifras_control TO XML HANDLER report_handler

         -- se obtienen los registros rechazados
         SELECT COUNT (*)
           INTO v_cuenta_registros
           FROM tmp_afi_det_cambio_rfc_rch

         IF v_cuenta_registros = 0 THEN
            LET r_registro_rechazado.cod_rechazo = 0
            LET r_registro_rechazado.nss10       = 0
            LET r_registro_rechazado.nss11       = 0
            LET r_registro_rechazado.rfc         = 0

            OUTPUT TO REPORT rpt_cifras_control(p_folio,
                                                p_usuario_cod,
                                                p_nombre_archivo,
                                                v_nombre_reporte,
                                                v_regs_procesados,
                                                v_regs_aceptados,
                                                v_regs_rechazados,
                                                r_registro_rechazado.*)
         ELSE
            DECLARE cur_rechazados CURSOR FOR
            SELECT *
            FROM tmp_afi_det_cambio_rfc_rch
            ORDER BY cod_rechazo, nss11

            -- se envia cada registro rechazado al reporte
            FOREACH cur_rechazados INTO r_registro_rechazado.*
               -- se envia el encabezado
               OUTPUT TO REPORT rpt_cifras_control(p_folio,
                                                   p_usuario_cod,
                                                   p_nombre_archivo,
                                                   v_nombre_reporte,
                                                   v_regs_procesados,
                                                   v_regs_aceptados,
                                                   v_regs_rechazados,
                                                   r_registro_rechazado.*)
            END FOREACH
         END IF

         -- se finaliza
         FINISH REPORT rpt_cifras_control
      ELSE
         -- no se pudo leer la plantilla del reporte
         DISPLAY "No se puede leer la plantilla del reporte AFIP01.4rp"
      END IF

      -- se finaliza la operacion
      CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
           RETURNING r_bnd_fin_oper

      -- si no se pudo finalizar la operacion
      IF ( r_bnd_fin_oper <> 0 ) THEN
          DISPLAY "Ocurrió un error al finalizar la operación:"
             -- En caso de error se muestra un mensaje a usuario y no continua
          CALL fn_desplega_inc_operacion(r_bnd_fin_oper)
      END IF
   END IF

   -- se envia la cadena que indica el fin de la etapa
   CALL fn_display_proceso(1, "INTEGRACIÓN")

   LET p_titulo = "Finalización de operación - ACTUALIZACIÓN RFC - INTEGRACIÓN"

   -- se invoca el envio de correo electronico de notificacion
   CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                          NULL, -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)

END MAIN

{ ==========================================================================
Clave:  rpt_cifras_control
Nombre: rpt_cifras_control
Fecha creacion: 14 Agosto 2012
Autor: Ivan Vega
Narrativa del proceso que realiza:
Genera el reporte de cifras de control de carga inicial
 
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
============================================================================}
   REPORT rpt_cifras_control(p_folio           ,
                             p_usuario_cod     ,
                             p_nombre_archivo  ,
                             v_nombre_reporte  ,
                             p_regs_procesados ,
                             p_regs_aceptados  ,
                             p_regs_rechazados ,
                             r_registro_rechazado)

   DEFINE p_usuario_cod            LIKE seg_usuario.usuario_cod
   DEFINE p_nombre_archivo         LIKE glo_ctr_archivo.nombre_archivo
   DEFINE p_folio                  DECIMAL(9,0)

          -- cifras de control
   DEFINE v_fecha_texto            VARCHAR(10)
   DEFINE p_regs_procesados        DECIMAL(9,0)
   DEFINE p_regs_aceptados         DECIMAL(9,0)
   DEFINE p_regs_rechazados        DECIMAL(9,0)

   DEFINE r_registro_rechazado RECORD -- registro rechazado para reporte
      nss11                        CHAR(11),
      nss10                        CHAR(10),
      rfc                          CHAR(13),
      cod_rechazo                  SMALLINT
   END RECORD

   DEFINE v_descrip_rechazo        STRING -- descripcion del rechazo
   DEFINE v_nombre_reporte         STRING

   FORMAT
      FIRST PAGE HEADER
         LET v_fecha_texto = TODAY USING "dd-mm-yyyy"
         -- se despliegan los datos del encabezado
         PRINTX p_folio
         PRINTX p_usuario_cod
         PRINTX v_fecha_texto
         PRINTX p_nombre_archivo
         PRINTX v_nombre_reporte

         -- se imprimen las cifras de control
         PRINTX p_regs_procesados,
                p_regs_aceptados,
                p_regs_rechazados

      -- cada grupo de descripcion de rechazo
         BEFORE GROUP OF r_registro_rechazado.cod_rechazo
         -- NSS no encontrado
         IF ( r_registro_rechazado.cod_rechazo = 1 ) THEN
            LET v_descrip_rechazo = "NSS no encontrado en base de derechohabientes"
         END IF

         -- RFC de longitud invalida
         IF ( r_registro_rechazado.cod_rechazo = 2 ) THEN
            LET v_descrip_rechazo = "La longitud del RFC es inferior a la esperada"
         END IF

         -- se imprime la descripción
         PRINTX v_descrip_rechazo

         ON EVERY ROW
         -- se envian los registros rechazados
         PRINTX r_registro_rechazado.*

END REPORT