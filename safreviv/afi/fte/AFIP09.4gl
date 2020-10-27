--=============================================================================
-- Versión: 1.0.0
-- Fecha última modificación:
--=============================================================================
#########################################################################################
#Módulo       => AFI                                                                    #
#Programa     => AFIP09                                                                 #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la integración    #
#                de recurrente de correo electrónico de derechohabientes                #
#Fecha inicio => Agosto 27, 2012                                                        #
#########################################################################################

DATABASE safre_viv

GLOBALS "AFIG01.4gl"

GLOBALS

   DEFINE g_pid                      LIKE bat_ctr_proceso.pid     --  ID del proceso
   DEFINE g_proceso_cod              LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE g_opera_cod                LIKE cat_operacion.opera_cod -- codigo de operacion

END GLOBALS

MAIN

   DEFINE p_pid                      LIKE bat_ctr_operacion.pid          -- PID del proceso
   DEFINE p_proceso_cod              LIKE bat_ctr_operacion.proceso_cod  -- codigo del proceso
   DEFINE p_opera_cod                LIKE bat_ctr_operacion.opera_cod    -- codigo de la operacion
   DEFINE p_usuario_cod              LIKE seg_usuario.usuario_cod        -- clave del usuario firmado
   DEFINE p_folio                    LIKE deo_preliquida.folio_liquida
   DEFINE p_nombre_archivo           LIKE glo_ctr_archivo.nombre_archivo
   DEFINE v_s_sql                    STRING  -- cadena con una instruccion SQL
   DEFINE v_i_resultado              INTEGER -- resultado del proceso
   DEFINE r_bnd_fin_oper             SMALLINT
   DEFINE v_si_correcto_integra      SMALLINT
   DEFINE p_titulo                   STRING -- titulo del mensaje enviado en el correo
   DEFINE p_mensaje                  STRING -- cuerpo del mensaje enviado
   DEFINE v_error_isam               INTEGER
   DEFINE v_mensaje                  VARCHAR(250)
   DEFINE v_regs_totales             INTEGER
   DEFINE v_regs_aceptados           INTEGER
   DEFINE v_regs_rechazados          INTEGER

   -- variables para el reporte de integración
   DEFINE p_programa_cod             VARCHAR(10)
   DEFINE p_modulo_cod               VARCHAR(3)
   DEFINE v_ruta_reporte             STRING -- ruta del archivo del reporte
   DEFINE v_ruta_listados            VARCHAR(40) -- ruta de los listados
   DEFINE v_ruta_ejecutable          STRING -- ruta del ejecutable
   DEFINE report_handler             om.SaxDocumentHandler -- handler para el reporte en PDF
   DEFINE v_nombre_usuario           CHAR (40)
   DEFINE v_usuario_cod_temp         CHAR (20)
   DEFINE v_nss_error                CHAR(11) -- NSS con error

   DEFINE v_r_afi_contacto_electronico_rch RECORD LIKE afi_contacto_electronico_rch.*

   -- se recuperan los parametros la clave de usuario desde parametro
   -- argumento con índice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)

   -- se asigna proceso y operación
   LET g_pid         = p_pid
   LET g_proceso_cod = p_proceso_cod -- retiros por disposición de recursos
   LET g_opera_cod   = p_opera_cod   -- integración

   -- se obtiene el folio
   CALL fn_genera_folio(g_proceso_cod, g_opera_cod, p_usuario_cod)
        RETURNING p_folio

   -- se asume que el proceso termina correctamente
   LET v_i_resultado         = 0
   LET v_si_correcto_integra = 0

   -- se envia la cadena que indica el inicio de etapa
   CALL fn_display_proceso(0, "INTEGRACION")

   -- se contruye el enuncionado SQL
   LET v_s_sql = "EXECUTE FUNCTION fn_afi_integra_contacto_electronico(?,?,?,?,?)"

   -- se prepara la ejecucion del stored procedure para la integracion
   PREPARE sid_integraafi FROM v_s_sql

   -- se ejecuta el stored procedure
   EXECUTE sid_integraafi USING p_usuario_cod, p_folio, p_nombre_archivo, g_pid, p_proceso_cod
      INTO v_i_resultado, v_error_isam, v_mensaje, v_nss_error

   DISPLAY "La integración se terminó completamente."
   DISPLAY "Estatus de integración:",v_i_resultado

   LET p_mensaje = "ID Proceso   : ", g_pid, "\n", 
                   "Proceso      : AFILIACIÓN - ALTA DE CONTRACTO ELECTRÓNICO\n",
                   "Operación    : INTEGRACIÓN\n",
                   "Fecha Inicio : ", TODAY, "\n",
                   "Fecha Fin    : ", TODAY, "\n",
                   "El folio asociado a su operación es: ", p_folio, "\n\n"

   -- si se termino correctamente
   IF ( v_i_resultado = 0 )THEN
      DISPLAY v_mensaje

      -- se complementa el mensaje
      LET p_mensaje = p_mensaje || "Integración realizada con éxito\n.Ya se puede continuar con la Preliquidación"

      CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
                                  RETURNING r_bnd_fin_oper

      -- =====================================================================================
      -- =====================================================================================
      --            REPORTE DE CIFRAS DE CONTROL DE LA INTEGRACION
      -- =====================================================================================
      -- se obtiene el modulo del proceso
      SELECT ruta_listados
      INTO   v_ruta_listados
      FROM   seg_modulo
      WHERE  modulo_cod = "afi"

      LET v_ruta_reporte = v_ruta_listados CLIPPED, "/",
                           p_usuario_cod   CLIPPED, "-", -- usuario
                           "AFIP09-"                   , -- programa
                           g_pid           USING "&&&&&","-", -- PID
                           g_proceso_cod   USING "&&&&&", "-", -- codigo del proceso
                           g_opera_cod     USING "&&&&&",".pdf" -- codigo de la operación

      -- se indica que el reporte usara la plantilla creada
      IF ( fgl_report_loadCurrentSettings("../../afi/bin/AFIP09.4rp") ) THEN  -- if  the file loaded OK
         -- sin preview
         CALL fgl_report_selectPreview(0)
         -- se indica que se escriba en archivo
         CALL fgl_report_setOutputFileName(v_ruta_reporte)

         LET report_handler = fgl_report_commitCurrentSettings()      -- commit the file settings

         -- se inicia el reporte
         INITIALIZE v_r_afi_contacto_electronico_rch.* TO NULL

         LET v_r_afi_contacto_electronico_rch.cod_rechazo = -99

         -- se inicia el reporte
         START REPORT rpt_cifras_control TO XML HANDLER report_handler

         -- se obtienen los registros totales
         SELECT COUNT(*)
           INTO v_regs_totales
           FROM safre_tmp:tmp_det_contacto_elect

         -- registros aceptados
         SELECT COUNT(*)
           INTO v_regs_aceptados
           FROM afi_contacto_electronico
          WHERE folio_lote = p_folio

         -- los registros rechazados son los totales menos los aceptados
         LET v_regs_rechazados = v_regs_totales - v_regs_aceptados

         DISPLAY ""
         DISPLAY "Registros procesados: ", v_regs_totales
         DISPLAY "Registros aceptados : ", v_regs_aceptados
         DISPLAY "Registros rechazados: ", v_regs_rechazados
         DISPLAY ""

         DISPLAY "Ruta del reporte: ", v_ruta_reporte
         DISPLAY ""

         -- se envian los datos al reporte
         OUTPUT TO REPORT rpt_cifras_control(p_folio          , 
                                             p_usuario_cod    , 
                                             v_regs_totales   ,
                                             v_regs_aceptados , 
                                             v_regs_rechazados,
                                             v_r_afi_contacto_electronico_rch.*)

         -- se obtienen los registros rechazados
         DECLARE cur_Rechazos CURSOR FOR
         SELECT *
         FROM   afi_contacto_electronico_rch
         WHERE  folio_lote = p_folio

         FOREACH cur_rechazos INTO v_r_afi_contacto_electronico_rch.*
            -- se envian los datos al reporte
            OUTPUT TO REPORT rpt_cifras_control(p_folio          ,
                                                p_usuario_cod    ,
                                                v_regs_totales   ,
                                                v_regs_aceptados ,
                                                v_regs_rechazados,
                                                v_r_afi_contacto_electronico_rch.*)
         END FOREACH

         -- se finaliza
         FINISH REPORT rpt_cifras_control
         -- =====================================================================================
      ELSE
         DISPLAY "No se puede leer la plantilla del reporte AFIP09.4rp"
      END IF
   ELSE
      -- se complementa el mensaje
      LET p_mensaje = p_mensaje || "El proceso de Integración ha finalizado pero con errores de validación."
      DISPLAY "Ocurrió un error al realizar la integración"
      DISPLAY "Error (SQL) : ", v_i_resultado
      DISPLAY "Error (ISAM): ", v_error_isam
      DISPLAY "Mensaje     : ", v_mensaje
      DISPLAY "NSS         : ",  v_nss_error

      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
                          RETURNING v_i_resultado
   END IF

   LET p_titulo = "Finalización de operación - ALTA DE CONTACTO ELECTRÓNICO - INTEGRACIÓN"

   -- se invoca el envio de correo electronico de notificacion
   CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                          NULL, -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)

   -- se envia la cadena que indica el fin de la etapa
   CALL fn_display_proceso(1, "INTEGRACIÓN")

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
                          p_regs_totales    ,
                          p_regs_aceptados  ,
                          p_regs_rechazados ,
                          p_r_afi_contacto_electronico_rch
                          )

   DEFINE p_usuario_cod              LIKE seg_usuario.usuario_cod -- Clave de usuario
   DEFINE p_folio                    DECIMAL(9,0)
   DEFINE v_fecha_texto              VARCHAR(10)
   DEFINE p_regs_totales             INTEGER
   DEFINE p_regs_aceptados           INTEGER -- número de movimientos aceptados
   DEFINE p_regs_rechazados          INTEGER -- número de movimientos rechazados

   DEFINE p_r_afi_contacto_electronico_rch RECORD LIKE afi_contacto_electronico_rch.*

   DEFINE v_desc_rechazo               VARCHAR(250)

   FORMAT

      FIRST PAGE HEADER
         LET v_fecha_texto = TODAY USING "dd-mm-yyyy"
         -- se despliegan los datos del encabezado
         PRINTX p_folio, p_usuario_cod, v_fecha_texto
         PRINTX p_regs_totales
         PRINTX p_regs_aceptados
         PRINTX p_regs_rechazados

      BEFORE GROUP OF p_r_afi_contacto_electronico_rch.cod_rechazo
         -- se obtiene la descripcion del rechazo
         SELECT des_rechazo
           INTO v_desc_rechazo
           FROM afi_cat_rch
          WHERE cod_rechazo = p_r_afi_contacto_electronico_rch.cod_rechazo

         -- se envia la descripcion del tipo de rechazo
         PRINTX p_r_afi_contacto_electronico_rch.cod_rechazo, v_desc_rechazo

      ON EVERY ROW
         PRINTX p_r_afi_contacto_electronico_rch.*

END REPORT
