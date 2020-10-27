--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => AFI                                                                    #
#Programa     => AFIP05                                                                 #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la integracion    #
#                de recurrente de domicilio de derechohabientes                         #
#Fecha inicio => Agosto 27, 2012                                                        #
#########################################################################################
DATABASE safre_viv
GLOBALS "AFIG01.4gl"
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_pid                  LIKE bat_ctr_operacion.pid, -- PID del proceso
       p_proceso_cod          LIKE bat_ctr_operacion.proceso_cod, -- codigo del proceso
       p_opera_cod            LIKE bat_ctr_operacion.opera_cod, -- codigo de la operacion
       p_usuario_cod          LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_folio                LIKE deo_preliquida.folio_liquida,
       p_nombre_archivo       LIKE glo_ctr_archivo.nombre_archivo, 
       v_s_sql                STRING, -- cadena con una instruccion SQL
       v_i_resultado          INTEGER -- resultado del proceso
       ,r_bnd_fin_oper        SMALLINT
       ,v_si_correcto_integra SMALLINT
       ,p_titulo              STRING -- titulo del mensaje enviado en el correo
       ,p_mensaje             STRING -- cuerpo del mensaje enviado
       ,v_error_isam          INTEGER
       ,v_mensaje             VARCHAR(250)
       ,v_regs_totales        INTEGER
       ,v_regs_aceptados      INTEGER
       ,v_regs_rechazados     INTEGER
       -- variables para el reporte de integracion
       ,p_programa_cod          VARCHAR(10)
       ,p_modulo_cod            VARCHAR(3)
       ,v_ruta_reporte       STRING -- ruta del archivo del reporte
       ,v_ruta_listados      VARCHAR(40) -- ruta de los listados
       ,v_ruta_ejecutable    STRING -- ruta del ejecutable
       ,report_handler       om.SaxDocumentHandler -- handler para el reporte en PDF
       ,v_nombre_usuario     CHAR (40)
       ,v_usuario_cod_temp   CHAR (20)
       ,v_nss_error          CHAR(11) -- NSS que genero el error
       ,v_afi_domicilio_rch    RECORD LIKE afi_domicilio_rch.*
       
   -- se recuperan los parametros la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   
   -- se asigna proceso y operacion
   LET g_pid         = p_pid      
   LET g_proceso_cod = p_proceso_cod -- retiros por disposicion de recursos
   LET g_opera_cod   = p_opera_cod -- integracion

   -- se obtiene el folio
   CALL fn_genera_folio(g_proceso_cod, g_opera_cod, p_usuario_cod)
        RETURNING p_folio
   
   -- se asume que el proceso termina correctamente
   LET v_i_resultado         = 0
   LET v_si_correcto_integra = 0

   -- se envia la cadena que indica el inicio de etapa
   CALL fn_display_proceso(0, "INTEGRACION")
   
   -- se contruye el enuncionado SQL
   LET v_s_sql = "EXECUTE FUNCTION fn_afi_integra_domicilio(?,?,?,?,?)"
   
   -- se prepara la ejecucion del stored procedure para la integracion
   PREPARE sid_integraafi FROM v_s_sql
   
   -- se ejecuta el stored procedure
   EXECUTE sid_integraafi USING p_usuario_cod, p_folio, p_nombre_archivo, g_pid, p_proceso_cod
      INTO v_i_resultado, v_error_isam, v_mensaje, v_regs_totales, v_regs_aceptados, v_regs_rechazados, v_nss_error

      
   DISPLAY "La integración se terminó completamente."
   DISPLAY "Estatus de integración:",v_i_resultado

   LET p_mensaje = "ID Proceso   : ", g_pid, "\n", 
                   "Proceso      : AFILIACION - ALTA DE DOMICILIO",
                   "Operación    : INTEGRACION\n",
                   "Fecha Inicio : ", TODAY, "\n",
                   "Fecha Fin    : ", TODAY, "\n",
                   "El folio asociado a su operación es: ", p_folio, "\n\n"

      
   -- si se termino correctamente 
   IF ( v_i_resultado = 0 )THEN
      DISPLAY v_mensaje
      DISPLAY "Registros procesados: ", v_regs_totales
      DISPLAY "Registros aceptados: ", v_regs_aceptados
      DISPLAY "Registros rechazados: ", v_regs_rechazados

      -- se complementa el mensaje
      LET p_mensaje = p_mensaje || "Integración realizada con éxito."

      
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
                        "AFIP05-"                   , -- programa
                        g_pid           USING "&&&&&","-", -- PID
                        g_proceso_cod   USING "&&&&&", "-", -- codigo del proceso
                        g_opera_cod     USING "&&&&&",".pdf" -- codigo de la operación

    DISPLAY "Ruta del reporte: ", v_ruta_reporte

    -- se indica que el reporte usara la plantilla creada
    IF ( fgl_report_loadCurrentSettings("../../afi/bin/AFIP05.4rp") ) THEN  -- if  the file loaded OK

       -- sin preview
       CALL fgl_report_selectPreview(0)
       -- se indica que se escriba en archivo
       CALL fgl_report_setOutputFileName(v_ruta_reporte)       

       LET report_handler = fgl_report_commitCurrentSettings()      -- commit the file settings
       
       
       -- se inicia el reporte
       START REPORT rpt_cifras_control TO XML HANDLER report_handler
       
       INITIALIZE v_afi_domicilio_rch.* TO NULL
       LET v_afi_domicilio_rch.cod_rechazo = -99
       -- se envian los datos del encabezado
                  
       -- se envian los datos al reporte
       OUTPUT TO REPORT rpt_cifras_control(p_folio            ,
                                           p_usuario_cod      ,
                                           v_regs_totales     ,
                                           v_regs_aceptados   ,
                                           v_regs_rechazados  ,
                                           v_afi_domicilio_rch.*
                                           )
       
       -- se leen los rechazos
       DECLARE cur_rch_afi_dom CURSOR FOR
       SELECT *
       FROM   afi_domicilio_rch
       WHERE  folio_lote = p_folio
       
       FOREACH cur_rch_afi_dom INTO v_afi_domicilio_rch.*
          -- se envian los datos al reporte
          OUTPUT TO REPORT rpt_cifras_control(p_folio            ,
                                              p_usuario_cod      ,
                                              v_regs_totales     ,
                                              v_regs_aceptados   ,
                                              v_regs_rechazados  ,
                                              v_afi_domicilio_rch.*
                                              )       
       END FOREACH
       
       -- se finaliza
       FINISH REPORT rpt_cifras_control
                  
       
    ELSE
       DISPLAY "No se puede leer la plantilla del reporte AFIP05.4rp"
       DISPLAY "El reporte no fue generado."
    END IF

   -- =====================================================================================


   ELSE
      -- se complementa el mensaje
      LET p_mensaje = p_mensaje || "El proceso de Integración ha finalizado pero con errores de validación."
      DISPLAY "Ocurrió un error al realizar la integración"
      DISPLAY "Error (SQL) : ", v_i_resultado
      DISPLAY "Error (ISAM): ", v_error_isam
      DISPLAY "Mensaje     : ", v_mensaje
      DISPLAY "NSS         : ", v_nss_error

      -- se marca la operacion en error
      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
                          RETURNING v_i_resultado

   END IF

   LET p_titulo = "Finalización de operación - ALTA DE DOMICILIO - INTEGRACION"
   
   -- se invoca el envio de correo electronico de notificacion
   CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                          NULL, -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)
   
   -- se envia la cadena que indica el fin de la etapa
   CALL fn_display_proceso(1, "INTEGRACION")

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
REPORT rpt_cifras_control(p_folio             ,
                          p_usuario_cod       ,
                          p_regs_totales      ,
                          p_regs_aceptados    ,
                          p_regs_rechazados   ,
                          p_afi_domicilio_rch
                          )

DEFINE p_usuario_cod            LIKE seg_usuario.usuario_cod, -- Clave de usuario
       p_folio                  DECIMAL(9,0),
       -- cifras de control
       v_fecha_texto            VARCHAR(10)
       ,p_regs_totales          INTEGER
       ,p_regs_aceptados        INTEGER -- numero de movimientos aceptados
       ,p_regs_rechazados       INTEGER -- numero de movimientos rechazados
       ,p_afi_domicilio_rch     RECORD LIKE afi_domicilio_rch.* 
       ,v_desc_rechazo               VARCHAR(250)
       

   FORMAT

      FIRST PAGE HEADER
         LET v_fecha_texto = TODAY USING "dd-mm-yyyy"
         -- se despliegan los datos del encabezado
         PRINTX p_folio, p_usuario_cod, v_fecha_texto
         PRINTX p_regs_totales    ,
                p_regs_aceptados  ,
                p_regs_rechazados 
         
      BEFORE GROUP OF p_afi_domicilio_rch.cod_rechazo
         -- se obtiene la descripcion del rechazo
         SELECT des_rechazo
         INTO   v_desc_rechazo
         FROM   afi_cat_rch
         WHERE  cod_rechazo = p_afi_domicilio_rch.cod_rechazo

         -- se envia la descripcion del tipo de rechazo
         PRINTX p_afi_domicilio_rch.cod_rechazo, v_desc_rechazo

         
      ON EVERY ROW
         PRINTX p_afi_domicilio_rch.*
    
END REPORT
