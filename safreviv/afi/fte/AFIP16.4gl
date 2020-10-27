--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => AFI                                                                    #
#Programa     => AFIP03                                                                 #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la integracion    #
#                de movimientos afiliatorios opt 75                                     #
#Fecha inicio => Junio 22, 2012                                                         #
#########################################################################################
DATABASE safre_viv
GLOBALS "AFIG01.4gl"
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_pid                         LIKE bat_ctr_operacion.pid, -- PID del proceso
       p_proceso_cod                 LIKE bat_ctr_operacion.proceso_cod, -- codigo del proceso
       p_opera_cod                   LIKE bat_ctr_operacion.opera_cod, -- codigo de la operacion
       p_usuario_cod                 LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_folio                       LIKE deo_preliquida.folio_liquida,
       p_nombre_archivo              LIKE glo_ctr_archivo.nombre_archivo, 
       v_s_sql                       STRING, -- cadena con una instruccion SQL
       v_i_resultado                 INTEGER -- resultado del proceso
       ,r_bnd_fin_oper               SMALLINT
       ,v_si_correcto_integra        SMALLINT
       ,p_titulo                     STRING -- titulo del mensaje enviado en el correo
       ,p_mensaje                    STRING -- cuerpo del mensaje enviado
       ,v_error_isam                 INTEGER
       ,v_mensaje                    VARCHAR(250)
       ,v_altas_aceptadas            INTEGER
       ,v_altas_rechazadas           INTEGER
       ,v_nss_error                  LIKE afi_derechohabiente.nss
       -- variables para el reporte de integracion
       DEFINE v_ruta_reporte       STRING -- ruta del archivo del reporte
       DEFINE v_ruta_listados      VARCHAR(40) -- ruta de los listados
       DEFINE report_handler       om.SaxDocumentHandler -- handler para el reporte en PDF
       -- cifras de control
       DEFINE v_registros_totales  INTEGER
       -- registros rechazados
       DEFINE v_afi_nss_trm_rch    RECORD LIKE afi_nss_trm_rch.*
	   -- apuntador para archivo de salida con registros rechazados
	   DEFINE v_archivo_salida     base.channel
	   DEFINE v_registro_rechazo   RECORD -- registro con nss rechazado
          nss          CHAR(11),
          curp         CHAR(18),
          rfc          CHAR(13),
          nombre_imss  CHAR(50),
          sexo         CHAR(1) ,
          cod_rechazo  SMALLINT,
          desc_rechazo CHAR(40)
	   END RECORD
	   DEFINE v_ruta_envio            LIKE seg_modulo.ruta_envio -- ruta de envio
	   DEFINE v_ruta_archivo_salida   STRING -- ruta completa del archivo de salida
	   DEFINE v_nombre_archivo_salida STRING -- nombre archivo salida
	   
   
   ##Ejecuta prevalidación de saldos
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
   LET g_proceso_cod = p_proceso_cod -- movimientos afiliatorios
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
   LET v_s_sql = "EXECUTE FUNCTION fn_afi_integra_nuevos_nss_trm(?,?,?,?,?)"
   
   -- se prepara la ejecucion del stored procedure para la integracion
   PREPARE sid_nss_trm FROM v_s_sql
   
   -- se ejecuta el stored procedure
   EXECUTE sid_nss_trm USING p_usuario_cod, p_folio, p_nombre_archivo, g_pid, p_proceso_cod
      INTO v_i_resultado, v_error_isam, v_mensaje, v_altas_aceptadas, v_altas_rechazadas, v_nss_error
      
   -- si el resultado fue correcto
   IF ( v_i_resultado <> 0 ) THEN
      DISPLAY "Error en la integración..."
      DISPLAY "NSS con error: ", v_nss_error
      DISPLAY "Mensaje      : ", v_mensaje
      DISPLAY "Error (SQL)  : ", v_i_resultado
      DISPLAY "Error (ISAM) : ", v_error_isam

      -- se marca la operacion en error
      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
                    RETURNING v_i_resultado
      
      -- se prepara el mensaje de correo
      LET p_mensaje = "ID Proceso   : ", g_pid, "\n", 
                      "Proceso      : NUEVOS NSS DE TRM\n",
                      "Operación    : INTEGRACIÓN\n",
                      "Fecha Inicio : ", TODAY, "\n",
                      "Fecha Fin    : ", TODAY, "\n",
                      "El folio asociado a su operación es: ", p_folio, "\n\n",
                      v_mensaje
                      
      LET p_titulo = "Finalización de operación - NUEVOS NSS TRM - INTEGRACION"
      
      -- se invoca el envio de correo electronico de notificacion
      CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                             NULL, -- no lleva archivo adjunto
                             p_titulo,
                             p_mensaje)
   ELSE              

      LET p_mensaje = "ID Proceso   : ", g_pid, "\n", 
                      "Proceso      : NUEVOS NSS DE TRM\n",
                      "Operación    : INTEGRACIÓN\n",
                      "Fecha Inicio : ", TODAY, "\n",
                      "Fecha Fin    : ", TODAY, "\n",
                      "El folio asociado a su operación es: ", p_folio, "\n"
      
      DISPLAY "=========================================================================="
      DISPLAY "                    RESULTADOS DE LA INTEGRACION"
      DISPLAY "=========================================================================="      
      
	  -- se complementa el mensaje
      LET p_mensaje = p_mensaje || "Integración de nuevos NSS de TRM realizada con éxito.\n"
	  DISPLAY p_mensaje
	  
      -- se obtiene el total de registros
      SELECT COUNT(*)
      INTO   v_registros_totales
      FROM   safre_tmp:tmp_afi_nuevos_nss_trm
	  
	  DISPLAY "\n\nNumero de registros procesados: ", v_registros_totales
	  DISPLAY "Numero de altas aceptadas     : ", v_altas_aceptadas
	  DISPLAY "Numero de altas rechazadas    : ", v_altas_rechazadas
      -- se finaliza la operacion      
      CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
                                  RETURNING r_bnd_fin_oper
            
      -- si no se pudo finalizar la operacion                      
      IF ( r_bnd_fin_oper <> 0 ) THEN
         DISPLAY "Ocurrió un error al finalizar la operación:"
         -- En caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_bnd_fin_oper)
      END IF 

         
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
                           "AFIP16-"                   , -- programa
                           g_pid           USING "&&&&&","-", -- PID
                           g_proceso_cod   USING "&&&&&", "-", -- codigo del proceso
                           g_opera_cod     USING "&&&&&",".pdf" -- codigo de la operación
      
      DISPLAY "Ruta del reporte: ", v_ruta_reporte
      
      -- se indica que el reporte usara la plantilla creada
      IF ( fgl_report_loadCurrentSettings("../../afi/bin/AFIP16.4rp") ) THEN  -- if  the file loaded OK
      
         -- sin preview
         CALL fgl_report_selectPreview(0)
         -- se indica que se escriba en archivo
         CALL fgl_report_setOutputFileName(v_ruta_reporte)       
      
         LET report_handler = fgl_report_commitCurrentSettings()      -- commit the file settings
      
      
         -- se inicia el registro de rechazos
         INITIALIZE v_afi_nss_trm_rch.* TO NULL
         LET v_afi_nss_trm_rch.cod_rechazo = -99
         
         -- se inicia el reporte
         START REPORT rpt_cifras_control TO XML HANDLER report_handler
         
         -- se envian los datos al reporte
         OUTPUT TO REPORT rpt_cifras_control(p_folio            , 
                                             p_usuario_cod      ,
	     									v_registros_totales,
                                             v_altas_aceptadas  ,
                                             v_altas_rechazadas ,
                                             v_afi_nss_trm_rch.*
                                             )
         
         -- se leen los rechazos
         DECLARE cur_rechazos CURSOR FOR
         SELECT *
         FROM   afi_nss_trm_rch
         WHERE  folio =  p_folio
		 ORDER BY cod_rechazo
         
         FOREACH cur_rechazos INTO v_afi_nss_trm_rch.*
            -- se envian los datos al reporte
            OUTPUT TO REPORT rpt_cifras_control(p_folio            , 
                                                p_usuario_cod      ,
	     									    v_registros_totales,
                                                v_altas_aceptadas  ,
                                                v_altas_rechazadas ,
                                                v_afi_nss_trm_rch.*
                                                )
         
         END FOREACH
                                             
         -- se finaliza
         FINISH REPORT rpt_cifras_control
         -- =====================================================================================
		 
		 -- si hay registros rechazados se crea archivo de salida
		 IF ( v_altas_rechazadas > 0 ) THEN
		 
		    -- se crea el archivo de salida con los registros rechazados
		    LET v_archivo_salida = base.Channel.create()
		    
		    -- el separador sera el pipe
		    CALL v_archivo_salida.setDelimiter("|")
		    
		    -- se obtiene la ruta de envio
		    SELECT ruta_envio
		    INTO   v_ruta_envio
		    FROM   seg_modulo
		    WHERE  modulo_cod = "afi"
		    
			DISPLAY "\n\n----------------------------------------------------"
			DISPLAY "Generando archivo de salida con registros rechazados"
			
		    -- el nombre del archivo de salida lo ponemos como RESP_ del original
		    LET v_nombre_archivo_salida = "RESP_", p_nombre_archivo CLIPPED
		    
		    LET v_ruta_archivo_salida = v_ruta_envio CLIPPED, "/", v_nombre_archivo_salida
		    
			DISPLAY "\nRuta del archivo de salida: ", v_ruta_archivo_salida
			
		    -- se abre el apuntador para el archivo
		    CALL v_archivo_salida.openFile(v_ruta_archivo_salida, "w")

            DECLARE cur_rechazos_archivo CURSOR FOR
            SELECT a.nss         ,
                   a.curp        ,
                   a.rfc         ,
                   a.nombre_imss ,
                   a.sexo        ,
                   a.cod_rechazo ,
                   b.des_rechazo
            FROM   afi_nss_trm_rch a,
                   afi_cat_rch     b
            WHERE  a.folio       =  p_folio
			AND    a.cod_rechazo = b.cod_rechazo
			ORDER BY a.cod_rechazo
            
            FOREACH cur_rechazos_archivo INTO v_registro_rechazo.*
               -- se envian los datos al archivo
			   CALL v_archivo_salida.write([v_registro_rechazo.*])
            END FOREACH
		    
		    -- se cierra el archivo
		    CALL v_archivo_salida.close()
			
			DISPLAY "\nArchivo creado exitosamente..."
		 ELSE
		    DISPLAY "\nNo se tienen registros rechazados. No se generó archivo de salida con registros rechazados."
		 END IF
      ELSE
         DISPLAY "No se puede leer la plantilla del reporte AFIP16.4rp"
		 DISPLAY "El reporte no se pudo crear."
      END IF
      
      -- se envia la cadena que indica el fin de la etapa
      CALL fn_display_proceso(1, "INTEGRACION")
       
      LET p_titulo = "Finalización de operación - NUEVOS NSS DE TRM - INTEGRACION"
        
      -- se invoca el envio de correo electronico de notificacion
      CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                             NULL, -- no lleva archivo adjunto
                             p_titulo,
                             p_mensaje)
   END IF

END MAIN

{ ==========================================================================
Clave:  rpt_cifras_control
Nombre: rpt_cifras_control
Fecha creacion: 15 Enero 2013
Autor: Ivan Vega
Narrativa del proceso que realiza:
Genera el reporte de cifras de control de la carga de nuevos NSS de TRM
 
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
============================================================================}
REPORT rpt_cifras_control(p_folio               , 
                          p_usuario_cod         ,
						  p_registros_totales   ,
                          p_altas_aceptadas     ,
                          p_altas_rechazadas    ,
                          p_afi_nss_trm_rch
                          )

DEFINE  p_usuario_cod        LIKE seg_usuario.usuario_cod -- Clave de usuario
       ,p_folio              DECIMAL(9,0)
       -- cifras de control
       ,v_fecha_texto        VARCHAR(10)
	   ,p_registros_totales  INTEGER -- numero total de registros procesados
       ,p_altas_aceptadas    INTEGER -- numero de movimientos aceptados
       ,p_altas_rechazadas   INTEGER -- numero de movimientos rechazados
       ,p_afi_nss_trm_rch    RECORD LIKE afi_nss_trm_rch.*
       ,v_desc_rechazo       VARCHAR(250)

   FORMAT

      FIRST PAGE HEADER
         LET v_fecha_texto = TODAY USING "dd-mm-yyyy"
         -- se despliegan los datos del encabezado
         PRINTX p_folio, p_usuario_cod, v_fecha_texto
         PRINTX p_registros_totales,
		        p_altas_aceptadas  ,
                p_altas_rechazadas
         
      BEFORE GROUP OF p_afi_nss_trm_rch.cod_rechazo
         -- se obtiene la descripcion del rechazo
         SELECT des_rechazo
         INTO   v_desc_rechazo
         FROM   afi_cat_rch
         WHERE  cod_rechazo = p_afi_nss_trm_rch.cod_rechazo

         -- se envia la descripcion del tipo de rechazo
         PRINTX p_afi_nss_trm_rch.cod_rechazo, v_desc_rechazo
                
      ON EVERY ROW
         -- se envian los registros rechazados
         PRINTX p_afi_nss_trm_rch.*

    
END REPORT