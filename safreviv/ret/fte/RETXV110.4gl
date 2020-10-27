--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETP150                                                                #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la integracion    #
#                para retiro contingente solo infonavit                                 #
#Fecha inicio => Marzo 02, 2012                                                         #
#########################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
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
       v_i_resultado          INTEGER, -- resultado del proceso
       v_error_isam           SMALLINT, -- error ISAM
       v_error_mensaje        VARCHAR(255),  -- mensaje devuelto por el SP
       r_bnd_fin_oper         SMALLINT,
       v_si_correcto_integra  SMALLINT,
       p_titulo               STRING, -- titulo del mensaje enviado en el correo
       p_mensaje              STRING, -- cuerpo del mensaje enviado
       v_ruta_reporte         STRING, -- ruta del archivo del reporte
       v_ruta_listados        VARCHAR(40), -- ruta de los listados
       v_ruta_ejecutable      STRING, -- ruta del ejecutable
       report_handler         om.SaxDocumentHandler, -- handler para el reporte en PDF
       v_nss                  CHAR(11), -- NSS que pudiera haber caido en error
       r_ret_solo_infonavit   RECORD LIKE ret_solo_infonavit.*,
       v_regs_procesados      INTEGER,
       v_regs_aceptados       INTEGER,
       v_regs_rechazados      INTEGER,
       v_conteo               INTEGER -- conteo de registros
   
   -- se recuperan los parametros la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = "IN331815"
   LET p_pid            = 32274
   LET p_proceso_cod    = 1517
   LET p_opera_cod      = 2
   LET p_folio          = 43976
   LET p_nombre_archivo = "NA"
   
   -- se asigna proceso y operacion
   LET g_pid         = 37274      
   LET g_proceso_cod = p_proceso_cod -- retiros contingente solo infonavit
   LET g_opera_cod   = p_opera_cod -- integracion
   LET p_folio       = 43976
   
   -- =====================================================================================
   -- =====================================================================================
   --            REPORTE DE CIFRAS DE CONTROL DE LA INTEGRACION
   -- =====================================================================================
   -- se obtiene el modulo del proceso
   SELECT ruta_listados
   INTO   v_ruta_listados
   FROM   seg_modulo
   WHERE  modulo_cod = "ret"

   
   LET v_ruta_reporte = v_ruta_listados CLIPPED, "/",
                        p_usuario_cod   CLIPPED, "-", -- usuario
                        "RETP150-"                   , -- programa
                        g_pid           USING "&&&&&","-", -- PID
                        g_proceso_cod   USING "&&&&&", "-", -- codigo del proceso
                        g_opera_cod     USING "&&&&&",".pdf" -- codigo de la operaci�n
   
   DISPLAY "Ruta del reporte: ", v_ruta_reporte
   
   -- se indica que el reporte usara la plantilla creada
   IF ( fgl_report_loadCurrentSettings("/safreviv/ret/bin/RETP150.4rp") ) THEN  -- if  the file loaded OK
   
      -- sin preview
      CALL fgl_report_selectPreview(0)
      -- se indica que se escriba en archivo
      CALL fgl_report_setOutputFileName(v_ruta_reporte)       
   
      LET report_handler = fgl_report_commitCurrentSettings()      -- commit the file settings
   
      -- se inicia el reporte
      START REPORT rpt_cifras_control TO XML HANDLER report_handler

      -- se cuenta si hay rechazos
      SELECT COUNT(*)
      INTO   v_conteo
      FROM   ret_solo_infonavit
      WHERE  folio = p_folio
      AND    cod_rechazo > 0

      -- si no se encontraron se da cero
      IF ( v_conteo IS NULL ) THEN
         LET v_conteo = 0
      END IF

      LET v_regs_procesados = 44 
      LET v_regs_aceptados  = 44
      LET v_regs_rechazados = 0

      -- se inicia en null para que no escriba nada
      INITIALIZE r_ret_solo_infonavit.* TO NULL

      -- se envia al reporte datos de encabezado
      {OUTPUT TO REPORT rpt_cifras_control(p_folio                    , 
                                          p_usuario_cod              , 
                                          v_regs_procesados          ,
                                          v_regs_aceptados           ,
                                          v_regs_rechazados          ,
                                          v_conteo                   ,
                                          r_ret_solo_infonavit.*               
                                          )}
     
      -- se obtienen los registros rechazados
      DECLARE cur_rechazo_ret CURSOR FOR
      SELECT * FROM ret_solo_infonavit
      WHERE folio = p_folio
      --AND   cod_rechazo > 0
      ORDER BY cod_rechazo
      
      FOREACH cur_rechazo_ret INTO r_ret_solo_infonavit.*
      
         -- se envian los registros rechazados al reporte
         OUTPUT TO REPORT rpt_cifras_control(p_folio                    , 
                                             p_usuario_cod              , 
                                             v_regs_procesados          ,
                                             v_regs_aceptados           ,
                                             v_regs_rechazados          ,
                                             v_conteo                   ,
                                             r_ret_solo_infonavit.*               
                                             )
      END FOREACH
      
      -- se finaliza
      FINISH REPORT rpt_cifras_control
      -- =====================================================================================
      
   ELSE
      -- no se pudo leer la plantilla del reporte
      DISPLAY "No se puede leer la plantilla del reporte RETP150.4rp"       
   END IF

END MAIN

{ ==========================================================================
Clave:  rpt_cifras_control
Nombre: rpt_cifras_control
Fecha creacion: 17 Abril 2013
Autor: Ivan Vega
Narrativa del proceso que realiza:
Genera un reporte de cifras de control de la informacion integrada en el proceso
de Retiros Solo Infonavit Contingente
 
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
============================================================================}
REPORT rpt_cifras_control(p_folio                    , 
                          p_usuario_cod              ,
                          p_regs_procesados          ,
                          p_regs_aceptados           ,
                          p_regs_rechazados          ,
                          p_conteo                   ,
                          p_ret_solo_infonavit
                          )

DEFINE p_usuario_cod                 LIKE seg_usuario.usuario_cod, -- Clave de usuario
       p_folio                       DECIMAL(9,0),
       -- cifras de control
       v_fecha_texto                 VARCHAR(10)
       ,p_regs_procesados            INTEGER -- numero de movimientos procesados
       ,p_regs_aceptados             INTEGER -- numero de movimientos aceptados
       ,p_regs_rechazados            INTEGER -- numero de movimientos rechazados
       ,p_conteo                     INTEGER
       ,p_ret_solo_infonavit         RECORD LIKE ret_solo_infonavit.* -- registro de solo Infonavit rechazado
       ,v_desc_rechazo               VARCHAR(250)
       ,v_subtotal_aivs_rechazo      DECIMAL(24,6)
       ,v_subtotal_pesos_rechazo     DECIMAL(22,2)
       ,v_total_aivs_rechazo         DECIMAL(24,6)
       ,v_total_pesos_rechazo        DECIMAL(22,2)
       ,v_cadena                     STRING

   
   
   FORMAT

      FIRST PAGE HEADER
         LET v_fecha_texto = TODAY USING "dd-mm-yyyy"
         -- se despliegan los datos del encabezado
         PRINTX p_folio
         PRINTX p_usuario_cod
         PRINTX v_fecha_texto
         DISPLAY "p_regs_procesados: ",p_regs_procesados
         DISPLAY "p_regs_aceptados:  ",p_regs_aceptados
         DISPLAY "p_regs_rechazados: ",p_regs_rechazados
         PRINTX p_regs_procesados,
                p_regs_aceptados,
                p_regs_rechazados

         -- se inician los acumuladores de total de rechazo
         LET v_total_aivs_rechazo  = 0
         LET v_total_pesos_rechazo = 0

         -- si no hay rechazos se indica
         IF ( p_conteo < 1 ) THEN
            LET v_cadena = "No se encontraron registros rechazados."
         END IF 


      BEFORE GROUP OF p_ret_solo_infonavit.cod_rechazo

         IF ( p_ret_solo_infonavit.cod_rechazo = 0 ) THEN
            LET v_desc_rechazo = "Aceptados"
         END IF
         
         -- Descripciones seg�m sp fn_ret_integra_conting_solo_infonavit
         -- NSS no encontrado
         IF ( p_ret_solo_infonavit.cod_rechazo = 1 ) THEN
            LET v_desc_rechazo = "NSS no encontrado"
         END IF

         -- NSS no inicia con 77
         IF ( p_ret_solo_infonavit.cod_rechazo = 10 ) THEN
            LET v_desc_rechazo = "NSS no inicia en 77"
         END IF

         -- La valuacion de pesos/AIVs no corresponde
         IF ( p_ret_solo_infonavit.cod_rechazo = 11 ) THEN
            LET v_desc_rechazo = "El monto en pesos no corresponde al monto en AIVs [monto vs valor fondo]"
         END IF

         -- NSS no inicia con 77
         IF ( p_ret_solo_infonavit.cod_rechazo = 12 ) THEN
            LET v_desc_rechazo = "Sin cuenta Solo Infonavit"
         END IF
         
         
         -- se envia la descripcion del tipo de rechazo
         DISPLAY "p_ret_solo_infonavit.cod_rechazo: ",p_ret_solo_infonavit.cod_rechazo
         DISPLAY "v_desc_rechazo: ",v_desc_rechazo
         PRINTX p_ret_solo_infonavit.cod_rechazo, v_desc_rechazo

         -- se reinician los acumuladores por grupo de rechazo
         LET v_subtotal_aivs_rechazo  = 0
         LET v_subtotal_pesos_rechazo = 0

                
      ON EVERY ROW
         -- se envian los registros rechazados
         PRINTX p_ret_solo_infonavit.*

         -- se acumula el monto rechazado (grupo)
         LET v_subtotal_aivs_rechazo  = v_subtotal_aivs_rechazo + p_ret_solo_infonavit.aivs_viv97
         LET v_subtotal_pesos_rechazo = v_subtotal_pesos_rechazo + p_ret_solo_infonavit.importe_viv97

         -- se acumula el monto rechazado (total)
         LET v_total_aivs_rechazo  = v_total_aivs_rechazo + p_ret_solo_infonavit.aivs_viv97
         LET v_total_pesos_rechazo = v_total_pesos_rechazo + p_ret_solo_infonavit.importe_viv97

         PRINTX v_cadena, p_conteo

      AFTER GROUP OF p_ret_solo_infonavit.cod_rechazo
         -- se imprimen los subtotales de grupo rechazado
         PRINTX v_subtotal_aivs_rechazo, v_subtotal_pesos_rechazo
         
      ON LAST ROW
         -- se imprime el total del reporte
         PRINTX v_total_aivs_rechazo, v_total_pesos_rechazo
    
END REPORT