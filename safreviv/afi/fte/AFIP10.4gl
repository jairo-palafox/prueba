--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => AFI                                                                    #
#Programa     => AFIP10                                                                 #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la lectura de los #
#                datos de la BDNSVIV cargada y actualizacion de los mismos en los datos #
#                de Afiliacion                                                          #
#Fecha inicio => Marzo 04, 2013                                                         #
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
       ,v_i_resultado_alta           SMALLINT -- resultado de la integracion de movimientos de alta 
       ,v_i_resultado_baja           SMALLINT -- resultado de la integracion de movimientos de baja
       ,v_i_resultado_cambio_nombre  SMALLINT -- resultado de la integracion de movimientos de cambio de nombre
       ,v_i_resultado_cambio_nss     SMALLINT -- resultado de la integracion de movimientos de nss (unificacion)
       ,v_i_resultado_reingreso      SMALLINT -- resultado de la integracion de movimientos de reingreso
       ,r_bnd_fin_oper               SMALLINT
       ,v_si_correcto_integra        SMALLINT
       ,p_titulo                     STRING -- titulo del mensaje enviado en el correo
       ,p_mensaje                    STRING -- cuerpo del mensaje enviado
       ,v_error_isam                 INTEGER
       ,v_mensaje                    VARCHAR(250)
       ,v_mensaje_alta               VARCHAR(250) -- mensaje devuelto por la integracion de alta
       ,v_mensaje_baja               VARCHAR(250) -- mensaje devuelto por la integracion de baja
       ,v_mensaje_cambio_nombre      VARCHAR(250) -- mensaje devuelto por la integracion de cambio de nombre
       ,v_mensaje_cambio_nss         VARCHAR(250) -- mensaje devuelto por la integracion de cambio de nss
       ,v_mensaje_reingreso          VARCHAR(250) -- mensaje devuelto por la integracion de reingreso
       ,v_regs_aceptados_alta        INTEGER -- numero de movimientos aceptados
       ,v_regs_rechazados_alta       INTEGER -- numero de movimientos rechazados
       ,v_bajas_realizadas           INTEGER -- numero de bajas realizadas
       ,v_cambios_nombre_realizados  INTEGER -- numero de cambios de nombre realizados
       ,v_nss_unificadores           INTEGER -- numero de NSS unificadores
       ,v_nss_unificados             INTEGER -- numero de NSS unificados
       ,v_reingresos                 INTEGER -- numero de reingresos realizados

       -- variables para el reporte de integracion
       DEFINE v_ruta_reporte       STRING -- ruta del archivo del reporte
       DEFINE v_ruta_listados      VARCHAR(40) -- ruta de los listados
       DEFINE report_handler       om.SaxDocumentHandler -- handler para el reporte en PDF
       -- cifras de control
       DEFINE v_regs_procesados       INTEGER
       DEFINE v_regs_aceptados        INTEGER
       DEFINE v_regs_rechazados       INTEGER
       DEFINE v_regs_cambio_nss       INTEGER
       DEFINE v_regs_procesados_alta  INTEGER  --total procesados ALTA
       DEFINE v_bajas_procesadas      INTEGER  --total aceptadas  BAJA
       DEFINE v_bajas_aceptadas       INTEGER  --total aceptadas  BAJA
       DEFINE v_bajas_rechazadas      INTEGER  --total rechazadas BAJA 
       DEFINE v_cambio_nom_aceptados  INTEGER  --total aceptados  CAMBIO NOMBRE
       DEFINE v_cambio_nom_rechazados INTEGER  --total rechazados CAMBIO NOMBRE
       DEFINE v_cambio_nss_aceptados  INTEGER  --total aceptados  CAMBIO NSS
       DEFINE v_cambio_nss_rechazados INTEGER  --total rechazados CAMBIO NSS
       DEFINE v_reingresos_aceptados  INTEGER  --total aceptados  REINGRESOS
       DEFINE v_reingresos_rechazados INTEGER  --total rechazados REINGRESOS

       -- registros rechazados
       DEFINE v_afi_rch_afiliatorio RECORD LIKE afi_rch_afiliatorio.*
       -- NSS en que pudo haber ocurrido un error
       DEFINE v_nss_error_alta           CHAR(11)
       DEFINE v_nss_error_baja           CHAR(11)
       DEFINE v_nss_error_cambio_nombre  CHAR(11)
       DEFINE v_nss_error_cambio_nss     CHAR(11)
       DEFINE v_nss_error_reingreso      CHAR(11)

       DEFINE v_ind_rech INTEGER
       DEFINE v_rch_tipo_mov RECORD 
                 v_tpo_movimiento CHAR(2), 
                 v_folio_lote     DECIMAL(9,0), 
                 v_total_rechazos INTEGER 
       END RECORD 

   -- se recuperan los parametros la clave de usuario desde parametro 
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
   CALL fn_display_proceso(0, "ACTUALIZACION BDNSVIV-Afiliacion")

   -- =============================================================================
   -- PREVALIDACION DE CONTENIDO DE ARCHIVO
   -- se verifican registros contra sumario
   -- =============================================================================
   DISPLAY "\n Ejecutando prevalidación de la lectura y actualización de datos BDNSVIV-Afiliacion..."
   -- se contruye el enuncionado SQL
   LET v_s_sql = "EXECUTE FUNCTION fn_afi_prevalida_integracion(?,?,?,?,?)"
   
   -- se prepara la ejecucion del stored procedure para la integracion
   --PREPARE sid_afi_prevalida FROM v_s_sql
   
   -- se ejecuta el stored procedure
   --EXECUTE sid_afi_prevalida USING p_usuario_cod, p_folio, p_nombre_archivo, g_pid, p_proceso_cod
   --   INTO v_i_resultado, v_error_isam, v_mensaje

   -- se muestra el mensaje de la prevalidacion
   DISPLAY v_mensaje

   -- si el resultado fue correcto
   IF ( v_i_resultado <> 0 ) THEN
      DISPLAY "Ocurrió un error durante el proceso de lectura y actualización..."
      DISPLAY "Error (SQL) : ", v_i_resultado
      DISPLAY "Error (ISAM): ", v_error_isam
      DISPLAY "Mensaje     : ", v_mensaje
      
      -- se marca la operacion en error
      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
                    RETURNING v_i_resultado
      
      -- se prepara el mensaje de correo
      LET p_mensaje = "ID Proceso   : ", g_pid, "\n", 
                      "Proceso      : MOVIMIENTOS AFILIATORIOS\n",
                      "Operación    : INTEGRACIÓN\n",
                      "Fecha Inicio : ", TODAY, "\n",
                      "Fecha Fin    : ", TODAY, "\n",
                      "El folio asociado a su operación es: ", p_folio, "\n\n",
                      v_mensaje
                      
      LET p_titulo = "Finalización de operación - Actualización BDNSVIV-Afiliacion"
      
      -- se invoca el envio de correo electronico de notificacion
      CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                             NULL, -- no lleva archivo adjunto
                             p_titulo,
                             p_mensaje)
   ELSE
         
      DISPLAY "=========================================================================="
      DISPLAY "                    RESULTADOS DE LA ACTUALIZACION"
      DISPLAY "=========================================================================="
      
         LET p_mensaje = p_mensaje || "Integración de movimientos afiliatorios realizada con éxito."
         DISPLAY "Integración de movimientos afiliatorios realizada con éxito."
         
{         
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
                              "AFIP01-"                   , -- programa
                              g_pid           USING "&&&&&","-", -- PID
                              g_proceso_cod   USING "&&&&&", "-", -- codigo del proceso
                              g_opera_cod     USING "&&&&&",".pdf" -- codigo de la operación
         
         DISPLAY "Ruta del reporte: ", v_ruta_reporte
         
         -- se indica que el reporte usara la plantilla creada
         IF ( fgl_report_loadCurrentSettings("../../afi/bin/AFIP01.4rp") ) THEN  -- if  the file loaded OK
         
            -- sin preview
            CALL fgl_report_selectPreview(0)
            -- se indica que se escriba en archivo
            CALL fgl_report_setOutputFileName(v_ruta_reporte)       
         
            LET report_handler = fgl_report_commitCurrentSettings()      -- commit the file settings
         
            -- se inicia el reporte
            START REPORT rpt_cifras_control TO XML HANDLER report_handler
            
            INITIALIZE v_afi_rch_afiliatorio.* TO NULL

            LET v_afi_rch_afiliatorio.cod_rechazo = -99

            -- se obtienen los totales de registros procesados
            SELECT num_altas ,
                   num_bajas ,
                   num_cambio_nombre ,
                   num_reingreso
              INTO v_regs_procesados_alta     ,
                   v_bajas_realizadas         ,
                   v_cambios_nombre_realizados,
                   v_reingresos
              FROM safre_tmp:tmp_afi_sumario;

            --se obtienen los totales de reachazados por tipo de movimiento
            DECLARE cur_total_rch CURSOR FOR                           
                             SELECT tpo_movimiento, folio_lote, count(*)
                             FROM afi_rch_afiliatorio                   
                             WHERE folio_lote = p_folio                 
                             GROUP BY 1,2        
                             
            LET v_ind_rech = 0

            FOREACH cur_total_rch  INTO v_rch_tipo_mov.*

               IF v_rch_tipo_mov.v_total_rechazos IS NULL THEN
                  LET v_rch_tipo_mov.v_total_rechazos = 0 
               END IF  
            
               CASE 
                  WHEN v_rch_tipo_mov.v_tpo_movimiento = "01"
                  LET v_regs_rechazados_alta = v_rch_tipo_mov.v_total_rechazos
               
                  WHEN v_rch_tipo_mov.v_tpo_movimiento = "06"
                  LET v_cambio_nom_rechazados = v_rch_tipo_mov.v_total_rechazos

                  WHEN v_rch_tipo_mov.v_tpo_movimiento = "08"
                  LET v_reingresos_rechazados = v_rch_tipo_mov.v_total_rechazos               
               END CASE 
            
               LET v_ind_rech = v_ind_rech + 1 
            END FOREACH
            
            -- se envia el encabezado
            OUTPUT TO REPORT rpt_cifras_control(p_folio                    , 
                                                p_usuario_cod              , 
                                                v_regs_procesados_alta     ,
                                                v_regs_aceptados_alta      ,
                                                v_regs_rechazados_alta     ,
                                                v_bajas_realizadas         ,
                                                v_cambios_nombre_realizados,
                                                v_regs_cambio_nss          ,
                                                v_nss_unificadores         ,
                                                v_nss_unificados           ,
                                                v_reingresos               ,
                                                v_bajas_procesadas         ,
                                                v_bajas_aceptadas          , --Se agrega aceptados/rechazados por tipo_mov
                                                v_bajas_rechazadas         ,
                                                v_cambio_nom_aceptados     ,
                                                v_cambio_nom_rechazados    ,
                                                v_cambio_nss_aceptados     ,
                                                v_cambio_nss_rechazados    ,
                                                v_reingresos_aceptados     ,
                                                v_reingresos_rechazados    , --Se agrega aceptados/rechazados por tipo_mov
                                                v_afi_rch_afiliatorio.*               
                                                )
            
            
            -- se finaliza
            FINISH REPORT rpt_cifras_control
            -- =====================================================================================
            
         ELSE
            -- no se pudo leer la plantilla del reporte
            DISPLAY "No se puede leer la plantilla del reporte AFIP01.4rp"       
         END IF
         }
         -- se finaliza la operacion      
         CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
                                     RETURNING r_bnd_fin_oper
               
         -- si no se pudo finalizar la operacion                      
         IF ( r_bnd_fin_oper <> 0 ) THEN
            DISPLAY "Ocurrió un error al finalizar la operación:"
            -- En caso de error se muestra un mensaje a usuario y no continua
            CALL fn_desplega_inc_operacion(r_bnd_fin_oper)
         END IF

      
      -- se envia la cadena que indica el fin de la etapa
      CALL fn_display_proceso(1, "ACTUALIZACION BDNSVIV-Afiliacion")
      
      LET p_titulo = "Finalización de operación - MOVIMIENTOS AFILIATORIOS - INTEGRACION"
      
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
Fecha creacion: 14 Agosto 2012
Autor: Ivan Vega
Narrativa del proceso que realiza:
Genera el reporte de cifras de control de carga inicial
 
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
============================================================================}
REPORT rpt_cifras_control(p_folio                    , 
                          p_usuario_cod              ,
                          p_regs_procesados_alta     ,
                          p_regs_aceptados_alta      ,
                          p_regs_rechazados_alta     ,
                          p_bajas_realizadas         ,
                          p_cambios_nombre_realizados,
                          p_regs_cambio_nss          ,
                          p_nss_unificadores         ,
                          p_nss_unificados           ,
                          p_reingresos               ,
                          p_bajas_procesadas         ,
                          p_bajas_aceptadas          , --Se agrega aceptados/rechazados por tipo_mov
                          p_bajas_rechazadas         ,
                          p_cambio_nom_aceptados     ,
                          p_cambio_nom_rechazados    ,
                          p_cambio_nss_aceptados     ,
                          p_cambio_nss_rechazados    ,
                          p_reingresos_aceptados     ,
                          p_reingresos_rechazados    , --Se agrega aceptados/rechazados por tipo_mov
                          p_afi_rch_afiliatorio
                          )

DEFINE p_usuario_cod                 LIKE seg_usuario.usuario_cod, -- Clave de usuario
       p_folio                       DECIMAL(9,0),
       -- cifras de control
       v_fecha_texto                 VARCHAR(10)
       ,p_regs_procesados_alta       INTEGER -- numero de movimientos procesados
       ,p_regs_aceptados_alta        INTEGER -- numero de movimientos aceptados
       ,p_regs_rechazados_alta       INTEGER -- numero de movimientos rechazados
       ,p_bajas_realizadas           INTEGER -- numero de bajas realizadas
       ,p_cambios_nombre_realizados  INTEGER -- numero de cambios de nombre realizados
       ,p_nss_unificadores           INTEGER -- numero de NSS unificadores
       ,p_nss_unificados             INTEGER -- numero de NSS unificados
       ,p_reingresos                 INTEGER -- numero de reingresos realizados
       ,p_afi_rch_afiliatorio        RECORD LIKE afi_rch_afiliatorio.* -- registro de afiliacion rechazado
       ,v_desc_rechazo               VARCHAR(250)       
       ,p_regs_cambio_nss            INTEGER  --total cambios NSS procesados
       ,p_bajas_procesadas           INTEGER  --total aceptadas  BAJA         
       ,p_bajas_aceptadas            INTEGER  --total aceptadas  BAJA         
       ,p_bajas_rechazadas           INTEGER  --total rechazadas BAJA         
       ,p_cambio_nom_aceptados       INTEGER  --total aceptados  CAMBIO NOMBRE
       ,p_cambio_nom_rechazados      INTEGER  --total rechazados CAMBIO NOMBRE
       ,p_cambio_nss_aceptados       INTEGER  --total aceptados  CAMBIO NSS   
       ,p_cambio_nss_rechazados      INTEGER  --total rechazados CAMBIO NSS   
       ,p_reingresos_aceptados       INTEGER  --total aceptados  REINGRESOS   
       ,p_reingresos_rechazados      INTEGER  --total rechazados REINGRESOS 
       
   FORMAT

      FIRST PAGE HEADER
         LET v_fecha_texto = TODAY USING "dd-mm-yyyy"
         -- se despliegan los datos del encabezado
         PRINTX p_folio
         PRINTX p_usuario_cod
         PRINTX v_fecha_texto
         PRINTX p_regs_procesados_alta     
         PRINTX p_regs_aceptados_alta      
         PRINTX p_regs_rechazados_alta     
         PRINTX p_bajas_realizadas         
         PRINTX p_cambios_nombre_realizados
         PRINTX p_regs_cambio_nss
         PRINTX p_nss_unificadores         
         PRINTX p_nss_unificados           
         PRINTX p_reingresos     
         PRINTX p_bajas_procesadas         
         PRINTX p_bajas_aceptadas          
         PRINTX p_bajas_rechazadas         
         PRINTX p_cambio_nom_aceptados     
         PRINTX p_cambio_nom_rechazados    
         PRINTX p_cambio_nss_aceptados     
         PRINTX p_cambio_nss_rechazados    
         PRINTX p_reingresos_aceptados     
         PRINTX p_reingresos_rechazados       

      BEFORE GROUP OF p_afi_rch_afiliatorio.cod_rechazo
         -- se obtiene la descripcion del rechazo
         SELECT des_rechazo
         INTO   v_desc_rechazo
         FROM   afi_cat_rch
         WHERE  cod_rechazo = p_afi_rch_afiliatorio.cod_rechazo

         -- se envia la descripcion del tipo de rechazo
         PRINTX p_afi_rch_afiliatorio.cod_rechazo, v_desc_rechazo
                
      ON EVERY ROW
         -- se envian los registros rechazados
         PRINTX p_afi_rch_afiliatorio.*
    
END REPORT
