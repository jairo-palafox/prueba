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
   DEFINE g_pid         LIKE bat_ctr_proceso.pid --  ID del proceso
   DEFINE g_proceso_cod LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
   DEFINE p_pid                          LIKE bat_ctr_operacion.pid -- PID del proceso
   DEFINE p_proceso_cod                  LIKE bat_ctr_operacion.proceso_cod -- codigo del proceso
   DEFINE p_opera_cod                    LIKE bat_ctr_operacion.opera_cod -- codigo de la operacion
   DEFINE p_usuario_cod                  LIKE seg_usuario.usuario_cod -- clave del usuario firmado
   DEFINE p_folio                        LIKE deo_preliquida.folio_liquida
   DEFINE p_nombre_archivo               LIKE glo_ctr_archivo.nombre_archivo
   DEFINE v_s_sql                        STRING -- cadena con una instruccion SQL
   DEFINE v_i_resultado                  INTEGER -- resultado del proceso
   DEFINE v_i_resultado_alta             SMALLINT -- resultado de la integracion de movimientos de alta
   DEFINE v_i_resultado_baja             SMALLINT -- resultado de la integracion de movimientos de baja
   DEFINE v_i_resultado_cambio_nombre    SMALLINT -- resultado de la integracion de movimientos de cambio de nombre
   DEFINE v_i_resultado_cambio_nss       SMALLINT -- resultado de la integracion de movimientos de nss (unificacion)
   DEFINE v_i_resultado_reingreso        SMALLINT -- resultado de la integracion de movimientos de reingreso
   DEFINE r_bnd_fin_oper                 SMALLINT
   DEFINE v_si_correcto_integra          SMALLINT
   DEFINE p_titulo                       STRING -- titulo del mensaje enviado en el correo
   DEFINE p_mensaje                      STRING -- cuerpo del mensaje enviado
   DEFINE v_error_isam                   INTEGER
   DEFINE v_mensaje                      VARCHAR(250)
   DEFINE v_mensaje_alta                 VARCHAR(250) -- mensaje devuelto por la integracion de alta
   DEFINE v_mensaje_baja                 VARCHAR(250) -- mensaje devuelto por la integracion de baja
   DEFINE v_mensaje_cambio_nombre        VARCHAR(250) -- mensaje devuelto por la integracion de cambio de nombre
   DEFINE v_mensaje_cambio_nss           VARCHAR(250) -- mensaje devuelto por la integracion de cambio de nss
   DEFINE v_mensaje_reingreso            VARCHAR(250) -- mensaje devuelto por la integracion de reingreso
   DEFINE v_cambios_realizados           INTEGER -- numero de operaciones 75 realizadas

   -- variables para el reporte de integracion
   DEFINE p_programa_cod          VARCHAR(10)
   DEFINE p_modulo_cod            VARCHAR(3)
   DEFINE v_ruta_reporte          STRING -- ruta del archivo del reporte
   DEFINE v_ruta_listados         VARCHAR(40) -- ruta de los listados
   DEFINE v_ruta_ejecutable       STRING -- ruta del ejecutable
   DEFINE report_handler          om.SaxDocumentHandler -- handler para el reporte en PDF
   DEFINE v_nombre_usuario        CHAR (40)
   DEFINE v_usuario_cod_temp      CHAR (20)

   -- cifras de control
   DEFINE v_registros_totales     INTEGER
   DEFINE v_regs_aceptados        INTEGER
   DEFINE v_regs_rechazados       INTEGER

   -- registros rechazados
   DEFINE v_afi_rch_afiliatorio   RECORD LIKE afi_rch_afiliatorio.*
   DEFINE v_comando               STRING


   
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

   -- se inicializa el contador de los rechazos
   LET v_regs_rechazados = 0

   -- se envia la cadena que indica el inicio de etapa
   CALL fn_display_proceso(0, "INTEGRACION")
   
   -- se contruye el enuncionado SQL
   LET v_s_sql = "EXECUTE FUNCTION fn_afi_integra_opt75(?,?,?,?,?)"
   
   -- se prepara la ejecucion del stored procedure para la integracion
   PREPARE sid_afi_op75 FROM v_s_sql
   
   -- se ejecuta el stored procedure
   EXECUTE sid_afi_op75 USING p_usuario_cod, p_folio, p_nombre_archivo, g_pid, p_proceso_cod
      INTO v_i_resultado, v_error_isam, v_mensaje, v_cambios_realizados,v_regs_rechazados

   -- se muestra el mensaje de la prevalidacion
   DISPLAY v_mensaje

   -- si el resultado fue correcto
   IF ( v_i_resultado <> 0 ) THEN
      DISPLAY "No se puede continuar con la integración..."
      DISPLAY "Error - ", v_i_resultado
      DISPLAY "Mensaje: ", v_mensaje
      
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
                      
      LET p_titulo = "Finalización de operación - MOVIMIENTOS AFILIATORIOS - INTEGRACION"
      
      -- se invoca el envio de correo electronico de notificacion
      CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                             NULL, -- no lleva archivo adjunto
                             p_titulo,
                             p_mensaje)
   ELSE              

      LET p_mensaje = "ID Proceso   : ", g_pid, "\n", 
                      "Proceso      : MOVIMIENTOS AFILIATORIOS\n",
                      "Operación    : INTEGRACIÓN\n",
                      "Fecha Inicio : ", TODAY, "\n",
                      "Fecha Fin    : ", TODAY, "\n",
                      "El folio asociado a su operación es: ", p_folio, "\n"
      
      DISPLAY "=========================================================================="
      DISPLAY "                    RESULTADOS DE LA INTEGRACION"
      DISPLAY "=========================================================================="      
     
               -- se complementa el mensaje
         LET p_mensaje = p_mensaje || "Operación 75 realizada con éxito\n."
         DISPLAY "OPERACIÓN 75 realizada con éxito.\n"
         DISPLAY "Movimientos aceptados: ", v_cambios_realizados

         -- se finaliza la operacion      
         CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
                                     RETURNING r_bnd_fin_oper
               
         -- si no se pudo finalizar la operacion                      
         IF ( r_bnd_fin_oper <> 0 ) THEN
            DISPLAY "Ocurrió un error al finalizar la operación:"
            -- En caso de error se muestra un mensaje a usuario y no continua
            CALL fn_desplega_inc_operacion(r_bnd_fin_oper)
         END IF








         {Ejecución del programa que genera un archivo de salida con los rechazoz}
         SELECT ruta_bin
         INTO   v_ruta_listados
         FROM   seg_modulo
         WHERE  modulo_cod = "afi"

         LET v_comando = "fglrun ", v_ruta_listados CLIPPED, "/AFIS04.42r ", p_usuario_cod, " ", p_folio, " ", g_proceso_cod

         -- se ejecuta el programa que genera el archivo con los rechazos
         RUN v_comando







         
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
                        "AFIP03-"                   , -- programa
                        g_pid           USING "&&&&&","-", -- PID
                        g_proceso_cod   USING "&&&&&", "-", -- codigo del proceso
                        g_opera_cod     USING "&&&&&",".pdf" -- codigo de la operación

    DISPLAY "Ruta del reporte: ", v_ruta_reporte

    -- se indica que el reporte usara la plantilla creada
    IF ( fgl_report_loadCurrentSettings("../../afi/bin/AFIP03.4rp") ) THEN  -- if  the file loaded OK

       -- sin preview
       CALL fgl_report_selectPreview(0)
       -- se indica que se escriba en archivo
       CALL fgl_report_setOutputFileName(v_ruta_reporte)       

       LET report_handler = fgl_report_commitCurrentSettings()      -- commit the file settings

    -- se obtiene el total de registros
    SELECT COUNT(*)
    INTO   v_registros_totales
    FROM   safre_tmp:tmp_afi_det_op75

    -- se inicia el registro de rechazos
    INITIALIZE v_afi_rch_afiliatorio.* TO NULL
    LET v_afi_rch_afiliatorio.cod_rechazo = -99
   
    -- se inicia el reporte
    START REPORT rpt_cifras_control TO XML HANDLER report_handler
    
    -- se envian los datos al reporte
    OUTPUT TO REPORT rpt_cifras_control(p_folio               , 
                                        p_usuario_cod         , 
                                        v_cambios_realizados  ,
                                        v_registros_totales   ,
                                        v_regs_rechazados
                                        )

    -- se leen los rechazos
    DECLARE cur_rechazos CURSOR FOR
    SELECT *
    FROM   afi_rch_afiliatorio
    WHERE  folio_lote = p_folio

    FOREACH cur_rechazos INTO v_afi_rch_afiliatorio.*
       -- se envian los datos al reporte
       OUTPUT TO REPORT rpt_cifras_control(p_folio               , 
                                           p_usuario_cod         , 
                                           v_cambios_realizados  ,
                                           v_registros_totales,
                                           v_regs_rechazados
                                           )

    END FOREACH
                                        
    -- se finaliza
    FINISH REPORT rpt_cifras_control
   -- =====================================================================================

    ELSE
       DISPLAY "No se puede leer la plantilla del reporte AFIP03.4rp"       
    END IF


      -- se envia la cadena que indica el fin de la etapa
      CALL fn_display_proceso(1, "INTEGRACION")
      
      LET p_titulo = "Finalización de operación - MOVIMIENTOS AFILIATORIOS OPERACIÓN 75 - INTEGRACION"
      
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
Genera el reporte de cifras de control de operacion 75
 
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
============================================================================}
REPORT rpt_cifras_control(p_folio               , 
                          p_usuario_cod         ,
                          p_regs_aceptados      ,
                          p_regs_totales        ,
                          p_regs_rechazados
                          )

DEFINE p_usuario_cod            LIKE seg_usuario.usuario_cod -- Clave de usuario
DEFINE p_folio                  DECIMAL(9,0)
       -- cifras de control
DEFINE v_fecha_texto            VARCHAR(10)
DEFINE  p_regs_aceptados        INTEGER -- numero de movimientos aceptados
DEFINE  p_regs_totales          INTEGER -- numero de movimientos rechazados
DEFINE  p_regs_rechazados       INTEGER
       --,p_afi_rch_afiliatorio   RECORD LIKE afi_rch_afiliatorio.*
DEFINE  v_desc_rechazo          VARCHAR(250)

   FORMAT

      FIRST PAGE HEADER
         LET v_fecha_texto = TODAY USING "dd-mm-yyyy"
         -- se despliegan los datos del encabezado
         PRINTX p_folio, p_usuario_cod, v_fecha_texto
         PRINTX p_regs_aceptados,
                p_regs_totales,
                p_regs_rechazados
         
      --BEFORE GROUP OF p_afi_rch_afiliatorio.cod_rechazo
         -- se obtiene la descripcion del rechazo
--         SELECT des_rechazo
--           INTO   v_desc_rechazo
--           FROM   afi_cat_rch
--          WHERE  cod_rechazo = p_afi_rch_afiliatorio.cod_rechazo

         -- se envia la descripcion del tipo de rechazo
        -- PRINTX p_afi_rch_afiliatorio.cod_rechazo, v_desc_rechazo
                
      ON EVERY ROW
         -- se envian los registros rechazados
         --PRINTX p_afi_rch_afiliatorio.*

    
END REPORT