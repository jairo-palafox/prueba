--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => PAG                                                                    #
#Programa     => PAGC33                                                                 #
#Objetivo     => Consulta de pagos por fortalecimiento al credito                       #
#Autor        => Ivan Vega                                                              #
#Fecha inicio => Junio 20, 2012                                                         #
#########################################################################################
DATABASE safre_viv
GLOBALS "PAGG01.4gl"  ---archivo de variables globales proceso_cod, opera_cod
GLOBALS
DEFINE g_pid            LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod    LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod      LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_reg_modulo     RECORD
        ruta_exp         CHAR(40),
        ruta_rescate     CHAR(40),
        ruta_listados    CHAR(40)
       END RECORD,
       seg_modulo_bat   RECORD
        ruta_listados   CHAR(40)
       END RECORD,
       g_usuario_cod        LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       
END GLOBALS

MAIN
DEFINE p_tipo_ejecucion     SMALLINT -- forma como ejecutara el programa
       ,p_s_titulo          STRING -- titulo de la ventana      
       ,v_fecha             DATE -- fecha de consulta
       
   -- se recuperan los argumentos de la linea de comandos
   LET g_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   -- se abre la ventana
   OPEN WINDOW w_consulta WITH FORM "PAGC331"
   
   -- se inicia el folio en null
   LET v_fecha = NULL
   
   INPUT v_fecha WITHOUT DEFAULTS
     FROM fecha
     ATTRIBUTES (UNBUFFERED)
   
      ON ACTION accept
         IF ( v_fecha IS NOT NULL ) THEN
            CALL f_consulta_pagos_fc(v_fecha)
         ELSE
            CALL fn_mensaje("Atención","Debe capturar una fecha","stop")
            CONTINUE INPUT
         END IF
         
      
      ON ACTION cancel
         EXIT INPUT
   
   END INPUT   
   CLOSE WINDOW w_consulta

   
END MAIN

{ ======================================================================
Clave: PAGC05
Nombre: f_consulta_pagos_fc
Fecha creacion: Junio 20, 2012
Autor: Ivan Vega
Narrativa del proceso que realiza:
Realiza una consulta de los pagos que existen de fortalecimiento al credito
con fecha de carga igual o anterior a la fecha recibida como parametro

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION f_consulta_pagos_fc(p_fecha)
DEFINE    p_fecha                 DATE, -- fecha de consulta
          v_r_pag_det_fc          RECORD LIKE pag_det_fc.*, -- registro de pagos por fortalecimiento al credito
          v_r_despliegue          RECORD -- registro de consulta
             num_regs              INTEGER      ,
             imp_ap_fc             DECIMAL(22,2)
          END RECORD,             
          v_arr_despliegue        DYNAMIC ARRAY OF RECORD -- arreglo de despliegue
             num_regs              INTEGER      ,
             mxn                   DECIMAL(22,2)
          END RECORD,
          v_contador              SMALLINT,
          v_sql                   STRING, -- cadena con instruccion SQL
          v_handler               om.SaxDocumentHandler, -- handler para el reporte
          v_archivo_salida        STRING,
          v_channel               base.Channel, -- para escritura del archivo
          v_ruta_envio            VARCHAR(40),
          v_cadena                STRING

   -- se obtienen cifras de control
   SELECT COUNT(*),
          SUM(imp_ap_fc)
   INTO   v_r_despliegue.*
   FROM   pag_det_fc
   WHERE  f_pago <= p_fecha

   -- se asignan al arreglo de despliegue
   LET v_arr_despliegue[1].* = v_r_despliegue.*
   
   -- se abre la ventana de despliegue de resultados
   OPEN WINDOW w_resultados WITH FORM "PAGC332"
   
   DISPLAY p_fecha TO fecha
   
   -- se inicia el nombre del archivo en nulo
   LET v_archivo_salida = NULL
   
   -- se despliegan los resultados
   DISPLAY ARRAY v_arr_despliegue TO tbl_despliegue.*
   ATTRIBUTES ( UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE )
   
      -- generar el archivo de salida
      ON ACTION generar
         -- se obtiene la ruta de listados
         SELECT ruta_envio
         INTO   v_ruta_envio
         FROM   seg_modulo
         WHERE  modulo_cod = "pag"
         
         LET v_ruta_envio = v_ruta_envio CLIPPED
         LET v_cadena = p_fecha USING "yyyymmdd"
         
         -- se construye la ruta del archivo de salida
         LET v_archivo_salida = v_ruta_envio, "/", v_cadena, ".extractor_fc"
         
         
         -- se prepara la consulta por fecha de pago
         LET v_sql = "\nSELECT folio      ,",
                     "\norigen_archivo    ,",
                     "\nid_referencia     ,",
                     "\nf_pago            ,",
                     "\nid_derechohabiente,",
                     "\nnss               ,",
                     "\nimp_ap_fc         ,",
                     "\nresult_operacion   ",
                     "\nFROM     pag_det_fc", -- tabla de detalle de fortalecimiento al credito
                     "\nWHERE    f_pago <= ?"
                        
         DISPLAY v_sql
         
         -- se prepara y ejecuta la consulta
         PREPARE sid_consulta_cc FROM v_sql
         DECLARE cur_consulta_cc CURSOR FOR sid_consulta_cc
         
         -- se inicia el contador
         LET v_contador = 1
         
         -- se inicia y abre el apuntador para escritura de archivo
         LET v_channel = base.Channel.create()
         CALL v_channel.setDelimiter("")
         
         CALL v_channel.openFile( v_archivo_salida, "w" )
         
         -- se transfieren los datos al arreglo de consulta
         FOREACH cur_consulta_cc USING p_fecha INTO v_r_pag_det_fc.*
            
            -- se escriben los registros al archivo
            LET v_cadena = v_r_pag_det_fc.nss,
                           v_r_pag_det_fc.f_pago USING "yyyymmdd",
                           (v_r_pag_det_fc.imp_ap_fc * 100) USING "&&&&&&&&"
                           
            CALL v_channel.writeLine(v_cadena)
            
            -- se incrementa el contador
            LET v_contador = v_contador + 1
            
         END FOREACH
         
         -- se muestra la ruta del archivo en la etiqueta
         DISPLAY v_archivo_salida TO lblrutaarchivo
         CALL ui.interface.refresh()
         
         CALL fn_mensaje("Atención","Se generó el archivo de salida. Puede obtenerlo mediante la consulta de archivos generados\nen el menú Consultas Generales del sistema","information")
         
      ON ACTION regresar
         EXIT DISPLAY
         
      ON ACTION reporte
         -- si aun no se ha generado el archivo, entonces no se puede emitir reporte de cifras de control
         IF ( v_archivo_salida IS NULL ) THEN
            CALL fn_mensaje("Atención","Aún no se ha generado el archivo. No es posible crear el reporte de cifras de control","stop")
            CONTINUE DISPLAY
         END IF
      
         -- se indica que se usara la plantilla
         IF ( fgl_report_loadCurrentSettings("PAGC33.4rp") ) THEN
            LET v_handler = fgl_report_commitCurrentSettings()      -- commit the file settings
         ELSE
            CALL fn_mensaje("Error","No se encuentra la plantilla PAGC33.4rp. No se puede emitir el reporte","stop")
            CONTINUE DISPLAY
         END IF
      
         -- se inicia la emision del reporte
         START REPORT rpt_consulta_cc TO XML HANDLER v_handler
         
         -- se transfieren los datos
         FOR v_contador = 1 TO v_arr_despliegue.getLength()         
            OUTPUT TO REPORT rpt_consulta_cc(p_fecha, v_archivo_salida, v_arr_despliegue[v_contador].*)
         END FOR
         
         -- se finaliza el reporte
         FINISH REPORT rpt_consulta_cc
   
   END DISPLAY
   
   CLOSE WINDOW w_resultados
END FUNCTION

{ ======================================================================
Clave: PAGC32
Nombre: rpt_consulta_cc
Fecha creacion: Junio 20, 2012
Autor: Ivan Vega
Narrativa del proceso que realiza:
Genera el reporte de cifras de control

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
REPORT rpt_consulta_cc(p_fecha_consulta, p_archivo_salida, v_r_despliegue)
DEFINE    v_r_despliegue          RECORD -- registro de consulta
             num_regs              INTEGER      ,
             imp_ap_fc             DECIMAL(22,2)
          END RECORD,             
          p_fecha_consulta           DATE, -- fecha de emision del reporte
          v_fecha_consulta_texto     STRING,
          v_fecha_emision            STRING,
          v_nombre_usuario           VARCHAR(100),
          v_tipo_registro            VARCHAR(20),
          v_nombre_archivo           VARCHAR(40),
          v_nombre_archivo_rechazos  STRING,
          v_ruta_envio               VARCHAR(40),
          v_archivo_rechazos         VARCHAR(100),
          -- variables para el totalizador
          v_total_regs               DECIMAL(12,2),
          v_total_mxn                DECIMAL(12,2),
          v_total_aivs               DECIMAL(12,2),
          v_folio_formato            VARCHAR(9),
          v_contador                 INTEGER,
          p_archivo_salida           STRING -- ruta del archivo creado

FORMAT

   FIRST PAGE HEADER      
      -- se obtiene el nombre del usuario
      SELECT usuario_desc
      INTO   v_nombre_usuario
      FROM   seg_usuario
      WHERE  usuario_cod = g_usuario_cod
      
      LET v_nombre_usuario = v_nombre_usuario CLIPPED

      -- se formatea la fecha de consulta
      LET v_fecha_consulta_texto = p_fecha_consulta USING "dd-mm-yyyy"
      
      -- la fecha de emision del reporte
      LET v_fecha_emision = TODAY USING "dd-mm-yyyy"
      
      PRINTX v_fecha_consulta_texto, g_usuario_cod, v_fecha_emision, v_nombre_usuario, p_archivo_salida
           

   ON EVERY ROW
      PRINTX v_r_despliegue.*

END REPORT   
             
             