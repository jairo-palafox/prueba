--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => PAG                                                                    #
#Programa     => PAGC83                                                                 #
#Objetivo     => Consulta de Extractor de precalificacion para menu                     #
#Autor        => GERARDO ALFONSO VEGA PAREDES                                           #
#Fecha inicio => 24 de Septiembre 2014                                                  #
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
          g_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
          v_sql            STRING -- cadena con instruccion SQL
          
END GLOBALS

MAIN

   DEFINE v_fecha_consulta DATE,    -- forma como ejecutara el programa
          p_s_titulo       STRING,  -- titulo de la ventana      
          p_tipo_ejecucion SMALLINT,
          v_folio_temp     DECIMAL(9,0),
          v_dato           CHAR(01)
          
   -- se recuperan los argumentos de la linea de comandos
   LET g_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)
   
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   -- se abre la ventana
   OPEN WINDOW w_consulta WITH FORM "PAGC811"
   
   -- se inicia el folio en null
   LET v_fecha_consulta = NULL
   
   INPUT v_fecha_consulta WITHOUT DEFAULTS
     FROM fecha_consulta
     ATTRIBUTES (UNBUFFERED)

         
      ON ACTION accept
         IF ( v_fecha_consulta IS NOT NULL ) THEN

            LET v_sql = "\n SELECT glo.folio         ",
                        "\n FROM   glo_folio glo, bat_ctr_operacion bat ",
                        "\n WHERE  date(bat.fecha_fin) = ","'",v_fecha_consulta,"'",
                        "\n AND    glo.folio  = bat.folio ",
                        "\n AND    glo.status = 2 ",
                        "\n AND    glo.proceso_cod IN (101,102,103,107,1401,1403) ", 
                        "\n AND    glo.proceso_cod = bat.proceso_cod ",
                        "\n AND    bat.opera_cod   = 4 "
                        
            PREPARE consulta_existe_fecha FROM v_sql
            DECLARE cur_consulta_fecha CURSOR FOR consulta_existe_fecha
   
            FOREACH cur_consulta_fecha INTO v_folio_temp
               EXIT FOREACH
            END FOREACH 

            LET v_dato = NULL
            SELECT "X" dato
            INTO   v_dato
            FROM   pag_extractor_preca
            WHERE  f_consulta = v_fecha_consulta
            GROUP BY dato

            IF (v_folio_temp IS NULL OR 0) AND v_dato IS NULL THEN
              CALL fn_mensaje("Atención","La fecha capturada no existe","stop")
            ELSE 
              CALL f_consulta_fecha(v_fecha_consulta)
            END IF
            
         ELSE
         
            CALL fn_mensaje("Atención","Debe capturar la Fecha de Consulta","stop")
            CONTINUE INPUT
         END IF

      ON ACTION cancel
         EXIT INPUT
   
   END INPUT   
   CLOSE WINDOW w_consulta

   
END MAIN

{ ======================================================================
Clave: PAGC83
Nombre: f_consulta_fecha
Fecha creacion: 24 Febrero 2014
Autor: Gerardo Vega
Narrativa del proceso que realiza:
Realiza una consulta sobre los datos de extracción para precalificación y 
emitir el resultado de la misma en un reporte

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
GERARDO VEGA    07-oct-2015       Se parametrizó subcuenta y movimientos con las tablas
                                  cat_sub_extractor y cat_mov_extractor
======================================================================
}

FUNCTION f_consulta_fecha(v_fecha_consulta)
   
   DEFINE v_fecha_consulta DATE
   
   DEFINE v_cifras RECORD
      origen    CHAR(08),
      registros DECIMAL(9,0),
      importe   DECIMAL(16,2)
   END RECORD

   DEFINE v_handler om.SaxDocumentHandler -- handler para el reporte
          
   DEFINE v_contador DECIMAL(9,0)
   
   -- Proesos 101=ENCLARA, 102= ACL SIN CAMBIO NSS, 103=ACL CAMBIO NSS, 
   --         107=ACL CAMBIO NOMBRE, 1401=LQINFO, 1403= SOLO INFONAVIT
   --         status = 2 significa que esta liquidado

   WHENEVER ERROR CONTINUE
   DROP TABLE tmp_folios_preca
   WHENEVER ERROR STOP

   LET v_sql = "CREATE TEMP TABLE tmp_folios_preca (folio DECIMAL(9,0))"
   PREPARE exe_crea_folio FROM v_sql
   EXECUTE exe_crea_folio
   
   LET v_sql = "\n INSERT INTO tmp_folios_preca ",
               "\n SELECT glo.folio folio ",
               "\n FROM   glo_folio glo, bat_ctr_operacion bat ",
               "\n WHERE  date(bat.fecha_fin) = ","'",v_fecha_consulta,"'",
               "\n AND    glo.folio  = bat.folio ",
               "\n AND    glo.status = 2 ", 
               "\n AND    glo.proceso_cod IN (101,102,103,107,1401,1403) ",
               "\n AND    glo.proceso_cod = bat.proceso_cod ",
               "\n AND    bat.opera_cod   = 4 " 

 DISPLAY v_sql              
               
   PREPARE exe_consulta_01 FROM v_sql
   EXECUTE exe_consulta_01             
   
   -- se indica que se usara la plantilla
   IF ( fgl_report_loadCurrentSettings("PAGI81.4rp") ) THEN
      LET v_handler = fgl_report_commitCurrentSettings()      -- commit the file settings
   ELSE
      CALL fn_mensaje("Error","No se encuentra la plantilla PAGC81.4rp. No se puede emitir el reporte","stop")
      RETURN
   END IF

   
   -- EXTRACCIÓN DE DATOS DE PAGOS DEL HISTORICO

   LET v_sql = "\n SELECT 'SACI'              origen,                     ",
               "\n        count(*)            registros,                  ",
               "\n        sum(det.imp_ap_pat) imp_ap_pat                  ",
               "\n FROM   cta_his_pagos det,                              ",
               "\n        cta_movimiento mov,                             ", 
               "\n        afi_derechohabiente afi,                        ",
               "\n        tmp_folios_preca glo                            ",
               "\n WHERE  det.folio = glo.folio                           ",
               "\n AND    det.folio = mov.folio_liquida                   ",
               "\n AND    det.id_referencia = mov.id_referencia           ",      
               "\n AND    det.ind_liquidacion <> -1                       ",         
               "\n AND    afi.id_derechohabiente = det.id_derechohabiente ",
               "\n AND    mov.subcuenta in (SELECT subcuenta   FROM cat_sub_extractor where vigencia_cod =1) ",  
               "\n AND    mov.movimiento in (SELECT movimiento FROM cat_mov_extractor where vigencia_cod =1) ",
               "\n AND    det.imp_ap_pat > 0                              ",
               "\n UNION ALL                                              ",
               "\n SELECT 'CONSULTA'    origen,                           ",
               "\n        count(*)      registros,                        ",
               "\n        sum(imp_pago) imp_ap_pat                        ",
               "\n FROM   pag_extractor_preca                             ",
               "\n WHERE  f_consulta = ","'",v_fecha_consulta,"'",
               "\n AND    cod_estado = 2                                  ",
               "\n AND    id_codigo = '00'                                "
              
   -- se prepara y ejecuta la consulta
   PREPARE sid_consulta_fecha2 FROM v_sql
   DECLARE cur_consulta_fecha2 CURSOR FOR sid_consulta_fecha2
   
   -- se inicia el contador
   LET v_contador = 0

   START REPORT rpt_consulta_lqinfo TO XML HANDLER v_handler   
   FOREACH cur_consulta_fecha2 INTO v_cifras.*
      OUTPUT TO REPORT rpt_consulta_lqinfo(v_cifras.*,v_fecha_consulta)
      LET v_contador = v_contador + 1
   END FOREACH
   FINISH REPORT rpt_consulta_lqinfo
   
END FUNCTION

{ ======================================================================
Clave: PAGC05
Nombre: rpt_consulta_lqinfo
Fecha creacion: Junio 20, 2012
Autor: Ivan Vega
Narrativa del proceso que realiza:
Genera el reporte de lqinfo

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}

REPORT rpt_consulta_lqinfo(v_cifras,v_fecha_consulta)

   DEFINE v_cifras RECORD
      origen    CHAR(08),
      registros DECIMAL(9,0),
      importe   DECIMAL(16,2)
   END RECORD
   
   DEFINE v_fecha_consulta DATE
   DEFINE v_fecha STRING
   DEFINE g_usuario_cod    LIKE seg_usuario.usuario_cod 
   DEFINE v_nombre_usuario VARCHAR(100)
         
   FORMAT

      FIRST PAGE HEADER
         
         -- se envia folio, usuario y fecha
         LET v_fecha = TODAY USING "dd-mm-yyyy"
         
         -- se obtiene el nombre del usuario
         SELECT USER
         INTO   g_usuario_cod
         FROM   seg_modulo
         WHERE  modulo_cod = "pag"
        
         SELECT usuario_desc
         INTO   v_nombre_usuario
         FROM   seg_usuario
         WHERE  usuario_cod = g_usuario_cod
         
         LET v_nombre_usuario = v_nombre_usuario CLIPPED
         
         PRINTX v_fecha_consulta, g_usuario_cod, v_nombre_usuario, v_fecha
         
    ON EVERY ROW

      IF v_cifras.importe IS NULL THEN
         LET v_cifras.importe = 0
      END IF

      IF v_cifras.registros IS NULL THEN
         LET v_cifras.registros = 0
      END IF
      
      PRINTX v_cifras.*
             
END REPORT   
             
             