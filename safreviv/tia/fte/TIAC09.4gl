--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => TIA                                                                    #
#Programa     => TIAC09                                                                 #
#Objetivo     => Consulta Dinamica de Traspasos IA para Contabilidad                    #
#Autor        => Ivan Vega                                                              #
#Fecha inicio => Abril 07, 2014                                                         #
#########################################################################################
DATABASE safre_viv
GLOBALS "TIAG01.4gl" --archivo de variables globales proceso_cod
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
       END RECORD

END GLOBALS

MAIN
DEFINE  p_usuario_cod        LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,p_tipo_ejecucion     SMALLINT                     -- forma como ejecutara el programa
       ,p_s_titulo           STRING                       -- titulo de la ventana
       ,v_c_ruta_bin_acr            LIKE seg_modulo.ruta_bin -- ruta del bin de acr
       ,v_c_ruta_list_bat           LIKE seg_modulo.ruta_listados -- ruta listados de bat
       ,v_ruta_vacia                STRING
       ,v_mes_inicio                SMALLINT -- para captura de periodo de consulta
       ,v_mes_fin                   SMALLINT
       ,v_ano_inicio                SMALLINT
       ,v_ano_fin                   SMALLINT
       ,v_combo_mes_inicio          ui.ComboBox
       ,v_combo_mes_fin             ui.ComboBox
       ,v_combo_ano_inicio          ui.ComboBox
       ,v_combo_ano_fin             ui.ComboBox
       ,v_result_operacion          CHAR(2) -- tipo de registros
        
   -- se recuperan los argumentos
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   --llamad a funciones generales que regresan las rutas para generación de archvivos
   CALL fn_rutas("tia") RETURNING v_c_ruta_bin_acr, v_ruta_vacia
   CALL fn_rutas("bat") RETURNING v_ruta_vacia, v_c_ruta_list_bat

   -- se asume que la fecha de consulta es la del dia
   LET v_mes_inicio = MONTH(TODAY)
   LET v_ano_inicio = YEAR(TODAY)  
   LET v_mes_fin    = MONTH(TODAY)
   LET v_ano_fin    = YEAR(TODAY)

   -- abre la ventana para desplegar los registros encontrados
   OPEN WINDOW w_consulta_tia  WITH FORM "TIAC091"
     
   -- se le asigna el apuntado del combo a la variable
   LET v_combo_mes_inicio = ui.ComboBox.forName("formonly.v_mes_inicio")
   LET v_combo_mes_fin    = ui.ComboBox.forName("formonly.v_mes_fin")

   -- se llenan los combos de mes
   CALL v_combo_mes_inicio.clear()
   CALL v_combo_mes_inicio.addItem("1","Enero")
   CALL v_combo_mes_inicio.addItem("2","Febrero")
   CALL v_combo_mes_inicio.addItem("3","Marzo")
   CALL v_combo_mes_inicio.addItem("4","Abril")
   CALL v_combo_mes_inicio.addItem("5","Mayo")
   CALL v_combo_mes_inicio.addItem("6","Junio")
   CALL v_combo_mes_inicio.addItem("7","Julio")
   CALL v_combo_mes_inicio.addItem("8","Agosto")
   CALL v_combo_mes_inicio.addItem("9","Septiembre")
   CALL v_combo_mes_inicio.addItem("10","Octubre")
   CALL v_combo_mes_inicio.addItem("11","Noviembre")
   CALL v_combo_mes_inicio.addItem("12","Diciembre")

   CALL v_combo_mes_fin.clear()
   CALL v_combo_mes_fin.addItem("1","Enero")
   CALL v_combo_mes_fin.addItem("2","Febrero")
   CALL v_combo_mes_fin.addItem("3","Marzo")
   CALL v_combo_mes_fin.addItem("4","Abril")
   CALL v_combo_mes_fin.addItem("5","Mayo")
   CALL v_combo_mes_fin.addItem("6","Junio")
   CALL v_combo_mes_fin.addItem("7","Julio")
   CALL v_combo_mes_fin.addItem("8","Agosto")
   CALL v_combo_mes_fin.addItem("9","Septiembre")
   CALL v_combo_mes_fin.addItem("10","Octubre")
   CALL v_combo_mes_fin.addItem("11","Noviembre")
   CALL v_combo_mes_fin.addItem("12","Diciembre")
     

   -- se inician los combos de ano
   LET v_combo_ano_inicio = ui.ComboBox.forName("formonly.v_ano_inicio")
   LET v_combo_ano_fin    = ui.ComboBox.forName("formonly.v_ano_fin")

   CALL v_combo_ano_inicio.clear()
   CALL v_combo_ano_fin.clear()
    
   FOR v_ano_inicio = 2012 TO YEAR(TODAY)
      CALL v_combo_ano_inicio.addItem(v_ano_inicio,v_ano_inicio)
      CALL v_combo_ano_fin.addItem(v_ano_inicio,v_ano_inicio)
   END FOR

   LET v_ano_inicio = YEAR(TODAY)
   LET v_ano_fin    = YEAR(TODAY)

   -- por omision, se eligen todos los registros
   LET v_result_operacion = "00"

   -- se capturan los valores de consulta
   INPUT BY NAME v_mes_inicio, v_ano_inicio, v_mes_fin, v_ano_fin, v_result_operacion
   WITHOUT DEFAULTS
   ATTRIBUTES (UNBUFFERED)
        
      ON ACTION CANCEL
         EXIT INPUT

      ON ACTION ACCEPT
         -- se invoca la consulta dinamica con el periodo inicial y final
         CALL f_consulta_dinamica(v_mes_inicio, v_ano_inicio, v_mes_fin, v_ano_fin, p_usuario_cod, v_result_operacion)
         
   END INPUT
   
   CLOSE WINDOW w_consulta_tia  

END MAIN


{===========================================================================
Nombre: f_consulta_dinamica
Fecha creacion: 14 Abril 2014
Autor: Ivan Vega
Narrativa del proceso que realiza:
 Genera un reporte dinamico con respecto al periodo de consulta conformado
 por un mes y ano inicial y, un mes y ano final
 
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
============================================================================}
FUNCTION f_consulta_dinamica(v_mes_inicio, v_ano_inicio, v_mes_fin, v_ano_fin, p_usuario_cod, p_result_operacion)
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod,
       v_registro_cifra_global      RECORD             --record de registros
         periodo                      STRING, 
         registros_cargados           INTEGER,
         saldo_viv92_cargados         DECIMAL(22,2),
         aivs_viv92_cargados          DECIMAL(24,6),
         registros_liquidados         INTEGER,
         saldo_viv92_liquidados       DECIMAL(22,2),
         aivs_viv92_liquidados        DECIMAL(24,6),
         registros_incidencia         INTEGER,
         saldo_viv92_incidencia       DECIMAL(22,2),
         aivs_viv92_incidencia        DECIMAL(24,6)
       END RECORD,
       v_registro_cifra_detalle     RECORD             --record de registros
         consec_cuenta                LIKE afi_decreto.id_decreto,
         f_presentacion               LIKE tia_det_traspaso.f_presentacion,
         saldo_viv92_cargado          LIKE tia_det_traspaso.sdo_viv92,
         aivs_viv92_cargado           LIKE tia_det_traspaso.aivs_viv92,
         saldo_viv92_liquidado        LIKE tia_det_traspaso.sdo_viv92,
         aivs_viv92_liquidado         LIKE tia_det_traspaso.aivs_viv92,
         saldo_viv92_incidencia       LIKE tia_det_traspaso.sdo_viv92,
         aivs_viv92_incidencia        LIKE tia_det_traspaso.aivs_viv92
       END RECORD,
       v_registro        RECORD             --record de registros 
         concepto          STRING,
         registros         INTEGER,
         saldo_viv92       DECIMAL(22,2),
         aivs_viv92        DECIMAL(24,6)
       END RECORD,
       p_result_operacion CHAR(2), -- recibido de parametro
       manejador_rpt      om.SaxDocumentHandler,
       v_mes_inicio       SMALLINT, -- para captura de periodo de consulta
       v_mes_fin          SMALLINT,
       v_ano_inicio        SMALLINT,
       v_ano_fin          SMALLINT,
       v_periodo_inicio   STRING,
       v_periodo_final    STRING,
       v_cadena_aux       STRING,
       v_fecha_inicio      DATE,
       v_fecha_fin        DATE,
       v_f_presentacion   LIKE tia_cza_traspaso.f_presentacion, -- fecha del archivo
       v_result_operacion CHAR(2), -- obtenido de consulta
       v_sql              STRING, -- cadena con una instruccion SQL
       v_folio            LIKE glo_folio.folio, -- folio de liquidacion
       v_tipo_registro    SMALLINT, -- 1 global, 2 detalle
       v_nombre_archivo   LIKE glo_ctr_archivo.nombre_archivo,
       v_f_liquidacion    DATE, -- fecha de liquidacion del folio
       v_consec_cuenta    LIKE afi_decreto.consec_cuenta, -- consecutivo cuenta de Decreto para reporte
       v_result_operacion_aux CHAR(2),
       v_registro_detalle  RECORD             --record de registros 
         consec_cuenta      LIKE afi_decreto.id_decreto,
         saldo_viv92        LIKE tia_det_traspaso.sdo_viv92,
         aivs_viv92         LIKE tia_det_traspaso.aivs_viv92,
         result_operacion   LIKE tia_det_traspaso.result_operacion
       END RECORD,
       li_dia_fin        SMALLINT
       
   -- se calcula el dia fin segun el mes fin
   CASE v_mes_fin
     WHEN 1
       LET li_dia_fin = 31

     WHEN 2
       LET li_dia_fin = 28
       
     WHEN 3
       LET li_dia_fin = 31

     WHEN 4
       LET li_dia_fin = 30
       
     WHEN 5
       LET li_dia_fin = 31

     WHEN 6
       LET li_dia_fin = 30
       
     WHEN 7
       LET li_dia_fin = 31

     WHEN 8
       LET li_dia_fin = 31

     WHEN 9
       LET li_dia_fin = 30

     WHEN 10
       LET li_dia_fin = 31
       
     WHEN 11
       LET li_dia_fin = 30

     WHEN 12
       LET li_dia_fin = 31
   END CASE

   -- se construyen las cadenas de periodo inicial y final
   CALL fn_mes_texto(v_mes_inicio) RETURNING v_cadena_aux
   LET v_periodo_inicio = v_cadena_aux, "-", v_ano_inicio USING "&&&&"

   CALL fn_mes_texto(v_mes_fin) RETURNING v_cadena_aux
   LET v_periodo_final = v_cadena_aux, "-", v_ano_fin USING "&&&&"

   -- se generan las fechas de consulta
   LET v_cadena_aux = v_mes_inicio USING "&&", "/01/", v_ano_inicio USING "&&&&"
   LET v_fecha_inicio = DATE(v_cadena_aux)

   LET v_cadena_aux = v_mes_fin USING "&&", "/", li_dia_fin USING "&&", "/", v_ano_fin USING "&&&&"
   LET v_fecha_fin = DATE(v_cadena_aux)   

   LET v_registro_cifra_global.periodo                = "PERIODO AQUI"
   LET v_registro_cifra_global.registros_cargados     = 0
   LET v_registro_cifra_global.saldo_viv92_cargados   = 0
   LET v_registro_cifra_global.aivs_viv92_cargados    = 0
   LET v_registro_cifra_global.registros_liquidados   = 0
   LET v_registro_cifra_global.saldo_viv92_liquidados = 0
   LET v_registro_cifra_global.aivs_viv92_liquidados  = 0
   LET v_registro_cifra_global.registros_incidencia   = 0
   LET v_registro_cifra_global.saldo_viv92_incidencia = 0
   LET v_registro_cifra_global.aivs_viv92_incidencia  = 0

   LET v_registro.concepto    = "concepto"
   LET v_registro.registros   = 0
   LET v_registro.saldo_viv92 = 0
   LET v_registro.aivs_viv92  = 0

   -- Se asigna la plantilla para generar el reporte
   IF ( fgl_report_loadCurrentSettings("TIAC09.4rp") ) THEN 
      CALL fgl_report_selectDevice ("PDF")
                    
      LET manejador_rpt = fgl_report_commitCurrentSettings()

      -- se inicia el reporte 
      START REPORT rpt_reporte_tia_contabilidad TO XML HANDLER manejador_rpt

      -- ========================================================================================
      -- ========================================================================================
      --               S E C C I O N   D E   C I F R A S   G L O B A L E S 
      -- ========================================================================================
      -- ========================================================================================
      DISPLAY "--- EMITIENDO SECCION DE CIFRAS GLOBALES ---"
      -- se obtienen las n diferentes fechas de presentacion segun el intervalo y sus folios
      DECLARE cur_fechas CURSOR FOR
      SELECT DISTINCT folio, f_presentacion
      FROM   tia_det_traspaso
      WHERE  f_presentacion BETWEEN v_fecha_inicio AND v_fecha_fin
      ORDER BY f_presentacion

      -- para cada folio - fecha que entra en el periodo      
      FOREACH cur_fechas INTO v_folio, v_f_presentacion
      
         -- se construye el periodo
         LET v_cadena_aux = fn_mes_texto(MONTH(v_f_presentacion)), " ", YEAR(v_f_presentacion)
         LET v_registro_cifra_global.periodo                = v_cadena_aux
         
         -- se obtiene el nombre del archivo
         SELECT nombre_archivo
         INTO   v_nombre_archivo
         FROM   glo_ctr_archivo
         WHERE  folio = v_folio
         
         -- se obtiene la fecha de liquidacion
         SELECT f_actualiza
         INTO   v_f_liquidacion
         FROM   glo_folio
         WHERE  folio = v_folio
      
         -- los registros cargados siempre van
         SELECT count(*)        , 
                sum(sdo_viv92)  , 
                sum(aivs_viv92)   
         INTO   v_registro.registros  ,
                v_registro.saldo_viv92, 
                v_registro.aivs_viv92
         FROM   tia_det_traspaso  
         WHERE  folio = v_folio

         -- todos los registros entran a cargados
         LET v_registro_cifra_global.registros_cargados     = v_registro.registros
         LET v_registro_cifra_global.saldo_viv92_cargados   = v_registro.saldo_viv92
         LET v_registro_cifra_global.aivs_viv92_cargados    = v_registro.aivs_viv92
                             
         -- registros aceptados
         SELECT count(*)        , 
                sum(sdo_viv92)  , 
                sum(aivs_viv92)   
         INTO   v_registro.registros  ,
                v_registro.saldo_viv92, 
                v_registro.aivs_viv92
         FROM   tia_det_traspaso  
         WHERE  folio = v_folio
         AND    result_operacion = "01"
         
         LET v_registro_cifra_global.registros_liquidados   = v_registro.registros
         LET v_registro_cifra_global.saldo_viv92_liquidados = v_registro.saldo_viv92
         LET v_registro_cifra_global.aivs_viv92_liquidados  = v_registro.aivs_viv92
         
         -- si se solicito rechazados
         SELECT count(*)        , 
                sum(sdo_viv92)  , 
                sum(aivs_viv92)   
         INTO   v_registro.registros  ,
                v_registro.saldo_viv92, 
                v_registro.aivs_viv92
         FROM   tia_det_traspaso  
         WHERE  folio = v_folio
         AND    result_operacion <> "01"
         
         LET v_registro_cifra_global.registros_incidencia   = v_registro.registros
         LET v_registro_cifra_global.saldo_viv92_incidencia = v_registro.saldo_viv92
         LET v_registro_cifra_global.aivs_viv92_incidencia  = v_registro.aivs_viv92
          
         -- se indica que se envia registro global
         LET v_tipo_registro = 1
                  -- se las cifras globales al reporte
         OUTPUT TO REPORT rpt_reporte_tia_contabilidad(v_periodo_inicio, v_periodo_final, p_usuario_cod, v_registro_cifra_detalle.*, v_registro_cifra_global.*, v_tipo_registro, v_nombre_archivo, v_f_liquidacion)

      END FOREACH


      -- ========================================================================================
      -- ========================================================================================
      --              S E C C I O N   D E   R E G I S T R O S   D E T A L L A D O S
      -- ========================================================================================
      -- ========================================================================================
      -- se obtienen las n diferentes fechas de presentacion segun el intervalo y sus folios
      DISPLAY "--- EMITIENDO SECCION DE CIFRAS DETALLADAS ---"
      
      DECLARE cur_fechas_det CURSOR FOR
      SELECT DISTINCT folio, f_presentacion
      FROM   tia_det_traspaso
      WHERE  f_presentacion BETWEEN v_fecha_inicio AND v_fecha_fin
      ORDER BY f_presentacion

      -- para cada folio - fecha que entra en el periodo      
      FOREACH cur_fechas_det INTO v_folio, v_f_presentacion
      
         -- se construye el periodo
         LET v_cadena_aux = fn_mes_texto(MONTH(v_f_presentacion)), " ", YEAR(v_f_presentacion)
         LET v_registro_cifra_global.periodo = v_cadena_aux
         
         -- se obtiene el nombre del archivo
         SELECT nombre_archivo
         INTO   v_nombre_archivo
         FROM   glo_ctr_archivo
         WHERE  folio = v_folio
         
         -- se obtiene la fecha de liquidacion
         SELECT f_actualiza
         INTO   v_f_liquidacion
         FROM   glo_folio
         WHERE  folio = v_folio
      
         -- los registros cargados siempre van
         DECLARE cur_registros_detalle CURSOR FOR
         SELECT a.consec_cuenta   , 
                b.sdo_viv92       , 
                b.aivs_viv92      ,
                b.result_operacion
         FROM   tia_det_traspaso b,
                afi_decreto a  
         WHERE  b.folio      = v_folio
         AND    b.id_decreto = a.id_decreto

         -- para cada registro del folio
         FOREACH cur_registros_detalle INTO v_registro_detalle.*

            -- el registro siempre se cuenta en los cargados
            LET v_registro_cifra_detalle.consec_cuenta          = v_registro_detalle.consec_cuenta
            LET v_registro_cifra_detalle.f_presentacion         = v_f_presentacion
            LET v_registro_cifra_detalle.saldo_viv92_cargado    = v_registro_detalle.saldo_viv92
            LET v_registro_cifra_detalle.aivs_viv92_cargado     = v_registro_detalle.aivs_viv92

            -- si esta aceptado
            IF ( v_registro_detalle.result_operacion = "01" ) THEN
               LET v_registro_cifra_detalle.saldo_viv92_liquidado  = v_registro_detalle.saldo_viv92 
               LET v_registro_cifra_detalle.aivs_viv92_liquidado   = v_registro_detalle.aivs_viv92
            ELSE
               LET v_registro_cifra_detalle.saldo_viv92_liquidado  = 0
               LET v_registro_cifra_detalle.aivs_viv92_liquidado   = 0
            END IF

            -- si no fue aceptado
            IF ( v_registro_detalle.result_operacion <> "01" ) THEN
               LET v_registro_cifra_detalle.saldo_viv92_incidencia = v_registro_detalle.saldo_viv92 
               LET v_registro_cifra_detalle.aivs_viv92_incidencia  = v_registro_detalle.aivs_viv92
            ELSE
               LET v_registro_cifra_detalle.saldo_viv92_incidencia = 0
               LET v_registro_cifra_detalle.aivs_viv92_incidencia  = 0
            END IF

            -- se indica que se envia registro detallado
            LET v_tipo_registro = 2

            -- se las cifras globales al reporte
            OUTPUT TO REPORT rpt_reporte_tia_contabilidad(v_periodo_inicio, v_periodo_final, p_usuario_cod, v_registro_cifra_detalle.*, v_registro_cifra_global.*, v_tipo_registro, v_nombre_archivo, v_f_liquidacion)
         END FOREACH

      END FOREACH

      -- se finaliza el reporte
      FINISH REPORT rpt_reporte_tia_contabilidad
   ELSE         
      CALL fn_mensaje("Atención","No se pudo abrir la plantilla TIAC08.4rp.\nNo es posible generar el reporte","stop")
      RETURN 
   END IF

END FUNCTION

{===========================================================================
Nombre: rpt_reporte_tia_contabilidad
Fecha creacion: 21 Febrero 2013
Autor: Ivan Vega
Narrativa del proceso que realiza:
 Reporte de NSS no encontrados en vivienda
 para el folio dado
 
Parametros de Entrada:
 - p_folio: folio del proceso
 - r_registro: registro de NSS no encontrado en vivienda
Parámetros de salida:
 - Ninguno
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
============================================================================}
REPORT rpt_reporte_tia_contabilidad(v_periodo_inicio, v_periodo_final, p_usuario_cod, v_registro_cifra_detalle, v_registro_cifra_global, p_tipo_registro, p_nombre_archivo, p_f_liquidacion)
DEFINE p_usuario_cod         LIKE seg_usuario.usuario_cod,
       p_nombre_usuario        LIKE seg_usuario.usuario_desc,
       v_registro_cifra_detalle     RECORD             --record de registros
         consec_cuenta                LIKE afi_decreto.id_decreto,
         f_presentacion               LIKE tia_det_traspaso.f_presentacion,
         saldo_viv92_cargado          LIKE tia_det_traspaso.sdo_viv92,
         aivs_viv92_cargado           LIKE tia_det_traspaso.aivs_viv92,
         saldo_viv92_liquidado        LIKE tia_det_traspaso.sdo_viv92,
         aivs_viv92_liquidado         LIKE tia_det_traspaso.aivs_viv92,
         saldo_viv92_incidencia       LIKE tia_det_traspaso.sdo_viv92,
         aivs_viv92_incidencia        LIKE tia_det_traspaso.aivs_viv92
       END RECORD,
       v_registro_cifra_global      RECORD             --record de registros
         periodo                      STRING, 
         registros_cargados           INTEGER,
         saldo_viv92_cargados         DECIMAL(22,2),
         aivs_viv92_cargados          DECIMAL(24,6),
         registros_liquidados         INTEGER,
         saldo_viv92_liquidados       DECIMAL(22,2),
         aivs_viv92_liquidados        DECIMAL(24,6),
         registros_incidencia         INTEGER,
         saldo_viv92_incidencia       DECIMAL(22,2),
         aivs_viv92_incidencia        DECIMAL(24,6)
       END RECORD,
       p_tipo_registro         SMALLINT,
       v_consec_cuenta         LIKE afi_decreto.consec_cuenta,
       v_fecha_reporte         STRING,
       v_periodo_inicio        STRING,
       v_periodo_final         STRING,
       p_nombre_archivo        STRING, -- nombre del archivo cargado
       p_f_liquidacion         DATE,
       v_fecha_cadena          STRING,
       v_fecha_cadena_det      STRING

   FORMAT

      FIRST PAGE HEADER
         -- se obtiene el nombre del usuario
         SELECT usuario_desc
         INTO p_nombre_usuario
         FROM seg_usuario
         WHERE usuario_cod = p_usuario_cod

         -- la fecha del reporte
         LET v_fecha_reporte = TODAY USING "dd-mm-yyyy"
         
         -- se imprime el folio, codigo de usuario y su nombre
         PRINTX v_periodo_inicio, v_periodo_final, p_usuario_cod, p_nombre_usuario, v_fecha_reporte

      --BEFORE GROUP OF v_registro_cifra_detalle.f_presentacion
      BEFORE GROUP OF p_nombre_archivo
         PRINTX p_nombre_archivo
         LET v_fecha_cadena_det = fn_mes_texto(MONTH(v_registro_cifra_detalle.f_presentacion)), " - ", YEAR(v_registro_cifra_detalle.f_presentacion) USING "&&&&"
         PRINTX v_fecha_cadena_det

      -- se imprimen las cifras
      ON EVERY ROW
         PRINTX p_nombre_archivo
         PRINTX p_tipo_registro
         PRINTX v_registro_cifra_global.*
         PRINTX v_registro_cifra_detalle.*, v_consec_cuenta
         
         -- se formatea la fecha de liquidacion
         LET v_fecha_cadena = p_f_liquidacion USING "dd-mm-yyyy"
         
         PRINTX v_fecha_cadena

END REPORT

FUNCTION fn_mes_texto(p_mes)
DEFINE p_mes       SMALLINT,
       v_mes_texto STRING

   -- se conforma la palabra del mes
   CASE p_mes

      WHEN 1
         LET v_mes_texto = "Enero"

      WHEN 2
         LET v_mes_texto = "Febrero"

      WHEN 3
         LET v_mes_texto = "Marzo"

      WHEN 4
         LET v_mes_texto = "Abril"

      WHEN 5
         LET v_mes_texto = "Mayo"

      WHEN 6
         LET v_mes_texto = "Junio"

      WHEN 7
         LET v_mes_texto = "Julio"

      WHEN 8
         LET v_mes_texto = "Agosto"

      WHEN 9
         LET v_mes_texto = "Septiembre"

      WHEN 10
         LET v_mes_texto = "Octubre"

      WHEN 11
         LET v_mes_texto = "Noviembre"

      WHEN 12
         LET v_mes_texto = "Diciembre"

   END CASE

   RETURN v_mes_texto
END FUNCTION