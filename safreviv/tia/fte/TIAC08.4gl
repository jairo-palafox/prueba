--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => TIA                                                                    #
#Programa     => TIAC08                                                                 #
#Objetivo     => Consulta de Traspasos IA para Contabilidad                             #
#Autor        => Ivan Vega                                                              #
#Fecha inicio => Abril 07, 2014                                                         #
#########################################################################################
IMPORT os
DATABASE safre_viv

GLOBALS "TIAG01.4gl" --archivo de variables globales proceso_cod

GLOBALS

DEFINE g_pid            LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod    LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod      LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_reg_modulo     RECORD
        ruta_exp        CHAR(40),
        ruta_rescate    CHAR(40),
        ruta_listados   CHAR(40)
       END RECORD,
       seg_modulo_bat   RECORD
        ruta_listados   CHAR(40)
       END RECORD,
       
       l_r_excepcion         DYNAMIC ARRAY OF RECORD             --record de registros 
           chkb                 SMALLINT  ,   
           folio                LIKE  tia_det_traspaso.folio              ,
           id_referencia        LIKE  tia_det_traspaso.id_referencia      ,
           tpo_ent_receptora    LIKE  tia_det_traspaso.tpo_ent_receptora  ,
           cve_ent_receptora    LIKE  tia_det_traspaso.cve_ent_receptora  ,
           tpo_ent_cedente      LIKE  tia_det_traspaso.tpo_ent_cedente    ,
           cve_ent_cedente      LIKE  tia_det_traspaso.cve_ent_cedente    ,
           origen_traspaso      LIKE  tia_det_traspaso.origen_traspaso    ,
           f_presentacion       LIKE  tia_det_traspaso.f_presentacion     ,
           f_movimiento         LIKE  tia_det_traspaso.f_movimiento       ,
           id_decreto           LIKE  tia_det_traspaso.id_decreto         ,
           curp                 LIKE  tia_det_traspaso.curp               ,
           nss_afo_recep        LIKE  tia_det_traspaso.nss_afo_recep      ,
           rfc_afo_recep        LIKE  tia_det_traspaso.rfc_afo_recep      ,
           paterno_afo_recep    LIKE  tia_det_traspaso.paterno_afo_recep  ,
           materno_afo_recep    LIKE  tia_det_traspaso.materno_afo_recep  ,
           nombres_afo_recep    LIKE  tia_det_traspaso.nombres_afo_recep  ,
           cve_sector           LIKE  tia_det_traspaso.cve_sector         ,
           f_recep_solicitud    LIKE  tia_det_traspaso.f_recep_solicitud  ,
           id_lote_solicitud    LIKE  tia_det_traspaso.id_lote_solicitud  ,
           nss_icefa            LIKE  tia_det_traspaso.nss_icefa          ,
           rfc_icefa            LIKE  tia_det_traspaso.rfc_icefa          ,
           nci_icefa            LIKE  tia_det_traspaso.nci_icefa          ,
           paterno_icefa        LIKE  tia_det_traspaso.paterno_icefa      ,
           materno_icefa        LIKE  tia_det_traspaso.materno_icefa      ,
           nombres_icefa        LIKE  tia_det_traspaso.nombres_icefa      ,
           sdo_viv92            LIKE  tia_det_traspaso.sdo_viv92          ,
           int_viv92            LIKE  tia_det_traspaso.int_viv92          ,
           result_operacion     LIKE  tia_det_traspaso.result_operacion    ,
           aivs_viv92           LIKE  tia_det_traspaso.aivs_viv92
        END RECORD

        DEFINE v_continua SMALLINT
       DEFINE  p_usuario_cod     LIKE seg_usuario.usuario_cod
       
END GLOBALS

        DEFINE v_ch_arch_solTransf             BASE.CHANNEL,
                        v_c_ruta_env_acr LIKE seg_modulo.ruta_envio
 
MAIN

    -- clave del usuario firmado
   DEFINE         p_tipo_ejecucion  SMALLINT                     -- forma como ejecutara el programa
          ,p_s_titulo        STRING                       -- titulo de la ventana
          ,v_c_ruta_bin_acr  LIKE seg_modulo.ruta_bin -- ruta del bin de acr
          ,v_c_ruta_list_bat LIKE seg_modulo.ruta_listados -- ruta listados de bat
          ,v_ruta_vacia      STRING
       
   -- argumento con indice 1
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

   SELECT ruta_envio
     INTO  v_c_ruta_env_acr
     FROM seg_modulo
    WHERE modulo_cod = "tia"

   LET v_continua = 1 
    
   OPEN WINDOW w_consulta_tia  WITH FORM "TIAC081"
   WHILE v_continua = 1
     CALL fn_consulta_contabilidad()
     IF v_continua = 0 THEN
        EXIT WHILE
     END IF
   END WHILE  
   CLOSE WINDOW w_consulta_tia    
 
END MAIN


{===========================================================================
Nombre: f_consulta_contabilidad
Fecha creacion: 21 Febrero 2013
Autor: Ivan Vega
Narrativa del proceso que realiza:
 Invoca la generacion del reporte de NSS no localizados en vivienda
 para el folio dado
 
Parametros de Entrada:
 - p_folio: folio del proceso
 - p_usuario_cod: codigo del usuario que ejecuta el reporte
Parámetros de salida:
 - Ninguno
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
============================================================================}
FUNCTION fn_consulta_contabilidad()
   DEFINE 
          manejador_rpt     om.SaxDocumentHandler,
          v_sql             STRING,
          v_construct       STRING,
          v_datos           CHAR(01)
   
   DEFINE v_registro RECORD             --record de registros 
      concepto    VARCHAR(50),
      registros   INTEGER,
      folio       INTEGER,
      saldo_viv92 DECIMAL(22,2),
      aivs_viv92  DECIMAL(24,6)
   END RECORD

   DEFINE     v_v_ruta_nomarch STRING,
                     v_v_nom_archi        STRING,
                     v_cadena_txt           STRING,
                     v_fecha STRING,--Agregado por CABC
                     v_estado_txt SMALLINT

    

   CONSTRUCT v_construct ON det.f_presentacion, det.folio
      FROM f_presentacion, folio

      ON ACTION CANCEL
         LET INT_FLAG = TRUE
         LET v_continua = 0
         RETURN
         EXIT CONSTRUCT

      ON ACTION ACCEPT
         LET v_continua = 1
         ACCEPT CONSTRUCT

   END CONSTRUCT

   IF NOT INT_FLAG THEN



      -- Se asigna la plantilla para generar el reporte
      IF ( fgl_report_loadCurrentSettings("TIAC08.4rp") ) THEN
         CALL fgl_report_selectDevice ("PDF")
         LET manejador_rpt = fgl_report_commitCurrentSettings()

      
         -- registros cargados
         LET v_sql = " SELECT 'CARGADOS',     ",
                     "        COUNT(*),       ",
                     "        det.folio,          ", 
                     "        sum(det.sdo_viv92), ",
                     "        sum(det.aivs_viv92) ",
                     " FROM   tia_det_traspaso det ",
                     " WHERE  ",v_construct,
                     " GROUP BY det.folio "

         PREPARE exe_consulta_1 FROM v_sql
         DECLARE cur_consulta_1 CURSOR FOR exe_consulta_1
      
         LET v_datos = "N"
         
         FOREACH cur_consulta_1 INTO v_registro.*
            LET v_datos = "S"
            EXIT FOREACH
         END FOREACH


         IF v_datos = "S" THEN
         
            -- se inicia el reporte 
            START REPORT rpt_reporte_tia_contabilidad TO XML HANDLER manejador_rpt

                        --==============Se agregan lineas para escribir en archivo de texto CABC=================
        LET v_fecha=TODAY USING "ddmmyyyy"
        LET v_v_nom_archi="Reporte_contable_tia"||v_fecha 

        DISPLAY v_v_ruta_nomarch
       LET v_v_ruta_nomarch = v_c_ruta_env_acr CLIPPED,"/"|| v_v_nom_archi||".tia"


       --DISPLAY v_v_ruta_nomarch
       -- se crea el manejador de archivo
       LET v_ch_arch_solTransf = base.Channel.create()

       LET v_estado_txt=1

       TRY
            -- se crea archivo y se indica que se escribira en el mismo
           CALL v_ch_arch_solTransf.openFile(v_v_ruta_nomarch, "w" )
           CALL v_ch_arch_solTransf.setDelimiter("")
           CALL v_ch_arch_solTransf.write("Concepto|"||"Total de Registros|"||"Folio|"||"Tot SDO VIV 92|"||"Tot AIVS VIV 92")

       CATCH 
             LET v_estado_txt=0
            CALL fn_mensaje("Atención","Hubo un error al crear el reporte TXT.","stop")
       END TRY

    --==========================================================================
            
            FOREACH cur_consulta_1 INTO v_registro.*
                IF v_estado_txt THEN
                    CALL fn_crea_reporte_txt(v_registro.*)
                END IF
               OUTPUT TO REPORT rpt_reporte_tia_contabilidad(p_usuario_cod, v_registro.*,v_construct)
            END FOREACH
            
            -- registros liquidados
            LET v_sql = " SELECT 'LIQUIDADOS',   ",
                        "        COUNT(*),       ",
                        "        det.folio,          ", 
                        "        sum(det.sdo_viv92), ",
                        "        sum(det.aivs_viv92) ",
                        " FROM   tia_det_traspaso det, ",
                        "        cta_decreto b       ",
                        " WHERE  ",v_construct,
                        " AND    det.result_operacion = '99' ",
                        " AND    det.folio            = b.folio_liquida ",
                        " AND    det.id_referencia    = b.id_referencia ",              
                        " GROUP BY det.folio "
            
            PREPARE exe_consulta_2 FROM v_sql
            DECLARE cur_consulta_2 CURSOR FOR exe_consulta_2
            
            
            FOREACH cur_consulta_2 INTO v_registro.*
                IF v_estado_txt THEN
                    CALL fn_crea_reporte_txt(v_registro.*)
                END IF
               OUTPUT TO REPORT rpt_reporte_tia_contabilidad(p_usuario_cod, v_registro.*,v_construct)
            END FOREACH
            
--=========================================
            -- registros NO liquidados
            LET v_sql = " SELECT 'REG EN CERO',      ",
                        "        COUNT(*),           ",
                        "        det.folio,          ", 
                        "        sum(det.sdo_viv92), ",
                        "        sum(det.aivs_viv92) ",
                        " FROM   tia_det_traspaso det ",
                        " WHERE  ",v_construct,
                        " AND    det.result_operacion in ('03','05') ",
                        " GROUP BY det.folio "
            
            PREPARE exe_consulta_25 FROM v_sql
            DECLARE cur_consulta_25 CURSOR FOR exe_consulta_25
            
            
            FOREACH cur_consulta_25 INTO v_registro.*
               IF v_estado_txt THEN
                    CALL fn_crea_reporte_txt(v_registro.*)
                END IF
               OUTPUT TO REPORT rpt_reporte_tia_contabilidad(p_usuario_cod, v_registro.*,v_construct)
            END FOREACH
--=========================================

            -- registros en his_decreto pendientes de liquidar
             LET v_sql = " SELECT 'PENDIENTES LIQ, HIST',    ",
                         "        COUNT(*),        ",
                         "        det.folio,           ", 
                         "        sum(det.sdo_viv92),  ",
                         "        sum(det.aivs_viv92)  ",
                         " FROM   tia_det_traspaso det ",
                         " WHERE  ",v_construct,
                         " AND    result_operacion = '10' ",
                         " GROUP BY det.folio "
            
            PREPARE exe_consulta_4 FROM v_sql
            DECLARE cur_consulta_4 CURSOR FOR exe_consulta_4

          --  LET v_datos = "N"
         
            FOREACH cur_consulta_4 INTO v_registro.*
               LET v_datos = "S"
               EXIT FOREACH
            END FOREACH

            IF v_datos = "S" THEN
               FOREACH cur_consulta_4 INTO v_registro.*
                  IF v_estado_txt THEN
                    CALL fn_crea_reporte_txt(v_registro.*)
                END IF
                  OUTPUT TO REPORT rpt_reporte_tia_contabilidad(p_usuario_cod, v_registro.*,v_construct)
               END FOREACH
            END IF
        
            -- registros rechazados y pendientes de procesar
             LET v_sql = " SELECT 'RECHAZADOS',    ",
                         "        COUNT(*),        ",
                         "        det.folio,           ", 
                         "        sum(det.sdo_viv92),  ",
                         "        sum(det.aivs_viv92)  ",
                         " FROM   tia_det_traspaso det ",
                         " WHERE  ",v_construct,
                         " AND    result_operacion not in ('01','03','05','99','10') ",
                         " GROUP BY det.folio "
            
            PREPARE exe_consulta_3 FROM v_sql
            DECLARE cur_consulta_3 CURSOR FOR exe_consulta_3

            --LET v_datos = "N"
         
            FOREACH cur_consulta_3 INTO v_registro.*
               LET v_datos = "S"
               EXIT FOREACH
            END FOREACH

            IF v_datos = "S" THEN
               FOREACH cur_consulta_3 INTO v_registro.*
                  IF v_estado_txt THEN
                    CALL fn_crea_reporte_txt(v_registro.*)
                END IF
                  OUTPUT TO REPORT rpt_reporte_tia_contabilidad(p_usuario_cod, v_registro.*,v_construct)
                  
               END FOREACH
            ELSE   
               CALL fn_mensaje("Atención","No existen datos con los criterios capturados\nNo es posible generar el reporte","stop")
               LET v_continua = 0
            END IF   
               IF v_estado_txt THEN
                    CALL fn_mensaje("Atención","El reporte TXT se generó correctamente","stop")
                    CALL v_ch_arch_solTransf.close()
                END IF
            
            FINISH REPORT rpt_reporte_tia_contabilidad
            CALL fn_mensaje("Atención","El reporte PDF se generó correctamente","stop")
         ELSE
            CALL fn_mensaje("Atención","No existen datos con los criterios capturados\nNo es posible generar el reporte","stop")
            LET v_continua = 1
           
         END IF 
      ELSE         
         CALL fn_mensaje("Atención","No se pudo abrir la plantilla TIAC08.4rp.\nNo es posible generar el reporte","stop")
         LET v_continua = 0
         
      END IF
   ELSE
      CALL fn_mensaje("Atención","No existen datos con los criterios capturados\nNo es posible generar el reporte","stop")
      LET v_continua = 0
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
REPORT rpt_reporte_tia_contabilidad(p_usuario_code, r_registro,v_construct)
   DEFINE  p_usuario_code                        LIKE seg_usuario.usuario_cod
   DEFINE       p_nombre_usuario        LIKE seg_usuario.usuario_desc
          
   DEFINE r_registro RECORD             --record de registros 
      concepto    VARCHAR(50),
      registros   INTEGER,
      folio       INTEGER,
      saldo_viv92 LIKE  tia_det_traspaso.sdo_viv92,
      aivs_viv92  LIKE  tia_det_traspaso.aivs_viv92
   END RECORD
   
   DEFINE v_consec_cuenta  LIKE afi_decreto.consec_cuenta,
          v_fecha_reporte  STRING,
          v_periodo_inicio STRING,
          v_periodo_final  STRING

   DEFINE v_construct  STRING
   DEFINE v_construct2 CHAR(80)
   DEFINE v_usuario    CHAR(20)
   
          
   FORMAT

      FIRST PAGE HEADER
         -- se obtiene el nombre del usuario
      {   SELECT USER
         INTO  p_nombre_usuario
         FROM  seg_modulo
         WHERE modulo_cod = 'tia'}


         SELECT usuario_desc
         INTO   p_nombre_usuario       
         FROM   seg_usuario
         WHERE  usuario_cod = p_usuario_code
         
         
         -- la fecha del reporte
         LET v_fecha_reporte = TODAY USING "dd-mm-yyyy"
         LET v_construct2 = v_construct
         --LET v_construct2 = v_construct2[27,75]

         -- se imprime el folio, codigo de usuario y su nombre
--         PRINTX v_periodo_inicio, v_periodo_final, p_usuario_cod, p_nombre_usuario, v_fecha_reporte
         PRINTX v_construct2, p_usuario_code, p_nombre_usuario, v_fecha_reporte

      -- se imprimen las cifras
      ON EVERY ROW         
         PRINTX r_registro.*, v_consec_cuenta

END REPORT

FUNCTION fn_mes_texto(p_mes)
DEFINE   p_mes       SMALLINT,
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

FUNCTION fn_crea_reporte_txt(r_registro)
    DEFINE r_registro RECORD             --record de registros 
      concepto    VARCHAR(50),
      registros   INTEGER,
      folio       INTEGER,
      saldo_viv92 LIKE  tia_det_traspaso.sdo_viv92,
      aivs_viv92  LIKE  tia_det_traspaso.aivs_viv92
   END RECORD

   DEFINE v_cadena_txt STRING

   LET v_cadena_txt=r_registro.concepto ,"|",
                                                             r_registro.registros ,"|",
                                                             r_registro.folio CLIPPED,"|",
                                                             r_registro.saldo_viv92 ,"|",
                                                              r_registro.aivs_viv92,"|" CLIPPED

            --DISPLAY v_cadena_txt
                                                              
            CALL v_cadena_txt.trim() RETURNING v_cadena_txt
            CALL v_ch_arch_solTransf.writeLine(v_cadena_txt)
   
END FUNCTION
