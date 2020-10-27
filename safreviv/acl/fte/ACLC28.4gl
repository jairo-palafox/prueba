--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:18/04/2012
--===============================================================

#########################################################################################
#Modulo       => ACL                                                                    #
#Programa     => ACLC28                                                                 #
#Objetivo     => Programa que genera el reporte de cifras de control de Aclaraciones    #
#                Salida manual Aclaratorio                                              # 
#Fecha inicio => Julio 07, 2012                                                         #
#Modificacion => se agrega archivo globales de aclaratorio y se sustituyen              #
#                las variables correspondientes; hilda rivas                            #
#########################################################################################
DATABASE safre_viv

GLOBALS "ACLG02.4gl"  -- se agrega archivo globales y se sustituyen las variables necesarias

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
DEFINE p_tipo_ejecucion     SMALLINT                     -- forma como ejecutara el programa
       ,p_s_titulo           STRING                       -- titulo de la ventana      
       ,v_folio              LIKE glo_folio.folio -- folio de consulta
       ,v_folio_busqueda     LIKE glo_folio.folio -- folio de busqueda
       
   -- se recuperan los argumentos de la linea de comandos
   LET g_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   -- se abre la ventana
   OPEN WINDOW w_consulta WITH FORM "ACLC281"
   
   -- se inicia el folio en null
   LET v_folio = NULL
   
   INPUT v_folio WITHOUT DEFAULTS
     FROM folio
     ATTRIBUTES (UNBUFFERED)
   
      ON ACTION accept
         IF ( v_folio IS NOT NULL ) THEN

            -- se consultan los datos del folio dado
            CALL f_consulta_acl_salidamanual(v_folio)
         ELSE
            CALL fn_mensaje("Atención","Debe capturar un folio","stop")
            CONTINUE INPUT
         END IF
         
      
      ON ACTION cancel
         EXIT INPUT
   
   END INPUT   
   CLOSE WINDOW w_consulta

   
END MAIN

{ ======================================================================
Clave: 
Nombre: f_consulta_acl_salidamanual
Fecha creacion: Junio 20, 2012
Autor: Ivan Vega
Narrativa del proceso que realiza:
Realiza una consulta sobre los datos de Aclaraciones de salida manual
aclaratorio

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION f_consulta_acl_salidamanual(p_folio)
DEFINE p_folio                 LIKE glo_folio.folio, -- folio de consulta
       v_r_despliegue         RECORD -- registro de consulta
          tpo_aclaracion        VARCHAR(50)  ,
          result_operacion      VARCHAR(50)  ,
          num_registros         INTEGER      ,
          imp_ap_pat            DECIMAL(12,2),
          aiv_ap_pat            DECIMAL(18,6),
          imp_am_cre            DECIMAL(12,2),
          int_gen_pgo_ext       DECIMAL(12,2),
          aiv_gen_pgo_ext       DECIMAL(18,6)
       END RECORD,             
       v_arr_despliegue_actualizados DYNAMIC ARRAY OF RECORD -- arreglo de despliegue reg actualizados
          tpo_aclaracion        VARCHAR(50)  ,
          result_operacion      VARCHAR(50)  ,
          num_registros         INTEGER      ,
          imp_ap_pat            DECIMAL(12,2),
          aiv_ap_pat            DECIMAL(18,6),
          imp_am_cre            DECIMAL(12,2),
          int_gen_pgo_ext       DECIMAL(12,2),
          aiv_gen_pgo_ext       DECIMAL(18,6)
       END RECORD,
       v_arr_despliegue_adelantados DYNAMIC ARRAY OF RECORD -- arreglo de despliegue reg adelantados
          tpo_aclaracion             VARCHAR(50)  ,
          result_operacion           VARCHAR(50)  ,
          num_registros              INTEGER      ,
          imp_ap_pat                 DECIMAL(12,2),
          aiv_ap_pat                 DECIMAL(18,6),
          imp_am_cre                 DECIMAL(12,2),
          int_gen_pgo_ext            DECIMAL(12,2),
          aiv_gen_pgo_ext            DECIMAL(18,6)
       END RECORD,
       v_contador              SMALLINT,
       v_sql                   STRING, -- cadena con instruccion SQL
       v_handler               om.SaxDocumentHandler -- handler para el reporte
          
  
   -- ==============================================================================
   --                          REGISTROS ACTUALIZADOS
   -- ==============================================================================
   LET v_sql = "\n select d.tpo_aclaracion       acl,                ",
               "\n        d.result_operacion     res,                ",
               "\n        count(*)               cua,                ",
               "\n        sum(d.imp_ap_pat)      viv,                ",
               "\n        sum(d.aiv_ap_pat)      aiv_ap,             ",
               "\n        sum(d.imp_am_cre)      am,                 ",
               "\n        sum(d.int_gen_pgo_ext) int,                ",
               "\n        sum(d.aiv_gen_pgo_ext) aiv_int             ",
               "\n from   cta_his_pagos       d,                     ",
               "\n        pag_tpo_archivo     a,                     ",
               "\n        pag_ctr_pago        e                      ",
               "\n where  d.folio = ", p_folio, "                    ",
               "\n and    d.folio = e.folio                          ",               
               "\n and    a.archivo_cod = d.origen_archivo           ",
               "\n and    d.id_referencia = e.id_referencia          ",
               "\n and    d.id_derechohabiente = e.id_derechohabiente", 
               "\n and    e.estado_pago = 80                         ", -- adelantado
               "\n and    d.tpo_aclaracion not in (13,17)            ", -- tipo aclaracion distinto de 13 y 17
               "\n and    d.origen_archivo = 1                       ", -- LQINFO
               "\n and    d.ind_liquidacion = 6                      ", -- salida manual aclaratorio
               "\n group  by 1,2                                     ",
               "\n order  by 1,2;                                    "


   -- se prepara y ejecuta la consulta
   PREPARE sid_consulta_actualizados FROM v_sql
   DECLARE cur_consulta_actualizados CURSOR FOR sid_consulta_actualizados
   
   -- se inicia el contador
   LET v_contador = 1
   
   -- se transfieren los datos al arreglo de consulta
   FOREACH cur_consulta_actualizados INTO v_r_despliegue.*
      LET v_arr_despliegue_actualizados[v_contador].* = v_r_despliegue.*
            
      -- se incrementa el contador
      LET v_contador = v_contador + 1
   END FOREACH

   
   -- se abre la ventana de despliegue de resultados
   OPEN WINDOW w_resultados WITH FORM "ACLC282"
   
   -- dialogo con los arreglos de registros aceptados, confirmados y rechazados
   DIALOG
   ATTRIBUTES ( UNBUFFERED)
   
      -- se despliegan los registros aceptados
      DISPLAY ARRAY v_arr_despliegue_actualizados TO tbl_despliegue.*
      END DISPLAY
               
      ON ACTION reporte        
         -- se indica que se usara la plantilla
         IF ( fgl_report_loadCurrentSettings("ACLC28.4rp") ) THEN
            LET v_handler = fgl_report_commitCurrentSettings()      -- commit the file settings
         ELSE
            CALL fn_mensaje("Error","No se encuentra la plantilla ACLC28.4rp. No se puede emitir el reporte","stop")
            CONTINUE DIALOG
         END IF
      
         -- se inicia la emision del reporte
         START REPORT rpt_consulta_acl_salidamanual TO XML HANDLER v_handler
         
         -- se transfieren los datos
         FOR v_contador = 1 TO v_arr_despliegue_actualizados.getLength()
            -- registros actualizados
            OUTPUT TO REPORT rpt_consulta_acl_salidamanual(p_folio, v_arr_despliegue_actualizados[v_contador].*, 1)
         END FOR

         -- se finaliza el reporte
         FINISH REPORT rpt_consulta_acl_salidamanual

      ON ACTION regresar
         EXIT DIALOG
   
   END DIALOG
   CLOSE WINDOW w_resultados
END FUNCTION

{ ======================================================================
Clave: 
Nombre: rpt_consulta_acl_salidamanual
Fecha creacion: Junio 20, 2012
Autor: Ivan Vega
Narrativa del proceso que realiza:
Genera el reporte de cifras de control de salida manual de aclaratorio

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
REPORT rpt_consulta_acl_salidamanual(v_folio, v_r_despliegue, v_tipo_registro)
DEFINE    v_r_despliegue          RECORD -- registro de consulta
             tpo_aclaracion        VARCHAR(50)  ,
             result_operacion      VARCHAR(50)  ,
             num_registros         INTEGER      ,
             imp_ap_pat            DECIMAL(12,2),
             aiv_ap_pat            DECIMAL(18,6),
             imp_am_cre            DECIMAL(12,2),
             int_gen_pgo_ext       DECIMAL(12,2),
             aiv_gen_pgo_ext       DECIMAL(18,6)
          END RECORD,
          v_tipo_registro         SMALLINT, -- tipo del registro 1)Aceptado, 2)Rechazado, 3)Adelantado
          v_leyenda_registro      STRING, -- leyenda del tipo de registro
          v_folio                 LIKE glo_folio.folio, -- folio
          v_nombre_archivo        VARCHAR(40), -- nombre del archivo
          v_fecha                 STRING, -- fecha de emision del reporte
          v_nombre_usuario        VARCHAR(100),
          -- variables para acumular por tipo de registro
          v_num_registros         INTEGER      ,
          v_imp_ap_pat            DECIMAL(12,2),
          v_aiv_ap_pat            DECIMAL(18,6),
          v_imp_am_cre            DECIMAL(12,2),
          v_int_gen_pgo_ext       DECIMAL(12,2),
          v_aiv_gen_pgo_ext       DECIMAL(18,6),
          v_nombre_archivo_rechazos  STRING,
          v_ruta_envio               VARCHAR(40),
          v_folio_formato            VARCHAR(9),
          v_contador                 INTEGER

         
          
FORMAT

   FIRST PAGE HEADER
      -- variables para acumular por tipo de registro
      LET v_num_registros   = 0
      LET v_imp_ap_pat      = 0
      LET v_aiv_ap_pat      = 0
      LET v_imp_am_cre      = 0
      LET v_int_gen_pgo_ext = 0
      LET v_aiv_gen_pgo_ext = 0

      
      -- se envia folio, usuario y fecha
      LET v_fecha = TODAY USING "dd-mm-yyyy"
      -- se obtiene el nombre del usuario
      SELECT usuario_desc
      INTO v_nombre_usuario
      FROM seg_usuario
      WHERE usuario_cod = g_usuario_cod
      
      LET v_nombre_usuario = v_nombre_usuario CLIPPED
      
      -- se obtiene el nombre del archivo
      SELECT nombre_archivo
      INTO   v_nombre_archivo
      FROM   glo_ctr_archivo
      WHERE  folio = v_folio
      
      PRINTX v_folio, g_usuario_cod, v_fecha, v_nombre_usuario, v_nombre_archivo
      

   BEFORE GROUP OF v_tipo_registro
      -- se emite la leyenda en turno
      CASE v_tipo_registro
         WHEN 1 -- aceptado
            LET v_leyenda_registro = "Registros Actualizados (Salida Manual)"
      END CASE
   
      LET v_num_registros   = 0
      LET v_imp_ap_pat      = 0
      LET v_aiv_ap_pat      = 0
      LET v_imp_am_cre      = 0
      LET v_int_gen_pgo_ext = 0
      LET v_aiv_gen_pgo_ext = 0

      -- se muestra la layenda del tipo de registro
      PRINTX v_leyenda_registro
      
   AFTER GROUP OF v_tipo_registro
      PRINTX v_num_registros  ,
             v_imp_ap_pat     ,
             v_aiv_ap_pat     ,
             v_imp_am_cre     ,
             v_int_gen_pgo_ext,
             v_aiv_gen_pgo_ext

   ON EVERY ROW
      PRINTX v_r_despliegue.*, v_tipo_registro
      
      LET v_num_registros   = v_num_registros   + v_r_despliegue.num_registros  
      LET v_imp_ap_pat      = v_imp_ap_pat      + v_r_despliegue.imp_ap_pat     
      LET v_aiv_ap_pat      = v_aiv_ap_pat      + v_r_despliegue.aiv_ap_pat     
      LET v_imp_am_cre      = v_imp_am_cre      + v_r_despliegue.imp_am_cre     
      LET v_int_gen_pgo_ext = v_int_gen_pgo_ext + v_r_despliegue.int_gen_pgo_ext
      LET v_aiv_gen_pgo_ext = v_aiv_gen_pgo_ext + v_r_despliegue.aiv_gen_pgo_ext

{
   ON LAST ROW
      -- se revisa si hubo registros rechazados
      SELECT COUNT(*)
      INTO   v_contador
      FROM   cta_his_pagos
      WHERE  folio = v_folio
      AND    result_operacion = '02'
      
      IF ( v_contador < 1 ) THEN
         LET v_nombre_archivo_rechazos = "No se tienen registros rechazados para este folio."
      ELSE
         -- se recupera el nombre del archivo de rechazos
         SELECT ruta_envio
         INTO   v_ruta_envio
         FROM   seg_modulo
         WHERE  modulo_cod = "acl"
              
         -- el nombre del archivo es
         -- NombreArchivoOriginal + _FOLIO_rechazoACL.SINSS"
         LET v_folio_formato = v_folio USING "&&&&&&&&&"
         
         LET v_contador = LENGTH(v_nombre_archivo CLIPPED)
         
         LET v_nombre_archivo_rechazos = v_nombre_archivo[1,v_contador - 5] || "_" || v_folio_formato || "_rechazoACL.diccncno"
      END IF
         
      PRINTX  v_nombre_archivo_rechazos
}
             
END REPORT   
             
             