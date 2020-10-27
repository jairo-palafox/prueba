--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => PAG                                                                    #
#Programa     => PAGC05                                                                 #
#Objetivo     => Consulta de TIA                                                        #
#Fecha inicio => Septiembre 19, 2012                                                         #
#########################################################################################
DATABASE safre_viv
GLOBALS "TIAG01.4gl"  ---archivo de variables globales proceso_cod, opera_cod
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
DEFINE p_tipo_ejecucion     SMALLINT                     -- forma como ejecutara el programa
       ,p_s_titulo           STRING                       -- titulo de la ventana      
       ,v_folio              LIKE glo_folio.folio -- folio de consulta
       ,v_folio_temp         LIKE cta_his_pagos.folio
       
   -- se recuperan los argumentos de la linea de comandos
   LET g_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   -- se abre la ventana
   OPEN WINDOW w_consulta WITH FORM "TIAC061"
   
   -- se inicia el folio en null
   LET v_folio = NULL
   
   INPUT v_folio WITHOUT DEFAULTS
     FROM folio
     ATTRIBUTES (UNBUFFERED)
   
      ON ACTION accept
         IF ( v_folio IS NOT NULL ) THEN

            LET v_sql = "SELECT FIRST 1 (a.folio)
                           FROM tia_det_traspaso a
                          WHERE a.folio = ", v_folio,
                          " AND a.result_operacion = '01' ",
                      "ORDER BY folio 
                           DESC"
            -- se prepara y ejecuta la consulta
            PREPARE consulta_existe_folio FROM v_sql
            DECLARE cur_consulta_folio CURSOR FOR consulta_existe_folio
   
            -- se transfieren los datos al arreglo de consulta
            FOREACH cur_consulta_folio INTO v_folio_temp
            END FOREACH 
   
            IF v_folio_temp IS NULL OR 0 THEN
              CALL fn_mensaje("Atención","El folio capturado no existe","stop")
            ELSE 
              CALL f_consulta_tia(v_folio)
            END IF
            
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
Clave: TIAC06
Nombre: f_consulta_tia
Fecha creacion: Septiembre 19, 2012
Narrativa del proceso que realiza:
Realiza una consulta sobre los datos de TIA y permite emitir el resultado de
la misma en un reporte

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION f_consulta_tia(p_folio)
DEFINE   p_folio                 LIKE glo_folio.folio, -- folio de consulta
         v_arr_despliegue DYNAMIC ARRAY OF RECORD -- registro de consulta
             num_registros        SMALLINT,
             ind_consistencia     SMALLINT,
             sum_pesos            DECIMAL (18,2)
         END RECORD ,
         v_arr_reporte_cons,      v_arr_reporte_nocons, 
         v_arr_reporte_cons_rech, v_arr_reporte_nocons_rech    RECORD 
             num_registros        SMALLINT,
             ind_consistencia     SMALLINT,
             sum_pesos            DECIMAL (18,2)     
         END RECORD ,     
         v_indice SMALLINT ,
         v_ruta_reporte            STRING ,-- ruta del archivo del reporte
         v_ruta_listados           STRING ,-- ruta de los listados
         v_ruta_ejecutable         STRING, -- ruta del ejecutable     
         manejador_rpt            om.SaxDocumentHandler

   
          
   -- ==========================================================================
   --  SE OBTIENEN LOS DATOS PARA EL REPORTE DE LOS REGISTROS ACEPTADOS
   -- ==========================================================================
   -- ind_consistencia = 1 Consistente
   -- ind_consistencia = 1 NO Consistente
   LET v_sql = "\n SELECT COUNT(*), b.ind_consistencia  ,",
               "\n        SUM(sdo_viv92)                 ",
               "\n FROM   tia_det_traspaso a            ,",
               "\n        afi_decreto      b             ",
               "\n WHERE folio = ", p_folio               , 
               "\n AND    a.id_decreto = b.id_decreto    ",
               "\n AND    a.result_operacion ='01'         ",
               "\n GROUP BY 2                            ",
               "\n ORDER BY 2                            "

   DISPLAY "@query: ", v_sql
   CALL v_arr_despliegue.clear()
   
   -- se prepara y ejecuta la consulta
   PREPARE sid_consulta_tia FROM v_sql
   DECLARE cur_consulta_tia CURSOR FOR sid_consulta_tia
   
   -- se inicia el contador
   LET v_indice = 1
   
   -- se transfieren los datos al arreglo de consulta
   FOREACH cur_consulta_tia INTO v_arr_despliegue[v_indice].*
   
      IF v_arr_despliegue[v_indice].ind_consistencia = 1 THEN 
          LET v_arr_reporte_cons.num_registros    = v_arr_despliegue[v_indice].num_registros
          LET v_arr_reporte_cons.ind_consistencia = 1
          LET v_arr_reporte_cons.sum_pesos        = v_arr_despliegue[v_indice].sum_pesos
      ELSE
          LET v_arr_reporte_nocons.num_registros    = v_arr_despliegue[v_indice].num_registros
          LET v_arr_reporte_nocons.ind_consistencia = 0
          LET v_arr_reporte_nocons.sum_pesos        = v_arr_despliegue[v_indice].sum_pesos
      END IF 
      
      LET v_indice = v_indice + 1
   END FOREACH

-- ==========================================================================
   --  SE OBTIENEN LOS DATOS PARA EL REPORTE DE LOS REGISTROS RECHAZADOS
   -- ==========================================================================
   -- ind_consistencia = 1 Consistente
   -- ind_consistencia = 1 NO Consistente
   LET v_sql = "\n SELECT COUNT(*), b.ind_consistencia  ,",
               "\n        SUM(sdo_viv92)                 ",
               "\n FROM   tia_det_traspaso a            ,",
               "\n        afi_decreto      b             ",
               "\n WHERE folio = ", p_folio               , 
               "\n AND    a.id_decreto = b.id_decreto    ",
               "\n AND    a.result_operacion in ('02','03','04','08')    ",
               "\n GROUP BY 2                            ",
               "\n ORDER BY 2                            "

   DISPLAY "@query: ", v_sql
   CALL v_arr_despliegue.clear()
   
   -- se prepara y ejecuta la consulta
   PREPARE sid_consulta_tia_rech FROM v_sql
   DECLARE cur_consulta_tia_rech CURSOR FOR sid_consulta_tia_rech
   
   -- se inicia el contador
   LET v_indice = 1

   
   -- se transfieren los datos al arreglo de consulta
   FOREACH cur_consulta_tia_rech INTO v_arr_despliegue[v_indice].*

      IF v_arr_despliegue[v_indice].ind_consistencia = 1 THEN 
          LET v_arr_reporte_cons_rech.num_registros    = v_arr_despliegue[v_indice].num_registros
          LET v_arr_reporte_cons_rech.ind_consistencia = 1
          LET v_arr_reporte_cons_rech.sum_pesos        = v_arr_despliegue[v_indice].sum_pesos
      ELSE
          LET v_arr_reporte_nocons_rech.num_registros    = v_arr_despliegue[v_indice].num_registros
          LET v_arr_reporte_nocons_rech.ind_consistencia = 0
          LET v_arr_reporte_nocons_rech.sum_pesos        = v_arr_despliegue[v_indice].sum_pesos
      END IF 
      
      LET v_indice = v_indice + 1
   END FOREACH

--DISPLAY "v_arr_reporte_cons        ",v_arr_reporte_cons.*    
--DISPLAY "v_arr_reporte_nocons      ", v_arr_reporte_nocons.*
--DISPLAY "v_arr_reporte_cons_rech   ",v_arr_reporte_cons_rech.*
--DISPLAY "v_arr_reporte_nocons_rech ",v_arr_reporte_nocons_rech.*

 
   IF     (v_arr_reporte_cons.num_registros = 0 OR v_arr_reporte_cons.num_registros IS NULL ) 
      AND (v_arr_reporte_nocons.num_registros = 0 OR v_arr_reporte_nocons.num_registros IS NULL )
      AND (v_arr_reporte_cons_rech.num_registros = 0 OR v_arr_reporte_cons_rech.num_registros IS NULL )
      AND (v_arr_reporte_nocons_rech.num_registros = 0 OR v_arr_reporte_nocons_rech.num_registros IS NULL ) THEN 
   
      CALL fn_mensaje("Atención","No existen datos para el folio ingresado","stop")
    ELSE 
      
      # Recupera la ruta de listados en el que se enviara el archivo
      CALL fn_rutas("tia") RETURNING v_ruta_ejecutable, v_ruta_listados 
    
      --Se asigna la plantilla para generar el reporte
      IF fgl_report_loadCurrentSettings("../../tia/bin/TIAC06.4rp") THEN 
          CALL fgl_report_selectDevice ("PDF")
                      
          LET v_ruta_reporte = v_ruta_ejecutable CLIPPED,"/","cifras_control_tia"                
          CALL fgl_report_setOutputFileName(v_ruta_reporte)
          CALL fgl_report_selectPreview(1)
          LET manejador_rpt = fgl_report_commitCurrentSettings()
      ELSE         
          DISPLAY "No fue posible generar el reporte. No se encuentra la plantilla TIAC06.4rp"
          EXIT PROGRAM 
      END IF  

   
         -- se inicia la emision del reporte
         START REPORT rpt_consulta_tia TO XML HANDLER manejador_rpt
         
            OUTPUT TO REPORT rpt_consulta_tia(p_folio, v_arr_reporte_cons.*, v_arr_reporte_nocons.*, 
                                              v_arr_reporte_cons_rech.*, v_arr_reporte_nocons_rech.*)
         
         -- se finaliza el reporte
         FINISH REPORT rpt_consulta_tia
  END IF  
END FUNCTION

{ ======================================================================
Clave: TIAC06
Nombre: rpt_consulta_tia
Fecha creacion: Septiembre 19, 2012
Narrativa del proceso que realiza:
Genera el reporte de tia

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
REPORT rpt_consulta_tia(v_folio, v_arr_report_cons, v_arr_report_nocons, 
                        v_arr_report_cons_rech, v_arr_report_nocons_rech)
DEFINE    v_arr_report_cons, v_arr_report_nocons,
          v_arr_report_cons_rech, v_arr_report_nocons_rech  RECORD -- registro de consulta
             num_registros        SMALLINT,
             ind_consistencia     SMALLINT,
             sum_pesos            DECIMAL (18,2)
          END RECORD,
          
          v_folio                 LIKE glo_folio.folio, -- folio
          v_fecha                 STRING, -- fecha de emision del reporte
          v_nombre_usuario        VARCHAR(100)
          
FORMAT

   FIRST PAGE HEADER
     
      -- se envia folio, usuario y fecha
      LET v_fecha = TODAY USING "dd-mm-yyyy"
      
      -- se obtiene el nombre del usuario
      SELECT usuario_desc
      INTO v_nombre_usuario
      FROM seg_usuario
      WHERE usuario_cod = g_usuario_cod
      
      LET v_nombre_usuario = v_nombre_usuario CLIPPED
      
      PRINTX v_folio, g_usuario_cod, v_fecha, v_nombre_usuario
      
   ON EVERY ROW
      PRINTX v_arr_report_cons.*
      PRINTX v_arr_report_nocons.*
      PRINTX v_arr_report_cons_rech.*
      PRINTX v_arr_report_nocons_rech.*
    
END REPORT   
             
             