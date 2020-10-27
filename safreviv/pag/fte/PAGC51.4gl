--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 
--===============================================================

#########################################################################################
#Modulo       => pag                                                                    #
#Programa     => PAGC51                                                                 #
#Objetivo     => Consulta reporte de Integración Reg. Pagos Fondo Anterior              #
#Fecha inicio => Febrero 13, 2013                                                       # 
#Autor        => Ivan Vega                                                              #
#########################################################################################

DATABASE safre_viv

DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado

MAIN
DEFINE p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING -- titulo de la ventana

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)
  
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   -- consulta de informacion recibida Fondo Anterior
   CALL fn_consulta_folio_fa()

END MAIN

{ ======================================================================
Clave: PAGC51
Nombre: fn_consulta_folio_fa
Fecha creacion: Febrero 13, 2013
Autor: Ivan Vega
Narrativa del proceso que realiza:
Realiza la consulta de datos cargados de Reg. de Pagos de Fondo Anterior

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_consulta_folio_fa()
DEFINE --p_usuario_cod            LIKE seg_usuario.usuario_cod, -- clave del usuario
       v_folio                  DECIMAL(9,0) -- folio
       -- v_cbx_folios             ui.ComboBox, -- combo de afores
       -- v_s_cadena               STRING, -- cadena de texto
       -- v_r_glo_ctr_archivo      RECORD LIKE glo_ctr_archivo.*,
       -- v_r_glo_folio            RECORD LIKE glo_folio.*,
       -- v_SqlQry                 STRING 
       -- ed_folio                 INTEGER 

   OPEN WINDOW w_consulta_folio WITH FORM "PAGC511"
     
   INPUT v_folio WITHOUT DEFAULTS
      FROM ed_folio    ATTRIBUTES (UNBUFFERED)
  
      --BEFORE INPUT
         -- se asignan los valores por omision
         --LET v_folio = ed_folio
         -- se llena el arreglo de folios
      {   LET v_SqlQry =" SELECT UNIQUE a.folio \n",
                       " FROM   glo_folio a, pag_det_fc b \n",
                       " WHERE  a.proceso_cod = 1405 \n",-- Proceso de solo fortalecimiento de credito
                       " AND    a.folio = b.folio \n",
                       " AND    status = 0" 
--DISPLAY "@ query @: ", v_SqlQry
            --se prepara el steatment 
            PREPARE num_folio_fc FROM v_SqlQry
            --se declara el cursor 
            DECLARE cur_folio_fc CURSOR FOR num_folio_fc
            FOREACH cur_folio_fc INTO v_r_glo_folio.folio
               LET v_s_cadena = v_r_glo_folio.folio
               CALL v_cbx_folios.addItem(v_r_glo_folio.folio, v_s_cadena)
            END FOREACH

         FREE cur_folio_fc}
        
      ON ACTION ACCEPT
         DISPLAY "@FOLIO INGRESADO: ", v_folio
         IF v_folio IS NULL OR v_folio = 0 THEN
            CALL fn_mensaje("Atención","Debe ingresar un folio","stop")
         ELSE 
            CALL fn_consulta_archivo(v_folio)
            CONTINUE INPUT
         END IF
         
      ON ACTION CANCEL
         EXIT INPUT
   
   END INPUT
   
   CLOSE WINDOW w_consulta_folio

END FUNCTION

{
======================================================================
Clave: PAGC51
Nombre: fn_consulta_archivo
Fecha creacion: Febrero 13, 2013
Autor: Ivan Vega
Narrativa del proceso que realiza:
Consulta los datos cargados de Reg. Pagos Fondo Anterior para un 
folio dado

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_consulta_archivo(p_folio)
DEFINE p_folio          DECIMAL(9,0), -- folio
       reg_cifras       RECORD
        total_registros   INTEGER, -- num registros
        imp_ap_pat        DECIMAL(22,2) -- monto total
      END RECORD

   DISPLAY "v_folio recibido: ", p_folio       

   -- se realiza la consulta para obtener las cifras
   --Obtenemos la suma de los importes y total de los registros
   SELECT COUNT(*),
          SUM(imp_ap_fa)
   INTO   reg_cifras.total_registros,
          reg_cifras.imp_ap_pat
   FROM   pag_det_fa
   WHERE  folio = p_folio

   IF reg_cifras.total_registros = 0 THEN
      CALL fn_mensaje('Atención','No se encontraron resultados con el folio ingresado','stop') 
      RETURN 
   END IF 

   -- se abre la ventana para mostrar los datos
   OPEN WINDOW w_consulta WITH FORM "PAGC512"
   DISPLAY BY NAME reg_cifras.*

    MENU ""
       COMMAND "Cancelar"
          EXIT MENU

        COMMAND "Reporte"
           CALL fn_reporte_carga_archivo(p_folio, reg_cifras.*, TRUE)
    END MENU
   
   CLOSE WINDOW w_consulta

END FUNCTION

-- OBJETIVO: Obtener los datos necesarios para emitir el reporte de Integracion Fondo Anterior
FUNCTION fn_reporte_carga_archivo(p_folio, reg_cifras, p_b_despliegue_pantalla)
    DEFINE p_folio                 INTEGER
    DEFINE p_b_despliegue_pantalla SMALLINT, -- booleana que indica si el archivo se escribe en disco o se envia a pantalla
    reg_cifras RECORD
         total_registros       INTEGER, 
         imp_ap_pat            LIKE cta_his_pagos.imp_ap_pat     
      END RECORD,

      manejador_rpt         om.SaxDocumentHandler
    
    DEFINE v_ruta_reporte       STRING -- ruta del archivo del reporte
    DEFINE v_ruta_listados      STRING -- ruta de los listados
    DEFINE v_ruta_ejecutable    STRING -- ruta del ejecutable
    DEFINE v_nombre_usuario     LIKE seg_usuario.usuario_desc
    DEFINE v_nombre_archivo     LIKE glo_ctr_archivo.nombre_archivo
    
    -- Se obtiene nombre de usuario y archivo
    SELECT usuario_desc 
      INTO v_nombre_usuario
      FROM seg_usuario 
     WHERE usuario_cod = p_usuario_cod

    SELECT nombre_archivo 
      INTO v_nombre_archivo 
      FROM glo_ctr_archivo
     WHERE folio = p_folio 
    
    # Recupera la ruta de listados en el que se enviara el archivo
    CALL fn_rutas("pag") RETURNING v_ruta_ejecutable, v_ruta_listados 
                                                --r_ruta_bin, r_ruta_listados
    --Se asigna la plantilla para generar el reporte
    IF fgl_report_loadCurrentSettings("PAGC51.4rp") THEN 
        CALL fgl_report_selectDevice ("PDF")
                    
        LET v_ruta_reporte = v_ruta_listados CLIPPED,"/","carga_fondo_anterior"
        CALL fgl_report_setOutputFileName(v_ruta_reporte)
        CALL fgl_report_selectPreview(1)
        LET manejador_rpt = fgl_report_commitCurrentSettings()
    ELSE         
        DISPLAY "No fue posible generar el reporte"
        EXIT PROGRAM 
    END IF   
    --Inicia el reporte de registros con rechazo
    START REPORT rpt_carga_fondo_anterior TO XML HANDLER manejador_rpt
    -- Asigna el titulo del reporte
                
    OUTPUT TO REPORT rpt_carga_fondo_anterior(reg_cifras.*, p_folio, v_nombre_usuario, v_nombre_archivo)                                                                
    FINISH REPORT rpt_carga_fondo_anterior 
END FUNCTION

-- OBJETIVO: Emitir el reporte de carga
REPORT rpt_carga_fondo_anterior(reg_cifras, p_folio, p_nombre_usuario, p_nombre_archivo)
  DEFINE  p_folio DECIMAL(9,0), 
    reg_cifras RECORD
         total_registros       INTEGER, 
         imp_ap_pat            LIKE cta_his_pagos.imp_ap_pat
      END RECORD,
        v_fecha_reporte           DATE,

          p_nombre_usuario      LIKE seg_usuario.usuario_desc,
          p_nombre_archivo      LIKE glo_ctr_archivo.nombre_archivo

FORMAT

   FIRST PAGE HEADER

      LET v_fecha_reporte = TODAY CLIPPED

      PRINTX v_fecha_reporte USING "dd-mm-yyyy"
      PRINTX p_folio
      PRINTX p_usuario_cod
      PRINTX p_nombre_usuario
      PRINTX p_nombre_archivo

   ON EVERY ROW
      PRINTX reg_cifras.*

END REPORT  