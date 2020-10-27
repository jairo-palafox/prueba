--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================

#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETC192                                                                #
#Objetivo     => Consulta montos sobregirados Fondo de Ahorro Contingente               #
#Fecha inicio => Noviembre 04, 2014                                                     # 
#########################################################################################

DATABASE safre_viv

GLOBALS "RETG01.4gl"  -- se agrega archivo globales y se sustituyen las variables necesarias

GLOBALS
	DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado
          ,v_ventana                ui.WINDOW
          ,v_folio         DECIMAL(9,0)
END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
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
   
   -- consulta de informacion recibida 
   CALL fn_consulta_montos(p_usuario_cod)

END MAIN

{ ============================================================================
Clave: RETC192
Nombre: fn_consulta_montos
Fecha creacion: Noviembre 04, 2014
Registro de modificaciones:
Descrip: CONSULTA MONTOS SOBREGIRADOS DE FONDO DE AHORRO CONOTINGENTE
==============================================================================
}
FUNCTION fn_consulta_montos(p_usuario_cod)
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario
       --v_folio          DECIMAL(9,0), -- folio
       v_cbx_folios     ui.ComboBox, -- combo de folios
       
       v_s_cadena       STRING, -- cadena de texto
       v_r_glo_folio    RECORD LIKE glo_folio.*,
       

       arr_reg_folio           DYNAMIC ARRAY OF RECORD
         v_folio                DECIMAL(9,0),
         v_registros            DECIMAL(6,0),
         v_tanto_normal         DECIMAL(24,2),
         v_tanto_adicional      DECIMAL(24,2),
         v_tipo                 CHAR(5)         
       END RECORD,
       arr_reg_det_folio           DYNAMIC ARRAY OF RECORD
         v_folio                DECIMAL(9,0),
         v_rfc                  LIKE ret_fondo_ahorro.rfc,
         v_tanto_normal         DECIMAL(24,2),
         v_tanto_adicional      DECIMAL(24,2),
         v_tipo                 CHAR(5)         
       END RECORD,
       arr_reporte           DYNAMIC ARRAY OF RECORD
         v_folio                DECIMAL(9,0),
         v_rfc                  LIKE ret_fondo_ahorro.rfc,
         v_tanto_normal         DECIMAL(24,2),
         v_tanto_adicional      DECIMAL(24,2),
         v_tipo                 CHAR(5),
         v_nombre               LIKE afi_fondo72.nombre
       END RECORD,
       
       v_query                       STRING, -- detalle
       v_indice                      SMALLINT, -- indice de arreglo       
       v_ruta_reporte                STRING ,-- ruta del archivo del reporte       
       v_ruta_listados               STRING ,-- ruta de los listados
       v_ruta_ejecutable             STRING ,-- ruta del ejecutable
       manejador_rpt                 om.SaxDocumentHandler ,
       v_indice_reporte              SMALLINT,
       v_id_solicitud                DECIMAL(9,0)
       
   OPEN WINDOW w_consulta_saldos WITH FORM "RETC1921"
   LET  v_ventana = UI.WINDOW.GETCURRENT()
   CALL v_ventana.SETTEXT("Consulta Sobregiros Fondo de Ahorro Contingente")

   -- se le asigna el apuntado del combo a la variable
   LET v_cbx_folios = ui.ComboBox.forName("formonly.cmb_folio")
  
   -- se inician los combobox en blanco
   CALL v_cbx_folios.clear()

   INPUT v_folio 
      FROM cmb_folio 
      ATTRIBUTES (UNBUFFERED, WITHOUT DEFAULTS)

      BEFORE INPUT 
         -- se limpian las variables
         LET v_folio     = NULL 
         
         -- se llena el arreglo de folios
         DECLARE cur_folios CURSOR FOR
         SELECT a.*
         FROM   glo_folio a
         WHERE  a.proceso_cod = g_proceso_cod_ret_fondo_ahorro_arch
         AND    a.status >= 1
         ORDER BY folio DESC

         FOREACH cur_folios INTO v_r_glo_folio.*
            LET v_s_cadena = v_r_glo_folio.folio
            CALL v_cbx_folios.addItem(v_r_glo_folio.folio, v_s_cadena)
         END FOREACH

         FREE cur_folios
        
      ON ACTION ACCEPT
         -- se borran los arreglos de despliegue
         --CALL v_arr_agrupador.clear()
         CALL arr_reg_det_folio.clear()
         CALL arr_reg_folio.clear()
         CALL arr_reporte.clear()

         --modificación de validación de  captura de parametros
         --valida que se ingrese al menos un parametro
         IF (v_folio     IS NULL OR v_folio <= 0) 
              THEN

            CALL fn_mensaje("Consulta","Debe de ingresar al menos un criterio de búsqueda","about")
            CONTINUE INPUT
         ELSE 
            -- se abre la ventana de resultados
            OPEN WINDOW w_detalle_consulta_saldos WITH FORM "RETC1922"
                     -- se limpia el arreglo de despligue del detalle
                       
                     CALL arr_reg_folio.clear()
                     -- se consulta del detalle de este agrupador
--                     LET v_query = "\n select rfa.folio, count(*)as registros, sum(rp.importe*(-1)) as tanto_normal,              ",
--                                   "\n        0 as tanto_adicional, rfa.id_traspaso as tipo ",
--                                   "\n   from ret_fondo_ahorro rfa, ret_preliquida72 rp                        ",
--                                   "\n  where rfa.id_solicitud = rp.id_referencia                              ",
--                                   "\n    and rp.movimiento = 802                                              ",
--                                   "\n    and rfa.folio = ", v_folio,
--                                   "\n  group by rfa.folio, rfa.id_traspaso "

                     LET v_query = "\n SELECT A.folio, A.registros, NVL(B.tanto_normal,0),                     ", 
                                   "\n                     NVL(C.tanto_adicional,0), B.tipo                    ",
                                   "\n FROM (SELECT rfa.folio, COUNT(*)AS registros, 0 AS tanto_normal,        ",
                                   "\n              0 AS tanto_adicional, rfa.id_traspaso AS tipo              ",
                                   "\n	       FROM ret_fondo_ahorro rfa, ret_preliquida72 rp                  ",      
                                   "\n	      WHERE rfa.id_solicitud = rp.id_referencia                        ",      
                                   "\n	        AND rfa.folio = ", v_folio                                      , 
                                   "\n	        AND rp.movimiento in (802, 422)                                ",
                                   "\n	      GROUP BY rfa.folio, rfa.id_traspaso) A                           ",
                                   "\n	    LEFT OUTER JOIN (SELECT rfa.folio, COUNT(*) AS registros,          ",
                                   "\n                              SUM(rp.importe*(-1)) AS tanto_normal,      ",        
                                   "\n			                    0 AS tanto_adicional,                      ",
                                   "\n                              rfa.id_traspaso AS tipo                    ",
                                   "\n			               FROM ret_fondo_ahorro rfa, ret_preliquida72 rp  ",                      
                                   "\n			              WHERE rfa.id_solicitud = rp.id_referencia        ",                              
                                   "\n			                AND rp.movimiento = 802                        ",                      
                                   "\n			                AND rfa.folio = ", v_folio                      ,
                                   "\n			              GROUP BY rfa.folio, rfa.id_traspaso) B           ",
                                   "\n			         ON A.folio = B.folio                                  ",
                                   "\n			        AND A.tipo = B.tipo                                    ",
                                   "\n	    LEFT OUTER JOIN (SELECT rfa.folio, COUNT(*) AS registros,          ",
                                   "\n                              0 AS tanto_normal,                         ",
                                   "\n                              SUM(rp.importe*(-1)) AS tanto_adicional,   ",
                                   "\n                              rfa.id_traspaso AS tipo                    ",
                                   "\n				           FROM ret_fondo_ahorro rfa, ret_preliquida72 rp  ",                      
                                   "\n				          WHERE rfa.id_solicitud = rp.id_referencia        ",                      
                                   "\n				            AND rp.movimiento = 422                        ", 
                                   "\n				            AND rfa.folio = ", v_folio                      ,
                                   "\n				          GROUP BY rfa.folio, rfa.id_traspaso) C           ",
                                   "\n				     ON B.folio = C.folio                                  ",
                                   "\n			        AND B.tipo =  C.tipo                                   "
                     
                     DISPLAY v_query
                     PREPARE sid_folio FROM v_query
                     DECLARE cur_folio  CURSOR FOR sid_folio
                     
                     --llena el arreglo        
                     LET v_indice = 1
                     
                     FOREACH cur_folio INTO 
                           arr_reg_folio[v_indice].v_folio              ,
                           arr_reg_folio[v_indice].v_registros          ,
                           arr_reg_folio[v_indice].v_tanto_normal       ,
                           arr_reg_folio[v_indice].v_tanto_adicional    ,
                           arr_reg_folio[v_indice].v_tipo
                           LET v_indice = v_indice + 1
                     END FOREACH
                     
                     -- se borra el ultimo registro
                     CALL arr_reg_folio.deleteElement(arr_reg_folio.getLength())

               
               -- se limpia el arreglo de despligue del detalle
                     CALL arr_reg_det_folio.clear()
                     -- se consulta del detalle de este agrupador
                     LET v_query = "\n select rfa.folio, rfa.rfc , rp.importe*(-1) as tanto_normal,                     ",
                                   "\n        0 as tanto_adicional, rfa.id_traspaso as tipo ",
                                   "\n   from ret_fondo_ahorro rfa, ret_preliquida72 rp                        ",
                                   "\n  where rfa.id_solicitud = rp.id_referencia                              ",
                                   "\n    and rp.movimiento = 802                                              ",
                                   "\n    and rfa.folio = ", v_folio
                     
                     DISPLAY v_query
                     PREPARE sid_det_folio FROM v_query
                     DECLARE cur_det_folio  CURSOR FOR sid_det_folio
                     
                     --llena el arreglo        
                     LET v_indice = 1
                     
                     FOREACH cur_det_folio INTO 
                           arr_reg_det_folio[v_indice].v_folio              ,
                           arr_reg_det_folio[v_indice].v_rfc                ,
                           arr_reg_det_folio[v_indice].v_tanto_normal       ,
                           arr_reg_det_folio[v_indice].v_tanto_adicional    ,
                           arr_reg_det_folio[v_indice].v_tipo
                           
                           LET v_indice = v_indice + 1
                     END FOREACH
                     
                     -- se borra el ultimo registro
                     CALL arr_reg_det_folio.deleteElement(arr_reg_det_folio.getLength())

                     DISPLAY ARRAY arr_reg_folio TO t_folio.*
                     DISPLAY ARRAY arr_reg_det_folio TO d_folio.*
               
               ON ACTION regresar
                  EXIT DISPLAY 
                  
               ON ACTION reporte                      
                  -- se consulta del detalle de este agrupador
                     LET v_query = "\n select distinct rfa.folio, rfa.rfc , rp.importe*(-1) as tanto_normal,                ",
                                   "\n        0 as tanto_adicional, rfa.id_traspaso as tipo,    ",
                                   "\n        af.nombre                                                       ",
                                   "\n   from ret_fondo_ahorro rfa                                            ",
                                   "\n        left outer join afi_fondo72 af                                  ",
                                   "\n                     on rfa.id_derechohabiente = af.id_derechohabiente, ",
                                   "\n        ret_preliquida72 rp                                             ",
                                   "\n  where rfa.id_solicitud = rp.id_referencia                             ",
                                   "\n    and rp.movimiento = 802                                             ",
                                   "\n    and rfa.folio = ", v_folio
                  
                  -- se obtienen los registros para el reporte
                  PREPARE sid_reporte FROM v_query
                  DECLARE cur_reporte CURSOR FOR sid_reporte
                  
                  -- llena el arreglo
                  LET v_indice = 1
                  
                  FOREACH cur_reporte INTO 
                        arr_reporte[v_indice].v_folio              ,
                           arr_reporte[v_indice].v_rfc             ,
                           arr_reporte[v_indice].v_tanto_normal    ,
                           arr_reporte[v_indice].v_tanto_adicional ,
                           arr_reporte[v_indice].v_tipo            ,
                           arr_reporte[v_indice].v_nombre                            
                        
                     LET v_indice = v_indice + 1
                  END FOREACH

                  -- elimina ultimo renglon en blanco
                  CALL arr_reporte.deleteElement(arr_reporte.getLength())

                  -- Recupera la ruta de listados en el que se enviara el archivo
                  CALL fn_rutas("ret") RETURNING v_ruta_ejecutable, v_ruta_listados 
    
                  -- Se asigna la plantilla para generar el reporte
                  IF ( fgl_report_loadCurrentSettings("../../ret/bin/RETC192.4rp") ) THEN 
                     CALL fgl_report_selectDevice ("PDF")
                       
                     LET v_ruta_reporte = v_ruta_ejecutable CLIPPED,"/","detalle_saldos_insuf"                
                     CALL fgl_report_setOutputFileName(v_ruta_reporte)
                     CALL fgl_report_selectPreview(1)
                     LET manejador_rpt = fgl_report_commitCurrentSettings()
                  ELSE         
                     CALL fn_mensaje("Atención","No fue posible generar el reporte. No se encuentra la plantilla RETC190.4rp", "stop")
                     CONTINUE DISPLAY 
                  END IF   

                  --Inicia el reporte de registros con rechazo
                  START REPORT rpt_detalle_saldos_sobregiro TO XML HANDLER manejador_rpt
                  
                  FOR v_indice_reporte = 1 TO arr_reporte.getLength()
                     OUTPUT TO REPORT rpt_detalle_saldos_sobregiro(v_indice_reporte, arr_reporte[v_indice_reporte].*, p_usuario_cod)
                  END FOR
                  
                  FINISH REPORT rpt_detalle_saldos_sobregiro

            END DISPLAY
            
            CLOSE WINDOW w_detalle_consulta_saldos
         END IF           
         
      ON ACTION CANCEL
         EXIT INPUT
   
   END INPUT
   
   CLOSE WINDOW w_consulta_saldos

END FUNCTION

{ ======================================================================
Clave: 
Nombre: rpt_detalle_saldos_sobregiro
Fecha creacion: Junio 20, 2012
Autor: Erick Rodriguez
Narrativa del proceso que realiza:
Genera el reporte de los saldos      

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
REPORT rpt_detalle_saldos_sobregiro(p_indice, v_r_despliegue, p_usuario_cod)
DEFINE    v_r_despliegue          RECORD -- registro de consulta
            v_folio                LIKE glo_folio.folio,
            v_rfc                  LIKE ret_fondo_ahorro.rfc,
            v_tanto_normal         DECIMAL(24,2),
            v_tanto_adicional      DECIMAL(24,2),
            v_tipo                 CHAR(5),   
            v_nombre               LIKE afi_fondo72.nombre
          END RECORD,
          p_indice                DECIMAL(9,0),
          p_usuario_cod           LIKE seg_usuario.usuario_cod, -- usuario en linea
          v_fecha                 STRING,                       -- fecha de emision del reporte
          v_nombre_usuario        VARCHAR(100),

          -- variables para acumular GRAN TOTAL
          v_total_tanto_normal           DECIMAL(24,6),
          v_total_tanto_adicional        DECIMAL(24,6),          
          p_total_regs                   DECIMAL(9,0),
          v_fecha_carga                  STRING
          
FORMAT

   FIRST PAGE HEADER
      
      -- variables para acumular gran total
      LET v_total_tanto_normal          = 0
      LET v_total_tanto_adicional       = 0
      LET p_total_regs                  = 0


      -- se envia folio, usuario y fecha
      LET v_fecha = TODAY USING "dd-mm-yyyy"
      -- se obtiene el nombre del usuario
      SELECT usuario_desc
      INTO v_nombre_usuario
      FROM seg_usuario
      WHERE usuario_cod = p_usuario_cod

      LET v_nombre_usuario = v_nombre_usuario CLIPPED      
      PRINTX p_usuario_cod, v_fecha, v_nombre_usuario
      
      PRINTX v_folio

   ON EVERY ROW
      PRINTX v_r_despliegue.*
      DISPLAY v_r_despliegue.*
      
      -- se acumulan los montos para gran total
      LET v_total_tanto_normal       = v_total_tanto_normal       + v_r_despliegue.v_tanto_normal      
      LET v_total_tanto_adicional    = v_total_tanto_adicional    + v_r_despliegue.v_tanto_adicional      
      LET p_total_regs               = p_total_regs               + 1

   ON LAST ROW 
      PRINTX p_total_regs             ,
             v_total_tanto_normal     ,
             v_total_tanto_adicional

END REPORT
