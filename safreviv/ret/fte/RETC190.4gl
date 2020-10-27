--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION: 10 OCTUBRE, 2012
--===============================================================

#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETC190                                                                 #
#Objetivo     => Consulta saldos Sobregiro en solicitudes de retiro por Ajuste manual   #
#Fecha inicio => Octubre 10, 2012                                                       # 
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
   CALL fn_consulta_saldos(p_usuario_cod)

END MAIN

{ ============================================================================
Clave: RETC190
Nombre: fn_consulta_saldos
Fecha creacion: Octubre 10, 2012
Registro de modificaciones:
Descrip: CONSULTA SALDOS SOBREGIRO EN SOLICITUDES DE RETIRO POR AJUSTE MANUAL
==============================================================================
}
FUNCTION fn_consulta_saldos(p_usuario_cod)
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario
       --v_folio          DECIMAL(9,0), -- folio
       v_cbx_folios     ui.ComboBox, -- combo de folios
       
       v_s_cadena       STRING, -- cadena de texto
       v_r_glo_folio    RECORD LIKE glo_folio.*,
       
       arr_reg_saldos           DYNAMIC ARRAY OF RECORD
         v_folio                LIKE glo_folio.folio,
         v_nss                  LIKE afi_fondo72.nss,
         v_nombre_trabajador    LIKE afi_fondo72.nombre,
         v_pesos72_sol          DECIMAL(24,2),
         v_pesos72_sdo          DECIMAL(24,2),
         v_pesos72_dif          DECIMAL(24,2)         
       END RECORD,
       
       arr_reporte              DYNAMIC ARRAY OF RECORD
         v_folio                LIKE glo_folio.folio,
         v_nss                  LIKE afi_fondo72.nss,
         v_nombre_trabajador    LIKE afi_fondo72.nombre,
         v_pesos72_sol          DECIMAL(24,2),
         v_pesos72_sdo          DECIMAL(24,2),
         v_pesos72_dif          DECIMAL(24,2)
       END RECORD,
       
       v_query                       STRING, -- detalle
       v_indice                      SMALLINT, -- indice de arreglo       
       v_ruta_reporte                STRING ,-- ruta del archivo del reporte       
       v_ruta_listados               STRING ,-- ruta de los listados
       v_ruta_ejecutable             STRING ,-- ruta del ejecutable
       manejador_rpt                 om.SaxDocumentHandler ,
       v_indice_reporte              SMALLINT,
       v_id_solicitud                DECIMAL(9,0)
       
   OPEN WINDOW w_consulta_saldos WITH FORM "RETC1901"
   LET  v_ventana = UI.WINDOW.GETCURRENT()
   CALL v_ventana.SETTEXT("Consulta Saldos Sobregirados Ajuste Manual")

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
         WHERE  a.proceso_cod = g_proceso_cod_ret_fondo_ahorro_arch ---g_proceso_cod_ret_fondo_ahorro_aj_manual
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
         CALL arr_reg_saldos.clear()
         CALL arr_reporte.clear()

         --modificación de validación de  captura de parametros
         --valida que se ingrese al menos un parametro
         IF (v_folio     IS NULL OR v_folio <= 0) 
              THEN

            CALL fn_mensaje("Consulta","Debe de ingresar al menos un criterio de búsqueda","about")
            CONTINUE INPUT
         ELSE 
            -- se abre la ventana de resultados
            OPEN WINDOW w_detalle_consulta_saldos WITH FORM "RETC1902"
                     -- se limpia el arreglo de despligue del detalle
                     CALL arr_reg_saldos.clear()
                     -- se consulta del detalle de este agrupador
                     LET v_query = "\n SELECT distinct ret.folio                         ,",
                                   "\n        ret.id_solicitud                           ,",
                                   "\n        afi.nss                                    ,",
                                   "\n        afi.nombre                                 ,",
                                   "\n        ret.saldo_viv72                             ",
                                   "\n FROM afi_fondo72             afi                  ,",
                                   "\n      ret_fondo_ahorro_manual ret                  ,",
                                   "\n      ret_det_fondo72         det                   ",
                                   "\n WHERE   ret.id_solicitud = det.id_solicitud                                         ",
                                   "\n   AND afi.id_afi_fondo72 = det.id_afi_fondo72                ",
                                   "\n   AND ret.folio = ", v_folio                
                                  
                     DISPLAY v_query
                     PREPARE sid_detalle FROM v_query
                     DECLARE cur_detalle  CURSOR FOR sid_detalle
                     
                     
                     --llena el arreglo        
                     LET v_indice = 1
                     
                     FOREACH cur_detalle INTO 
                           arr_reg_saldos[v_indice].v_folio             ,
                           v_id_solicitud                               ,
                           arr_reg_saldos[v_indice].v_nss               ,
                           arr_reg_saldos[v_indice].v_nombre_trabajador ,
                           arr_reg_saldos[v_indice].v_pesos72_sol

                        -- se obtiene el saldo de la subcuenta de vivienda 92 de la tabla de insuficiencia
                        SELECT  (SUM(importe) * -1)                               
                          INTO   arr_reg_saldos[v_indice].v_pesos72_sdo
                          FROM   ret_preliquida72
                         WHERE  id_referencia   = v_id_solicitud
                           AND    folio_liquida = arr_reg_saldos[v_indice].v_folio
                           AND    subcuenta     = 40
                           AND    movimiento    = 182 
                        
                        
                        -- se verifica si se obtuvieron datos
                        IF ( arr_reg_saldos[v_indice].v_pesos72_sdo IS NULL ) THEN
                           LET arr_reg_saldos[v_indice].v_pesos72_sdo = 0
                        END IF

                        -- se calculan las diferencias
                        LET arr_reg_saldos[v_indice].v_pesos72_dif    = arr_reg_saldos[v_indice].v_pesos72_sol       - arr_reg_saldos[v_indice].v_pesos72_sdo      
                        
                        LET v_indice = v_indice + 1
                     END FOREACH
                     
                     -- se borra el ultimo registro
                     CALL arr_reg_saldos.deleteElement(arr_reg_saldos.getLength())

               DISPLAY ARRAY arr_reg_saldos TO r_saldos_insuf.*
               
               ON ACTION regresar
                  EXIT DISPLAY 
                  
               ON ACTION reporte                      
                  -- se consulta del detalle de este agrupador
                  LET v_query = "\n SELECT distinct ret.folio                            ,",
                                   "\n        ret.id_solicitud                           ,",
                                   "\n        afi.nss                                    ,",
                                   "\n        afi.nombre                                 ,",
                                   "\n        ret.saldo_viv72                             ",
                                   "\n FROM afi_fondo72             afi                  ,",
                                   "\n      ret_fondo_ahorro_manual ret                  ,",
                                   "\n      ret_det_fondo72         det                   ",
                                   "\n WHERE   ret.id_solicitud = det.id_solicitud        ",
                                   "\n   AND afi.id_afi_fondo72 = det.id_afi_fondo72      ",
                                   "\n   AND ret.folio = ", v_folio
                  
                  -- se obtienen los registros para el reporte
                  PREPARE sid_reporte FROM v_query
                  DECLARE cur_reporte CURSOR FOR sid_reporte
                  
                  -- llena el arreglo
                  LET v_indice = 1
                  
                  FOREACH cur_reporte INTO 
                        arr_reporte[v_indice].v_folio             ,
                           v_id_solicitud                               ,
                           arr_reporte[v_indice].v_nss               ,
                           arr_reporte[v_indice].v_nombre_trabajador ,
                           arr_reporte[v_indice].v_pesos72_sol                           
                        
                     -- se obtiene el saldo de la subcuenta de vivienda 92 de la tabla de insuficiencia
                        SELECT  (SUM(importe) * -1)                               
                          INTO   arr_reporte[v_indice].v_pesos72_sdo
                          FROM   ret_preliquida72
                         WHERE  id_referencia   = v_id_solicitud
                           AND    folio_liquida = arr_reporte[v_indice].v_folio
                           AND    subcuenta     = 40
                           AND    movimiento    = 182 
                        
                        
                        -- se verifica si se obtuvieron datos
                        IF ( arr_reporte[v_indice].v_pesos72_sdo IS NULL ) THEN
                           LET arr_reporte[v_indice].v_pesos72_sdo = 0
                        END IF

                        -- se calculan las diferencias
                        LET arr_reporte[v_indice].v_pesos72_dif    = arr_reporte[v_indice].v_pesos72_sol       - arr_reporte[v_indice].v_pesos72_sdo      
                        
                     LET v_indice = v_indice + 1
                  END FOREACH

                  -- elimina ultimo renglon en blanco
                  CALL arr_reporte.deleteElement(arr_reporte.getLength())

                  -- Recupera la ruta de listados en el que se enviara el archivo
                  CALL fn_rutas("ret") RETURNING v_ruta_ejecutable, v_ruta_listados 
    
                  -- Se asigna la plantilla para generar el reporte
                  IF ( fgl_report_loadCurrentSettings("../../ret/bin/RETC190.4rp") ) THEN 
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
            v_nss                  LIKE afi_fondo72.nss,
            v_nombre_trabajador    LIKE afi_fondo72.nombre,
            v_pesos72_sol          DECIMAL(24,2),
            v_pesos72_sdo          DECIMAL(24,2),
            v_pesos72_dif          DECIMAL(24,2)   
          END RECORD,
          p_indice                DECIMAL(9,0),
          p_usuario_cod           LIKE seg_usuario.usuario_cod, -- usuario en linea
          v_fecha                 STRING,                       -- fecha de emision del reporte
          v_nombre_usuario        VARCHAR(100),

          -- variables para acumular GRAN TOTAL
          v_total_pesos72_sol           DECIMAL(24,6),
          v_total_pesos72_sdo           DECIMAL(24,6),          
          v_total_pesos72_dif           DECIMAL(24,6),
          p_total_regs                  DECIMAL(9,0),
          v_fecha_carga                  STRING
          
FORMAT

   FIRST PAGE HEADER
      
      -- variables para acumular gran total
      LET v_total_pesos72_sol       = 0
      LET v_total_pesos72_sdo       = 0
      LET v_total_pesos72_dif       = 0


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
      LET v_total_pesos72_sol       = v_total_pesos72_sol       + v_r_despliegue.v_pesos72_sol      
      LET v_total_pesos72_sdo       = v_total_pesos72_sdo       + v_r_despliegue.v_pesos72_sdo      
      LET v_total_pesos72_dif       = v_total_pesos72_dif       + v_r_despliegue.v_pesos72_dif
      LET p_total_regs              = p_total_regs             + 1

   ON LAST ROW 
      PRINTX p_total_regs             ,
             v_total_pesos72_sol      ,
             v_total_pesos72_sdo      ,
             v_total_pesos72_dif 

END REPORT
