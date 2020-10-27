--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 05/06/2013
--==============================================================================

################################################################################
#Modulo       => PAG                                                           #
#Programa     => PAGC71                                                        #
#Objetivo     => Consulta de integración de garantia de estados y municipios   #
#Fecha inicio => 05 Junio de 2013                                              #
################################################################################
DATABASE safre_viv

DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, # clave del usuario firmado
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin

MAIN
DEFINE p_tipo_ejecucion SMALLINT, # forma como ejecutara el programa
       p_titulo_vtna    STRING    # titulo de la ventana

   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo_vtna    = ARG_VAL(3)
  
   # si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_titulo_vtna IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_titulo_vtna)
   END IF

   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = 'pag'
   
   # consulta de informacion recibida aportaciones voluntarias
   CALL fn_consulta_folio_gem()

END MAIN

{===============================================================================
Clave: PAGC61
Nombre: fn_consulta_folio_gem
Fecha creacion: 05 Junio del 2013
Autor: Hugo Ramírez
Narrativa del proceso que realiza:
 Realiza la consulta de datos cargados de Reg. de Pagos de garantia de 
 estados y municipios
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

================================================================================}
FUNCTION fn_consulta_folio_gem()
DEFINE v_folio DECIMAL(9,0) # folio a consultar
       
   OPEN WINDOW w_consulta_folio WITH FORM v_ruta_ejecutable CLIPPED||"/PAGC711"
     
      INPUT v_folio WITHOUT DEFAULTS FROM ed_folio    ATTRIBUTES (UNBUFFERED)

         ON ACTION ACCEPT
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

{===============================================================================
Clave: PAGC71
Nombre: fn_consulta_archivo
Fecha creacion: 17 Abril del 2013
Autor: Hugo Ramírez
Narrativa del proceso que realiza:
 Consulta los datos cargados de Reg. Pagos garantia de estados y municipios 
 para un folio dado.
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

================================================================================}
FUNCTION fn_consulta_archivo(p_folio)
DEFINE p_folio  DECIMAL(9,0), # folio
       v_cifras RECORD
        v_total_registros INTEGER,      # número de registros
        v_sum_imp_gar_gem  DECIMAL(22,2) # sumatorio de importe
       END RECORD

   # se realiza la consulta para obtener las cifras
   # Obtenemos la suma de los importes y total de los registros
   SELECT COUNT(*),
          SUM(imp_gar_gem)
     INTO v_cifras.v_total_registros,
          v_cifras.v_sum_imp_gar_gem
     FROM pag_det_fondo
    WHERE folio = p_folio

   IF( v_cifras.v_total_registros = 0 )THEN
      CALL fn_mensaje('Atención','No se encontraron resultados con el folio ingresado','stop') 
      RETURN 
   END IF 

   # se abre la ventana para mostrar los datos
   OPEN WINDOW w_consulta WITH FORM v_ruta_ejecutable CLIPPED||"/PAGC712"
      DISPLAY v_cifras.v_total_registros TO total_registros
      DISPLAY v_cifras.v_sum_imp_gar_gem TO imp_gar_gem

       MENU ""
          
          COMMAND "Reporte"
             CALL fn_reporte_carga_archivo(p_folio, v_cifras.*)

          COMMAND "Cancelar"
             EXIT MENU
             
       END MENU
   
   CLOSE WINDOW w_consulta

END FUNCTION

{===============================================================================
Clave: PAGC61
Nombre: fn_reporte_carga_archivo
Fecha creacion: 05 Junio del 2013
Autor: Hugo Ramírez
Narrativa del proceso que realiza:
 Obtener los datos necesarios para emitir el reporte de garantia de estados
 y municipios
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

================================================================================}
FUNCTION fn_reporte_carga_archivo(p_folio, p_cifras)
DEFINE p_folio                 INTEGER,
       p_cifras RECORD
        v_total_registros INTEGER,      # número de registros
        v_sum_imp_gar_gem DECIMAL(22,2) # sumatorio de importe
       END RECORD,
       v_manejador_rpt         om.SaxDocumentHandler,
       v_ruta_reporte       STRING, # ruta del archivo del reporte
       v_ruta_listados      STRING, # ruta de los listados
       --v_ruta_ejecutable    STRING, # ruta del ejecutable
       v_nombre_usuario     LIKE seg_usuario.usuario_desc,
       v_nombre_archivo     LIKE glo_ctr_archivo.nombre_archivo
    
    # Se obtiene nombre de usuario y archivo
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

    #Se asigna la plantilla para generar el reporte
    IF( fgl_report_loadCurrentSettings("PAGC71.4rp") )THEN
    
       CALL fgl_report_selectDevice ("PDF")                    
       LET v_ruta_reporte = v_ruta_listados CLIPPED,"/","carga_gem"
       CALL fgl_report_setOutputFileName(v_ruta_reporte)
       CALL fgl_report_selectPreview(1)
       LET v_manejador_rpt = fgl_report_commitCurrentSettings()
        
    ELSE
    
       DISPLAY "No fue posible generar el reporte"
       EXIT PROGRAM
        
    END IF
    
    # Inicia el reporte de registros con rechazo
    START REPORT rpt_carga_aportaciones_gem TO XML HANDLER v_manejador_rpt
       OUTPUT TO REPORT rpt_carga_aportaciones_gem(p_cifras.*, 
                                                   p_folio, 
                                                   v_nombre_usuario, 
                                                   v_nombre_archivo)                                                                
    FINISH REPORT rpt_carga_aportaciones_gem
    
END FUNCTION

{===============================================================================
Clave: PAGC71
Nombre: rpt_carga_aportaciones_gem
Fecha creacion: 05 Junio del 2013
Autor: Hugo Ramírez
Narrativa del proceso que realiza:
 Genera reporte de garantia de estados y municipios
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

================================================================================}
REPORT rpt_carga_aportaciones_gem(p_cifras, p_folio, p_nombre_usuario, p_nombre_archivo)
DEFINE p_folio  DECIMAL(9,0), 
       p_cifras RECORD
         v_total_registros INTEGER,      # número de registros
         v_sum_imp_gar_gem  DECIMAL(22,2) # sumatorio de importe
       END RECORD,
       v_fecha_reporte  DATE,
       p_nombre_usuario LIKE seg_usuario.usuario_desc,
       p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo

   FORMAT

      FIRST PAGE HEADER
         LET v_fecha_reporte = TODAY CLIPPED

         PRINTX v_fecha_reporte USING "dd-mm-yyyy"
         PRINTX p_folio
         PRINTX p_usuario_cod
         PRINTX p_nombre_usuario
         PRINTX p_nombre_archivo

      ON EVERY ROW
         PRINTX p_cifras.*

END REPORT  