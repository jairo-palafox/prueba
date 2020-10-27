--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--==============================================================================

################################################################################
#Modulo       => pag                                                           #
#Programa     => PAGC65                                                        #
#Objetivo     => Consulta Detalle del Pago de voluntarias                      #
#Fecha inicio => Mayo 27, 2013                                                 # 
#Autor        => Hugo Ramírez                                                  #
#Modificaiones                                                                 #
################################################################################
DATABASE safre_viv
GLOBALS "PAGG01.4gl"

DEFINE g_folio_param     LIKE glo_folio.folio,
       g_id_referencia   LIKE cta_his_pagos.id_referencia,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_ruta_listados   LIKE seg_modulo.ruta_listados,
       p_usuario_cod     LIKE seg_usuario.usuario_cod, # clave del usuario firmado
       p_tipo_ejecucion  SMALLINT, # forma como ejecutara el programa
       p_titulo_vtna     STRING    # titulo de la ventana


MAIN

   # se recupera la clave de usuario desde parametro 
   # argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo_vtna    = ARG_VAL(3)

   # parametros optativos
   LET g_folio_param    = ARG_VAL(4)
   LET g_id_referencia  = ARG_VAL(5)
  
   # si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_titulo_vtna IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_titulo_vtna)
   END IF

   SELECT ruta_bin
          ruta_listados
     INTO v_ruta_ejecutable,
          v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = "pag"
   
   # consulta de informacion recibida voluntarias
   CALL fn_consulta_registros(p_usuario_cod)

END MAIN

{ ==============================================================================
Nombre: fn_consulta_registros
Fecha creacion: Mayo 27, 2013
Autor: Hugo Ramírez
Narrativa del proceso que realiza:
Presenta la pantalla para elegir el folio del que se consultaran los datos
de detalle de pago de Voluntarias
Registro de modificaciones:
Autor            Fecha                 Descripcion

================================================================================}
FUNCTION fn_consulta_registros(p_usuario_cod)
DEFINE p_usuario_cod         LIKE seg_usuario.usuario_cod, -- clave del usuario       
       v_consulta            STRING,
       v_nss                 LIKE afi_derechohabiente.nss,
       v_id_derechohabiente  LIKE afi_fondo72.id_afi_fondo72,
       v_nss_derechohabiente LIKE afi_fondo72.nss,
       v_ventana             ui.WINDOW,
       r_continua            BOOLEAN,
       v_conteo              SMALLINT
       
   # se abre la ventana para captura de parametros
   OPEN WINDOW w_consulta_registros WITH FORM v_ruta_ejecutable CLIPPED||"/PAGC651"
      LET  v_ventana = UI.WINDOW.GETCURRENT()
      CALL v_ventana.SETTEXT(p_titulo_vtna)

      # si no se recibio folio e id_referencia como parametros se captura normalmente
      IF ( g_folio_param IS NULL AND g_id_referencia IS NULL ) THEN
   
         INPUT v_nss FROM ed_nss ATTRIBUTES(UNBUFFERED, WITHOUT DEFAULTS)
   
            ON ACTION ACCEPT
               # modificación de validación de  captura de parametros
               # valida que se  el NSS no sea nulo
               IF ( v_nss IS NULL ) THEN -- OR (v_folio IS NULL) THEN 
         	      CALL fn_mensaje("Atención", "Debe de ingresar un NSS","stop")
                  NEXT FIELD ed_nss
               ELSE
                  LET v_conteo = 0
                  LET v_consulta = "\n SELECT COUNT(*)",
                                   "\n   FROM afi_derechohabiente",
                                   "\n  WHERE nss = ?"
               
                  PREPARE prp_id_derechohab FROM v_consulta
                  EXECUTE prp_id_derechohab USING v_nss 
                                             INTO v_conteo

                  IF ( v_conteo IS NULL ) OR v_conteo = 0 THEN 
                     CALL fn_mensaje("Atención", "No existen registros para el NSS ingresado","about")
                     CONTINUE INPUT
                  ELSE
                     LET INT_FLAG = FALSE
                     # Consulta el detalle de fondo72 para el v_id_afi_fondo72 recuperado
                     CALL fn_consulta_detalle_fondo72(v_nss) RETURNING r_continua
                     IF(r_continua)THEN
                        CONTINUE INPUT
                     ELSE
                        EXIT INPUT
                     END IF
                     
                  END IF
               END IF 
         
            ON ACTION CANCEL
               EXIT INPUT
               
         END INPUT
         
      ELSE
      
         # se recibieron los parametros
         LET v_consulta = "\n SELECT vol.id_derechohabiente,",
                          "\n        afi.nss",
                          "\n   FROM pag_det_apvol vol JOIN afi_derechohabiente afi",
                          "\n     ON afi.id_derechohabiente = vol.id_derechohabiente",           
                          "\n  WHERE vol.folio = ?",
                          "\n    AND vol.id_referencia = ?"

         PREPARE prp_rec_nss FROM v_consulta
         EXECUTE prp_rec_nss USING g_folio_param,
                                   g_id_referencia
                              INTO v_id_derechohabiente,
                                   v_nss_derechohabiente 
      
         # Consulta el detalle de fondo72 para el v_id_afi_fondo72 recuperado
         CALL fn_consulta_detalle_fondo72(v_nss_derechohabiente) RETURNING r_continua
      END IF
      
   CLOSE WINDOW w_consulta_registros

END FUNCTION

{===============================================================================
Nombre: fn_consulta_detalle_voluntarias
Fecha creacion: 27 Mayo 2013
Autor: Hugo Ramírez
Narrativa del proceso que realiza:
  Recupera y muestra el detalle de voluntarias para el nss recibido
Registro de modificaciones:
Autor            Fecha                 Descripcion
================================================================================}
FUNCTION fn_consulta_detalle_fondo72(p_nss_afi_derechohabiente)
DEFINE v_consulta        STRING,
       p_nss_afi_derechohabiente LIKE afi_derechohabiente.nss,
       v_registros       INTEGER,
       v_indice          INTEGER,
       r_registro        RECORD
          nss            LIKE afi_derechohabiente.nss,
          curp           LIKE afi_derechohabiente.curp,
          f_pago         LIKE pag_det_fa.f_pago,
          cve_entidad    LIKE pag_det_apvol.cve_entidad,
          id_pago        LIKE pag_det_apvol.id_pago,
          f_liquida      LIKE cta_movimiento.f_liquida,
          precio_fondo   LIKE glo_valor_fondo.precio_fondo,
          imp_ap_vol     LIKE pag_det_apvol.imp_ap_vol,
          aiv_ap_vol     LIKE pag_det_apvol.imp_ap_vol,
          nom_trabajador LIKE pag_det_fa.nom_trabajador,
          folio          LIKE glo_folio.folio
       END RECORD,
       v_registros_nss   DYNAMIC ARRAY OF RECORD
          nss            LIKE afi_derechohabiente.nss,
          curp           LIKE afi_derechohabiente.curp,        
          f_pago         LIKE pag_det_fa.f_pago,
          cve_entidad    LIKE pag_det_apvol.cve_entidad,
          id_pago        LIKE pag_det_apvol.id_pago,
          f_liquida      LIKE cta_movimiento.f_liquida,
          precio_fondo   LIKE glo_valor_fondo.precio_fondo,          
          imp_ap_vol     LIKE pag_det_apvol.imp_ap_vol,
          aiv_ap_vol     LIKE pag_det_apvol.imp_ap_vol,
          nom_trabajador LIKE pag_det_fa.nom_trabajador,
          folio          LIKE glo_folio.folio
       END RECORD,
       v_manejador_rpt       om.SaxDocumentHandler,
       v_folio_titulo        LIKE glo_folio.folio,
       v_fecha_texto         STRING,
       v_continua            BOOLEAN,
       v_indice_reporte      SMALLINT,
       v_nombre_reporte      STRING


   # se iniciaiza para indicar que no se continua consultando
   LET v_continua = FALSE
   # hace el conteo de registros
   LET v_consulta = "\n SELECT COUNT(pag.id_derechohabiente)",
                    "\n   FROM pag_det_apvol pag JOIN afi_derechohabiente afi",
                    "\n     ON pag.id_derechohabiente = afi.id_derechohabiente",
                    "\n  WHERE afi.nss = ? "
   PREPARE prp_count_registros FROM v_consulta
   EXECUTE prp_count_registros USING p_nss_afi_derechohabiente
                                INTO v_registros

   IF ( v_registros IS NULL ) THEN
      LET v_registros = 0
   END IF

   # valida que se econtrarón registros
   IF ( v_registros > 0 ) THEN
      # realizala busqueda para llenar el arreglo
      LET v_consulta = "\n SELECT afi.nss            ,",
                       "\n        pag.curp           ,",
                       "\n        pag.f_pago         ,",
                       "\n        pag.cve_entidad    ,",
                       "\n        pag.id_pago        ,",
                       "\n        mov.f_liquida      ,",
                       "\n        glo.precio_fondo   ,",
                       "\n        pag.imp_ap_vol     ,",
                       "\n        mov.monto_acciones ,",
                       "\n        pag.nom_trabajador ,",
                       "\n        pag.folio           ",
                       "\n   FROM pag_det_apvol pag, afi_derechohabiente afi, ",
                       "\n        pag_cta_tpo_pago tp, cta_movimiento mov, glo_valor_fondo glo ",
                       "\n  WHERE pag.id_derechohabiente = afi.id_derechohabiente",
                       "\n  AND   afi.nss                = ",p_nss_afi_derechohabiente,
                       "\n  AND   pag.tpo_pago           = tp.id_tpo_pago",
                       "\n  AND   pag.folio              = mov.folio_liquida ",
                       "\n  AND   pag.id_referencia      = mov.id_referencia ",
                       "\n  AND   glo.fondo              = mov.fondo_inversion ",
                       "\n  AND   glo.f_valuacion        = mov.f_liquida "

      # si se recibio el folio e id_referencia como parametros se toman
      IF ( g_folio_param IS NOT NULL AND g_id_referencia IS NOT NULL ) THEN
         LET v_consulta = v_consulta,
                          "\n   AND  pag.folio = ", g_folio_param, 
                          "\n   AND  pag.id_referencia = ", g_id_referencia
      END IF

      PREPARE prp_registros FROM v_consulta
      DECLARE cur_registros CURSOR FOR prp_registros

      LET v_indice = 1
      FOREACH cur_registros INTO  r_registro.*        
         # se transfieren los registros al arreglo de despliegue
         LET v_registros_nss[v_indice].* = r_registro.*
        
         # se incrementa el indice
         LET v_indice = v_indice + 1
      END FOREACH
        
      IF ( v_registros_nss.getLength() < 1 ) THEN
         CALL fn_mensaje("Atención", "No se obtuvieron resultados con el NSS ingresado", "stop") 
         LET v_continua = TRUE
      ELSE

         DIALOG  ATTRIBUTE(UNBUFFERED)
        
            DISPLAY ARRAY v_registros_nss TO tbl_registros.*
               
               # boton de datos complementarios
               ON ACTION reporte
                  # se obtiene el indice del retistro
                  LET v_indice = ARR_CURR()                  
                  LET r_registro.* = v_registros_nss[v_indice].*                  
                  # Se asigna la plantilla para generar el reporte
                  IF ( fgl_report_loadCurrentSettings(v_ruta_ejecutable CLIPPED||"/PAGC65.4rp")) THEN
                      CALL fgl_report_selectDevice ("PDF")
                      LET v_nombre_reporte = v_ruta_listados CLIPPED,"/","detalle_pago_voluntarias"
                      CALL fgl_report_setOutputFileName(v_nombre_reporte)
                      CALL fgl_report_selectPreview(1) 
                      LET v_manejador_rpt = fgl_report_commitCurrentSettings()
                  ELSE         
                      CALL fn_mensaje("Atención","No fue posible generar el reporte. No se encuentra la plantilla PAGC65.4rp","stop")
                      LET v_continua = TRUE
                      EXIT DIALOG
                  END IF                  
                  # se obtienen los datos para el titulo
                  LET v_folio_titulo = r_registro.folio
                  LET v_fecha_texto  = TODAY USING "dd-mm-yyyy"

                  # Inicia el reporte de registros con rechazo
                  START REPORT fn_genera_rpt_detalle_pagos TO XML HANDLER v_manejador_rpt
                  
                     # Asigna el titulo del reporte                     
                     OUTPUT TO REPORT fn_genera_rpt_detalle_pagos(v_folio_titulo, v_fecha_texto, r_registro.*)
                  
                  FINISH REPORT fn_genera_rpt_detalle_pagos
                  # se invoda la generacino del reporte

               ON ACTION cancelar
                  LET v_continua = FALSE
                  EXIT DIALOG

            END DISPLAY

         END DIALOG
               
      END IF
            
   ELSE
      LET v_continua = TRUE
      CALL fn_mensaje("Consulta",
                      "No existen registros con los criterios dados",
                      "about")   
   END IF

   RETURN v_continua
END FUNCTION


{===============================================================================
Nombre: fn_genera_rpt_detalle_pagos
Fecha creacion: 27 Mayo 2013
Autor: Hugo Ramírez
Narrativa del proceso que realiza:
 Emitir el reporte de liquidacion/preliquidacion
Registro de modificaciones:
Autor            Fecha                 Descripcion
================================================================================}
REPORT fn_genera_rpt_detalle_pagos(p_folio_titulo, p_fecha_texto, r_registro)
       DEFINE r_registro        RECORD
          nss            LIKE afi_derechohabiente.nss,
          curp           LIKE afi_derechohabiente.curp,
          f_pago         LIKE pag_det_fa.f_pago,
          cve_entidad    LIKE pag_det_apvol.cve_entidad,
          id_pago        LIKE pag_det_apvol.id_pago,
          f_liquida      LIKE cta_movimiento.f_liquida,
          precio_fondo   LIKE glo_valor_fondo.precio_fondo,
          imp_ap_vol     LIKE pag_det_apvol.imp_ap_vol,
          aiv_ap_vol     LIKE pag_det_apvol.imp_ap_vol,
          nom_trabajador LIKE pag_det_fa.nom_trabajador,
          folio          LIKE glo_folio.folio
       END RECORD,
       
       p_fecha_texto      STRING,
       p_folio_titulo     DECIMAL(9,0),
       v_fecha            STRING,
       v_fecha_liq        STRING
       
                                                                                                                                                                                          
   FORMAT                                                                                        
                                                                                              
      FIRST PAGE HEADER
         # folio y fecha del reporte
         PRINTX p_folio_titulo, p_fecha_texto
      
      ON EVERY ROW
         LET v_fecha     = r_registro.f_pago    USING "dd-mm-yyyy"
         LET v_fecha_liq = r_registro.f_liquida USING "dd-mm-yyyy"
         
         PRINTX r_registro.nss           ,
                r_registro.curp          ,
                v_fecha                  ,
                r_registro.cve_entidad   ,
                r_registro.id_pago       ,
                v_fecha_liq              ,
                r_registro.precio_fondo  ,
                r_registro.imp_ap_vol    ,
                r_registro.aiv_ap_vol    ,
                r_registro.nom_trabajador,
                r_registro.folio          
   
END REPORT
