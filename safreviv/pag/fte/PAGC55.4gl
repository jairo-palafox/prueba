--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

#########################################################################################
#Modulo       => pag                                                                    #
#Programa     => PAGC55                                                                 #
#Objetivo     => Consulta Detalle del Pago de Fondo Anterior                            #
#Fecha inicio => Febrero 22, 2013                                                       # 
#Autor        => Ivan Vega                                                              #
#Modificaiones                                                                          #
#Autor        =>                                                                        #
#Fecha        =>                                                                        #
#Modificación =>                                                                        #
#                                                                                       #
#                                                                                       #
#########################################################################################
DATABASE safre_viv
GLOBALS "PAGG01.4gl"
GLOBALS
DEFINE g_folio_param   LIKE glo_folio.folio,
       g_id_referencia LIKE cta_his_pagos.id_referencia
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

   -- parametros optativos
   LET g_folio_param    = ARG_VAL(4)
   LET g_id_referencia  = ARG_VAL(5)
  
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   -- consulta de informacion recibida Fondo Anterior
   CALL fn_consulta_registros(p_usuario_cod)

END MAIN

{ ============================================================================
Nombre: fn_consulta_registros
Fecha creacion: Febrero 22, 2013
Autor: Ivan Vega               
Narrativa del proceso que realiza:
Presenta la pantalla para elegir el folio del que se consultaran los datos
de detalle de pago de Fondo Anterior

Registro de modificaciones:
Autor            Fecha                 Descripcion
HCRG             07-05-2013            se separa el dialog en una funcion diferente
                                       y tener mayor control en la ejecución ya que al
                                       no encontrar registros en pag_det_fa se terminaba
                                       la ejecución
==============================================================================
}
FUNCTION fn_consulta_registros(p_usuario_cod)
DEFINE p_usuario_cod       LIKE seg_usuario.usuario_cod, -- clave del usuario       
       v_query             STRING,
       v_nss               LIKE afi_derechohabiente.nss,
       v_id_afi_fondo72    LIKE afi_fondo72.id_afi_fondo72,
       v_nss_afi_fondo72   LIKE afi_fondo72.nss,
       v_ventana           ui.WINDOW,
       r_continua          BOOLEAN,
       v_conteo            SMALLINT
       
   -- se abre la ventana para captura de parametros
   OPEN WINDOW w_consulta_registros WITH FORM "../../pag/bin/PAGC551"
      LET  v_ventana = UI.WINDOW.GETCURRENT()
      CALL v_ventana.SETTEXT("Consulta detalle de pago Fondo Anterior")

      -- si no se recibio folio e id_referencia como parametros se captura normalmente
      IF ( g_folio_param IS NULL AND g_id_referencia IS NULL ) THEN
   
         INPUT v_nss FROM ed_nss ATTRIBUTES(UNBUFFERED, WITHOUT DEFAULTS)
   
            ON ACTION ACCEPT
               -- modificación de validación de  captura de parametros
               -- valida que se  el NSS no sea nulo
               IF ( v_nss IS NULL ) THEN -- OR (v_folio IS NULL) THEN 
         	      CALL fn_mensaje("Atención", "Debe de ingresar un NSS","stop")
                  NEXT FIELD ed_nss
               ELSE
                  LET v_conteo = 0
                  --LET v_query = "\nSELECT id_afi_fondo72",
                  LET v_query = "\nSELECT COUNT(*)",
                                "\nFROM   afi_fondo72",
                                "\nWHERE  nss = '",v_nss,"'"
               
                  PREPARE prp_id_derechohab FROM v_query
                  --EXECUTE prp_id_derechohab INTO v_id_afi_fondo72
                  EXECUTE prp_id_derechohab INTO v_conteo
                  
                  --IF ( v_id_afi_fondo72 IS NULL ) OR v_id_afi_fondo72 = 0 THEN 
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
               --LET INT_FLAG = TRUE                           
               EXIT INPUT
         END INPUT
         
      ELSE
      
         -- se recibieron los parametros
         SELECT id_afi_fondo72,
                nss
           INTO v_id_afi_fondo72,
                v_nss_afi_fondo72
           FROM pag_det_fa
          WHERE folio = g_folio_param
            AND id_referencia = g_id_referencia
      
         # Consulta el detalle de fondo72 para el v_id_afi_fondo72 recuperado
         CALL fn_consulta_detalle_fondo72(v_nss_afi_fondo72) RETURNING r_continua
      END IF
      
   CLOSE WINDOW w_consulta_registros

END FUNCTION

{ ============================================================================
Nombre: fn_consulta_detalle_fondo72
Fecha creacion: 07 Mayo 2013
Autor: Hugo Ramírez
Narrativa del proceso que realiza:
  Recupera y muestra el detalle de fondo 72 para el id_afi_fondo72 recibido

Registro de modificaciones:
Autor            Fecha                 Descripcion

==============================================================================}
--FUNCTION fn_consulta_detalle_fondo72(p_id_afi_fondo72)
FUNCTION fn_consulta_detalle_fondo72(p_nss_afi_fondo72)
DEFINE v_query             STRING,
       p_id_afi_fondo72    LIKE afi_fondo72.id_afi_fondo72,
       p_nss_afi_fondo72   LIKE afi_fondo72.nss,
       v_registros         INTEGER,
       v_indice            INTEGER,
       v_indice_aux        INTEGER,
       r_registro         RECORD
          f_pago             LIKE pag_det_fa.f_pago,
          imp_ap_fa          LIKE pag_det_fa.imp_ap_fa,
          tpo_pago           LIKE pag_cta_tpo_pago.tpo_pago_desc,
          mes_bimestre       LIKE pag_det_fa.mes_bimestre,
          nss                LIKE afi_derechohabiente.nss,
          rfc                LIKE pag_det_fa.rfc,
          nom_trabajador     LIKE pag_det_fa.nom_trabajador,
          folio              LIKE glo_folio.folio
       END RECORD,
       arr_registros       DYNAMIC ARRAY OF RECORD
          f_pago             LIKE pag_det_fa.f_pago,
          imp_ap_fa          LIKE pag_det_fa.imp_ap_fa,
          tpo_pago           LIKE pag_cta_tpo_pago.tpo_pago_desc,
          mes_bimestre       LIKE pag_det_fa.mes_bimestre,
          nss                LIKE afi_derechohabiente.nss,
          rfc                LIKE pag_det_fa.rfc,
          nom_trabajador     LIKE pag_det_fa.nom_trabajador,
          folio              LIKE glo_folio.folio
       END RECORD,
       v_manejador_rpt       om.SaxDocumentHandler,
       v_folio_titulo        LIKE glo_folio.folio,
       v_fecha_texto         STRING,
       v_continua            BOOLEAN,
       v_indice_reporte      SMALLINT


   # se iniciaiza para indicar que no se continua consultando
   LET v_continua = FALSE
   --hace el conteo de registros
   {LET v_query = "\nSELECT COUNT(*)",
                 "\nFROM pag_det_fa a",
                 "\nWHERE ",
                 "\nid_afi_fondo72 = ",p_id_afi_fondo72}
                 
   LET v_query = "\n SELECT COUNT(pag.id_afi_fondo72)",
                 "\n   FROM pag_det_fa pag JOIN afi_fondo72 afi",
                 "\n     ON pag.id_afi_fondo72 = afi.id_afi_fondo72",
                 "\n  WHERE afi.nss = ? "
   --DISPLAY " v_query  = ",v_query
   PREPARE prp_count_registros FROM v_query
   EXECUTE prp_count_registros USING p_nss_afi_fondo72
                                INTO v_registros
                               

   IF ( v_registros IS NULL ) THEN
      LET v_registros = 0
   END IF

   -- valida que se econtrarón registros
   IF ( v_registros > 0 ) THEN
      -- realizala busqueda para llenar el arreglo
      LET v_query = "SELECT                 ",
                    "\n pag.f_pago         ,",
                    "\n pag.imp_ap_fa      ,",
                    "\n tp.tpo_pago_desc   ,",
                    "\n pag.mes_bimestre   ,",
                    "\n afi.nss            ,", -- id_afi_fondo72 decimal(9,0), --> NSS
                    "\n pag.rfc            ,",
                    "\n pag.nom_trabajador ,",
                    "\n pag.folio           ",
                    "\n FROM   pag_det_fa pag, afi_fondo72 afi, pag_cta_tpo_pago tp",
                    "\n WHERE  pag.id_afi_fondo72 = afi.id_afi_fondo72",
                    --"\n AND    pag.id_afi_fondo72 = ",p_id_afi_fondo72,
                    "\n AND    afi.nss = ",p_nss_afi_fondo72,
                    "\n AND    pag.tpo_pago = tp.id_tpo_pago"

      -- si se recibio el folio e id_referencia como parametros se toman
      IF ( g_folio_param IS NOT NULL AND g_id_referencia IS NOT NULL ) THEN
         LET v_query = v_query,
                       "\n   AND  pag.folio = ", g_folio_param, 
                       "\n   AND  pag.id_referencia = ", g_id_referencia
      END IF
                     
      --DISPLAY "@QUERY: ",v_query
      PREPARE prp_registros FROM v_query
      DECLARE cur_registros CURSOR FOR prp_registros

      LET v_indice = 1
      --llen ael arreglo
      LET v_indice_aux = 1
            
      FOREACH cur_registros INTO  r_registro.*        
         -- se transfieren los registros al arreglo de despliegue
         LET arr_registros[v_indice].* = r_registro.*
        
         -- se incrementa el indice
         LET v_indice = v_indice+ 1
      END FOREACH
        
      IF ( arr_registros.getLength() < 1 ) THEN
         CALL fn_mensaje("Atención", "No se obtuvieron resultados con el NSS ingresado.", "stop") 
         LET v_continua = TRUE
      ELSE
            
         DIALOG  ATTRIBUTE(UNBUFFERED)
        
            DISPLAY ARRAY arr_registros TO tbl_registros.*
               
               -- boton de datos complementarios
               ON ACTION reporte
                  -- se obtiene el indice del retistro
                  LET v_indice = ARR_CURR()                  
                  LET r_registro.* = arr_registros[v_indice].*                  
                  -- Se asigna la plantilla para generar el reporte
                  IF ( fgl_report_loadCurrentSettings("../../pag/bin/PAGC55.4rp")) THEN 
                      LET v_manejador_rpt = fgl_report_commitCurrentSettings()
                  ELSE         
                      CALL fn_mensaje("Atención","No fue posible generar el reporte. No se encuentra la plantilla PAGC55.4rp","stop")
                      LET v_continua = TRUE
                      EXIT DIALOG
                  END IF                  
                  -- se obtienen los datos para el titulo
                  LET v_folio_titulo = r_registro.folio
                  LET v_fecha_texto  = TODAY USING "dd-mm-yyyy"
                  
                  --Inicia el reporte de registros con rechazo
                  START REPORT rpt_detalle_pagos TO XML HANDLER v_manejador_rpt
                  
                     -- Asigna el titulo del reporte                     
                     OUTPUT TO REPORT rpt_detalle_pagos(v_folio_titulo, v_fecha_texto, r_registro.*)
                  
                  FINISH REPORT rpt_detalle_pagos
                  -- se invoda la generacino del reporte

               ON ACTION cancelar
                  LET v_continua = FALSE
                  EXIT DIALOG

            END DISPLAY

         END DIALOG
               
      END IF
            
   ELSE
      LET v_continua = TRUE
      CALL fn_mensaje("Consulta",
                      "No existen registros con los criterios dados.",
                      "about")   
   END IF

   RETURN v_continua
END FUNCTION


-- OBJETIVO: Emitir el reporte de liquidacion/preliquidacion
REPORT rpt_detalle_pagos(p_folio_titulo, p_fecha_texto, r_registro)
DEFINE r_registro         RECORD
          f_pago             LIKE pag_det_fa.f_pago,
          imp_ap_fa          LIKE pag_det_fa.imp_ap_fa,
          tpo_pago           LIKE pag_cta_tpo_pago.tpo_pago_desc,
          mes_bimestre       LIKE pag_det_fa.mes_bimestre,
          nss                LIKE afi_derechohabiente.nss,
          rfc                LIKE pag_det_fa.rfc,
          nom_trabajador     LIKE pag_det_fa.nom_trabajador,
          folio              LIKE glo_folio.folio
       END RECORD,
       p_fecha_texto      STRING,
       p_folio_titulo     DECIMAL(9,0),
       v_fecha            STRING      
                                                                                                                                                                                          
FORMAT                                                                                        
                                                                                              
   FIRST PAGE HEADER
      -- folio y fecha del reporte
      PRINTX p_folio_titulo, p_fecha_texto
      
   ON EVERY ROW
      LET v_fecha = r_registro.f_pago USING "dd-mm-yyyy"
      PRINTX v_fecha                  ,
             r_registro.imp_ap_fa     ,
             r_registro.tpo_pago      ,
             r_registro.mes_bimestre  ,
             r_registro.nss           ,
             r_registro.rfc           ,
             r_registro.nom_trabajador,
             r_registro.folio          
   
END REPORT
