--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 25/10/2012
--==============================================================================

################################################################################
#Modulo       => SEP                                                           #
#Programa     => SEPC28                                                        #
#Objetivo     => Consulta de deudor                                            #
#Fecha inicio => 25 Octubre 2012                                               #
################################################################################

DATABASE safre_viv

DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion   INTEGER,
       p_cad_ventana     STRING,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_ventana         ui.Window,
       v_forma           ui.Form,
       v_deudores        DYNAMIC ARRAY OF RECORD
          --v_id_restitucion     LIKE sep_mov_deudor.id_restitucion,
          v_id_derechohabiente LIKE sep_mov_deudor.id_derechohabiente,
          v_nss                LIKE afi_derechohabiente.nss,
          v_nombre             VARCHAR(200),--LIKE afi_derechohabiente.nombre_af,
          v_monto_pesos        LIKE sep_mov_deudor.monto_pesos
       END RECORD,
       v_detalle_deudor     DYNAMIC ARRAY OF RECORD
          v_f_liquida       LIKE sep_mov_deudor.f_liquida,
          v_folio_liquida   LIKE sep_mov_deudor.folio_liquida,
          v_movimiento      LIKE cat_movimiento.movimiento_desc,
          v_monto_pesos     LIKE sep_mov_deudor.monto_pesos
       END RECORD,
       v_deudores_rpt RECORD
          v_id_derechohabiente LIKE sep_mov_deudor.id_derechohabiente,
          v_nss                LIKE afi_derechohabiente.nss,
          v_nombre             VARCHAR(200),--LIKE afi_derechohabiente.nombre_af,
          v_monto_pesos_gen    LIKE sep_mov_deudor.monto_pesos,
          v_f_liquida          LIKE sep_mov_deudor.f_liquida,
          v_folio_liquida      LIKE sep_mov_deudor.folio_liquida,
          v_movimiento         LIKE cat_movimiento.movimiento_desc,
          v_monto_pesos        LIKE sep_mov_deudor.monto_pesos
       END RECORD,
       
       v_nom_reporte             STRING,
       v_total_restitucion_sol   LIKE sep_mov_deudor.monto_pesos,
       v_total_deudor_compensado LIKE sep_mov_deudor.monto_pesos,
       v_total_activo            LIKE sep_mov_deudor.monto_pesos

MAIN
   # recupera parámetros
   LET p_usuario_cod   = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_cad_ventana   = ARG_VAL(3)

   CALL fn_consulta_deudor()

END MAIN

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPC28                                                   #
#Descripcion       => Consulta deudor                                          #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 25 Octubre 2012                                          #
################################################################################
FUNCTION fn_consulta_deudor()
DEFINE v_tpo_consulta    SMALLINT,
       v_continua        BOOLEAN

   LET v_continua = TRUE
   # se recupera la ruta ejecutable del módulo
   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = 'sep'

   OPEN WINDOW vtna_tpo_consulta WITH FORM v_ruta_ejecutable CLIPPED||"/SEPC281"
      #Se asigna el titulo de la ventana
      LET v_ventana = ui.Window.getCurrent()
      LET v_forma = v_ventana.getForm() 
      IF(p_cad_ventana IS NOT NULL)THEN
         CALL ui.Interface.setText(p_cad_ventana)         
         CALL v_ventana.setText(p_cad_ventana)
      END IF
      WHILE v_continua
         INPUT v_tpo_consulta FROM rdgpo_tpo_consulta 
               ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE, UNBUFFERED)

            BEFORE INPUT 
               LET v_ventana = ui.Window.getCurrent()
               LET v_forma = v_ventana.getForm() 
               CALL v_forma.setElementHidden("gpo_capturar",TRUE)
               CALL v_forma.setElementHidden("tbl_deudores",TRUE)
               CALL v_forma.setElementHidden("gpo_cifras",TRUE)
               CALL v_forma.setElementHidden("gpo_reporte",TRUE)
               # inicializa a consulta detallada
               LET v_tpo_consulta = 1 

            ON ACTION aceptar
               CALL fn_determina_busqueda(v_tpo_consulta,v_forma)
               ACCEPT INPUT
 
            ON ACTION cancelar
               LET v_continua = FALSE
               EXIT INPUT

         END INPUT
      END WHILE
      

   CLOSE WINDOW vtna_tpo_consulta 

END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPC28                                                   #
#Descripcion       => Determina tipo de consulta                               #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 25 Octubre 2012                                          #
################################################################################
FUNCTION fn_determina_busqueda(p_tpo_consulta,p_forma)
DEFINE p_tpo_consulta SMALLINT,
       p_forma        ui.Form

    CASE p_tpo_consulta
    
       WHEN 1
          CALL p_forma.setElementHidden("gpo_capturar",FALSE)
          CALL fn_consulta_detalle(p_forma)

       WHEN 2
          CALL p_forma.setElementHidden("gpo_cifras",FALSE)
          CALL fn_consulta_global(p_forma)

    END CASE 

END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPC28                                                   #
#Descripcion       => recupera filtros para detalle de movimientos             #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 25 Octubre 2012                                          #
################################################################################
FUNCTION fn_consulta_detalle(p_forma)
DEFINE p_forma               ui.Form,
       # filtros de busqueda
       v_filtro_nss          LIKE afi_derechohabiente.nss,
       v_filtro_periodo_ini  LIKE sep_mov_deudor.f_liquida,
       v_filtro_periodo_fin  LIKE sep_mov_deudor.f_liquida,
       v_filtro_folio        LIKE sep_mov_deudor.folio_liquida,
       v_filtro_deudor       BOOLEAN,
       v_filtro_compensacion BOOLEAN,
       # -----
       r_filtro              STRING
       

   INITIALIZE v_filtro_nss, 
              v_filtro_periodo_ini,
              v_filtro_periodo_fin,
              v_filtro_folio,
              v_filtro_deudor,
              v_filtro_compensacion TO NULL
              
   INPUT v_filtro_nss, 
         v_filtro_periodo_ini,
         v_filtro_periodo_fin,
         v_filtro_folio,
         v_filtro_deudor,
         v_filtro_compensacion --WITHOUT DEFAULTS
    FROM nss,
         periodo_inicio,
         periodo_fin,
         folio,
         deudor,
         compensacion ATTRIBUTES(ACCEPT = FALSE, CANCEL =  FALSE, UNBUFFERED)

      BEFORE INPUT
         INITIALIZE v_filtro_folio TO NULL 
         LET v_filtro_deudor = 0
         LET v_filtro_compensacion = 0

      AFTER FIELD nss
         IF(v_filtro_nss IS NOT NULL AND 
            LENGTH(v_filtro_nss CLIPPED) <> 11)THEN
            CALL fn_mensaje("Aviso","NSS no valido","information")
            NEXT FIELD nss
         END IF

      AFTER FIELD periodo_fin
         IF(v_filtro_periodo_ini > v_filtro_periodo_fin)THEN
            CALL fn_mensaje("Aviso","Periodo inicio no puede ser mayor a periodo fin","information")
            NEXT FIELD periodo_fin 
         END IF

      AFTER FIELD folio
        IF(v_filtro_folio = 0)THEN
            CALL fn_mensaje("Aviso","Folio no valido","information")
            NEXT FIELD folio 
         END IF

      ON ACTION consultar
         IF(v_filtro_nss IS NOT NULL AND 
            LENGTH(v_filtro_nss CLIPPED) <> 11)THEN
            CALL fn_mensaje("Aviso","NSS no valido","information")
            NEXT FIELD nss
         END IF
         IF(v_filtro_periodo_ini > v_filtro_periodo_fin)THEN
            CALL fn_mensaje("Aviso","Periodo inicio no puede ser mayor a periodo fin","information")
            NEXT FIELD periodo_fin 
         END IF
         IF(v_filtro_folio = 0)THEN
            CALL fn_mensaje("Aviso","Folio no valido","information")
            NEXT FIELD folio 
         END IF
         
         INITIALIZE r_filtro TO NULL
         CALL fn_recupera_deudores(v_filtro_nss,
                                   v_filtro_periodo_ini,
                                   v_filtro_periodo_fin,
                                   v_filtro_folio,
                                   v_filtro_deudor,
                                   v_filtro_compensacion) RETURNING r_filtro
         IF(v_deudores.getLength() > 0)THEN
            CALL p_forma.setElementHidden("tbl_deudores",FALSE)
            CALL p_forma.setElementHidden("gpo_cifras",FALSE)
            CALL fn_consulta_detalle_deudores(v_filtro_nss,
                                              v_filtro_periodo_ini,
                                              v_filtro_periodo_fin,
                                              v_filtro_folio,
                                              v_filtro_deudor,
                                              v_filtro_compensacion,
                                              p_forma,
                                              r_filtro)
         ELSE
            CALL fn_mensaje("Aviso","No se encontraron registros con criterio dado","information")
            CONTINUE INPUT
         END IF
         ACCEPT INPUT

      ON ACTION cancelar
         INITIALIZE v_filtro_nss, 
                    v_filtro_periodo_ini,
                    v_filtro_periodo_fin,
                    v_filtro_folio,
                    v_filtro_deudor,
                    v_filtro_compensacion TO NULL
         EXIT INPUT

   END INPUT

END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPC28                                                   #
#Descripcion       =>              #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 25 Octubre 2012                                          #
################################################################################
FUNCTION fn_recupera_deudores(p_nss,
                              p_periodo_ini,
                              p_periodo_fin,
                              p_folio,
                              p_deudor,
                              p_compensacion)
DEFINE v_consulta     STRING,
       v_filtro       STRING,
       p_nss          LIKE afi_derechohabiente.nss,
       p_periodo_ini  LIKE sep_mov_deudor.f_liquida,
       p_periodo_fin  LIKE sep_mov_deudor.f_liquida,
       p_folio        LIKE sep_mov_deudor.folio_liquida,
       p_deudor       BOOLEAN,
       p_compensacion BOOLEAN,       
       v_deudores_aux RECORD
          --v_id_restitucion     LIKE sep_mov_deudor.id_restitucion,
          v_id_derechohabiente LIKE sep_mov_deudor.id_derechohabiente,
          v_nss                LIKE afi_derechohabiente.nss,
          v_nombre             LIKE afi_derechohabiente.nombre_af,
          v_ap_paterno         LIKE afi_derechohabiente.ap_paterno_af,
          v_ap_materno         LIKE afi_derechohabiente.ap_materno_af,
          v_monto_pesos        LIKE sep_mov_deudor.monto_pesos,
          v_movimiento         LIKE sep_mov_deudor.movimiento
       END RECORD,
       v_indice       SMALLINT

   INITIALIZE v_filtro TO NULL
   
   IF(p_nss IS NULL         AND 
      p_periodo_ini IS NULL AND
      p_periodo_fin IS NULL AND
      p_folio IS NULL       AND 
      p_deudor = 0          AND 
      p_compensacion = 0)THEN
      
      LET v_filtro = " 1 = 1"
   ELSE
      LET v_filtro = " "
      # nss
      IF(p_nss IS NOT NULL)THEN
         LET v_filtro = "afi.nss = '",p_nss,"' AND "
      END IF
      # periodo
      IF(p_periodo_ini IS NOT NULL)THEN
         IF(p_periodo_fin IS NOT NULL)THEN
            LET v_filtro = v_filtro," mov.f_liquida BETWEEN '",p_periodo_ini,"' AND '",p_periodo_fin,"' AND "  
         ELSE
            LET v_filtro = v_filtro," mov.f_liquida = '",p_periodo_ini,"' AND "
         END IF
      ELSE
         IF(p_periodo_fin IS NOT NULL)THEN
            LET v_filtro = v_filtro," mov.f_liquida = '",p_periodo_fin,"' AND "
         END IF
      END IF
      
      # folio
      IF(p_folio > 0)THEN
         LET v_filtro = v_filtro," mov.folio_liquida = ",p_folio," AND "
      END IF
      # movimiento
      IF(p_deudor <> 0)THEN
         IF(p_compensacion <> 0)THEN
            LET v_filtro = v_filtro," mov.movimiento IN( 381,382) AND "  
         ELSE
            LET v_filtro = v_filtro," mov.movimiento = 381 AND "
         END IF
      ELSE
         IF(p_compensacion <> 0)THEN
            LET v_filtro = v_filtro," mov.movimiento = 382 AND "
         END IF
      END IF
      # elimina el ultimo AND
      LET v_filtro = v_filtro.subString(1,v_filtro.getLength()-4)

   END IF
   # limpia el arreglo   
   CALL v_deudores.clear()
   LET v_total_restitucion_sol   = 0
   LET v_total_deudor_compensado = 0
   LET v_total_activo            = 0
   LET v_indice = 1

   LET v_consulta = "\n SELECT mov.id_derechohabiente,",
                    "\n        afi.nss,",
                    "\n        afi.nombre_af,",
                    "\n        afi.ap_paterno_af,",
                    "\n        afi.ap_materno_af,",
                    "\n        SUM(mov.monto_pesos)",
                    "\n   FROM sep_mov_deudor mov JOIN afi_derechohabiente afi",
                    "\n     ON afi.id_derechohabiente = mov.id_derechohabiente",
                    "\n  WHERE ",v_filtro,
                    "\n  GROUP BY 1,2,3,4,5",
                    "\n  ORDER BY 2"
                    
   PREPARE prp_rec_sum_deudores FROM v_consulta
   DECLARE cur_rec_sum_deudores CURSOR FOR prp_rec_sum_deudores
   FOREACH cur_rec_sum_deudores INTO v_deudores_aux.v_id_derechohabiente,
                                     v_deudores_aux.v_nss,
                                     v_deudores_aux.v_nombre,
                                     v_deudores_aux.v_ap_paterno,
                                     v_deudores_aux.v_ap_materno,
                                     v_deudores_aux.v_monto_pesos
      LET v_deudores[v_indice].v_id_derechohabiente = v_deudores_aux.v_id_derechohabiente
      LET v_deudores[v_indice].v_nss                = v_deudores_aux.v_nss
      LET v_deudores[v_indice].v_nombre             = v_deudores_aux.v_nombre CLIPPED," ",v_deudores_aux.v_ap_paterno CLIPPED," ",v_deudores_aux.v_ap_materno CLIPPED
      LET v_deudores[v_indice].v_monto_pesos        = v_deudores_aux.v_monto_pesos

      LET v_indice = v_indice + 1

   END FOREACH
   FREE cur_rec_sum_deudores
   
   # Funcion que recupera cifras globales
   CALL fn_recupera_cifras_globales(v_filtro)

   RETURN v_filtro
   
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPC28                                                   #
#Descripcion       => muestra deudores                                         #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 25 Octubre 2012                                          #
################################################################################
FUNCTION fn_consulta_detalle_deudores(p_filtro_nss,
                                      p_filtro_periodo_ini,
                                      p_filtro_periodo_fin,
                                      p_filtro_folio,
                                      p_filtro_deudor,
                                      p_filtro_compensacion,
                                      p_forma,
                                      p_filtro)
DEFINE # filtros de busqueda
       p_filtro_nss          LIKE afi_derechohabiente.nss,
       p_filtro_periodo_ini  LIKE sep_mov_deudor.f_liquida,
       p_filtro_periodo_fin  LIKE sep_mov_deudor.f_liquida,
       p_filtro_folio        LIKE sep_mov_deudor.folio_liquida,
       p_filtro_deudor       BOOLEAN,
       p_filtro_compensacion BOOLEAN,
       p_forma               ui.Form,
       p_filtro              STRING

   DISPLAY ARRAY v_deudores TO sr_deudores.* ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE)

      BEFORE DISPLAY
         DISPLAY v_total_restitucion_sol   TO restitucion_solicitada
         DISPLAY v_total_deudor_compensado TO deudor_compensado
         DISPLAY v_total_activo            TO total_activo

      BEFORE ROW
         CALL fn_con_detallde_deudor(v_deudores[ARR_CURR()].v_id_derechohabiente,
                                     v_deudores[ARR_CURR()].v_nss,
                                     v_deudores[ARR_CURR()].v_nombre,
                                     p_filtro)
         

      ON ACTION reporte
         CALL fn_genera_reporte_deudores(p_filtro_nss,
                                         p_filtro_periodo_ini,
                                         p_filtro_periodo_fin,
                                         p_filtro_folio,
                                         p_filtro_deudor,
                                         p_filtro_compensacion,
                                         v_total_restitucion_sol,
                                         v_total_deudor_compensado,
                                         v_total_activo,
                                         p_filtro,
                                         1) # tipo consulta detalle
         CALL p_forma.setElementHidden("gpo_reporte",FALSE)
         
         DISPLAY "<a gwc:attributes=\"href resourceUri('"||v_nom_reporte||"','sep')\" target='_blank'><img gwc:attributes=\"src resourceuri('logo_pdf_descarga.gif','glo')\" title='Visualizar reporte'/></a>"  TO reporte
         CONTINUE DISPLAY

      ON ACTION cancelar
         CALL v_deudores.clear()
         EXIT DISPLAY

   END DISPLAY
   
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPC28                                                   #
#Descripcion       => muestra detalle de deudor por registro                   #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 25 Octubre 2012                                          #
################################################################################
FUNCTION fn_con_detallde_deudor(p_id_derechohabiente,p_nss,p_nombre,p_filtro)
DEFINE p_id_derechohabiente LIKE sep_mov_deudor.id_derechohabiente,
       p_nss                LIKE afi_derechohabiente.nss,
       p_nombre             VARCHAR(80),
       p_filtro             STRING,
       v_consulta           STRING,
       v_detalle_deudor_aux RECORD
          v_f_liquida       LIKE sep_mov_deudor.f_liquida,
          v_folio_liquida   LIKE sep_mov_deudor.folio_liquida,
          v_movimiento      LIKE sep_mov_deudor.movimiento,
          v_movimiento_desc LIKE cat_movimiento.movimiento_desc,
          v_monto_pesos     LIKE sep_mov_deudor.monto_pesos
       END RECORD,
       v_indice INTEGER

   CALL v_detalle_deudor.clear()
   LET v_indice = 1
   LET v_consulta = "\n SELECT mov.f_liquida,",
                    "\n        mov.folio_liquida,",
                    "\n        mov.movimiento,",
                    "\n        cat.movimiento_desc,",
                    "\n        mov.monto_pesos",
                    "\n   FROM sep_mov_deudor mov LEFT OUTER JOIN cat_movimiento cat",
                    "\n     ON cat.movimiento = mov.movimiento",
                    "\n        JOIN afi_derechohabiente afi",
                    "\n     ON afi.id_derechohabiente = mov.id_derechohabiente",
                    "\n  WHERE ",p_filtro,
                    "\n    AND mov.id_derechohabiente = ?" 
                    
   PREPARE prp_rec_det_deudor FROM v_consulta
   DECLARE cur_rec_det_deudor CURSOR FOR prp_rec_det_deudor
   FOREACH cur_rec_det_deudor USING p_id_derechohabiente 
                               INTO v_detalle_deudor_aux.*
      LET v_detalle_deudor[v_indice].v_f_liquida     = v_detalle_deudor_aux.v_f_liquida
      LET v_detalle_deudor[v_indice].v_folio_liquida = v_detalle_deudor_aux.v_folio_liquida
      IF(v_detalle_deudor_aux.v_movimiento_desc IS NULL)THEN
         LET v_detalle_deudor[v_indice].v_movimiento = "SIN"
      ELSE     
         LET v_detalle_deudor[v_indice].v_movimiento = v_detalle_deudor_aux.v_movimiento," - ",v_detalle_deudor_aux.v_movimiento_desc
      END IF
      LET v_detalle_deudor[v_indice].v_monto_pesos   = v_detalle_deudor_aux.v_monto_pesos
      LET v_indice = v_indice + 1
   END FOREACH 
   FREE cur_rec_det_deudor

   IF(v_detalle_deudor.getLength() > 0)THEN
      OPEN WINDOW vtna_cons_det_deudor WITH FORM v_ruta_ejecutable CLIPPED||"/SEPC282"
         ATTRIBUTES(STYLE="dialog") # se abrira como pop-up
         #Se asigna el titulo de la ventana
         LET v_ventana = ui.Window.getCurrent()
         LET v_forma = v_ventana.getForm() 
         IF(p_cad_ventana IS NOT NULL)THEN
            CALL ui.Interface.setText(p_cad_ventana)         
            CALL v_ventana.setText(p_cad_ventana)
         END IF

         DISPLAY ARRAY v_detalle_deudor TO sr_movimientos.* ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE)

            BEFORE DISPLAY 
               DISPLAY p_nss    TO nss
               DISPLAY p_nombre TO nombre

            ON ACTION aceptar
               CALL v_detalle_deudor.clear()
               ACCEPT DISPLAY

         END DISPLAY

      CLOSE WINDOW vtna_cons_det_deudor

   END IF
                    
   
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPC28                                                   #
#Descripcion       => genera reporte de deudores                               #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 25 Octubre 2012                                          #
################################################################################
FUNCTION fn_genera_reporte_deudores(p_filtro_nss,
                                    p_filtro_periodo_ini,
                                    p_filtro_periodo_fin,
                                    p_filtro_folio,
                                    p_filtro_deudor,
                                    p_filtro_compensacion,
                                    p_total_restitucion_sol,
                                    p_total_deudor_compensado,
                                    p_total_activo,
                                    p_filtro,
                                    p_tpo_consulta)
DEFINE # filtros de busqueda
       p_filtro_nss          LIKE afi_derechohabiente.nss,
       p_filtro_periodo_ini  LIKE sep_mov_deudor.f_liquida,
       p_filtro_periodo_fin  LIKE sep_mov_deudor.f_liquida,
       p_filtro_folio        LIKE sep_mov_deudor.folio_liquida,
       p_filtro_deudor       BOOLEAN,
       p_filtro_compensacion BOOLEAN,
       p_total_restitucion_sol   LIKE sep_mov_deudor.monto_pesos,
       p_total_deudor_compensado LIKE sep_mov_deudor.monto_pesos,
       p_total_activo            LIKE sep_mov_deudor.monto_pesos,
       p_filtro         STRING,
       p_tpo_consulta   SMALLINT,
       v_consulta       STRING,
       v_indice         INTEGER,
       v_indice_arr_rpt INTEGER,
       v_detalle_deudor_aux RECORD
          v_f_liquida       LIKE sep_mov_deudor.f_liquida,
          v_folio_liquida   LIKE sep_mov_deudor.folio_liquida,
          v_movimiento      LIKE sep_mov_deudor.movimiento,
          v_movimiento_desc LIKE cat_movimiento.movimiento_desc,
          v_monto_pesos     LIKE sep_mov_deudor.monto_pesos
       END RECORD,
       r_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       r_ruta_lst        LIKE seg_modulo.ruta_listados,
       v_manejador_rpt   OM.SaxDocumentHandler

   CALL fn_rutas( "sep") RETURNING r_ruta_ejecutable, r_ruta_lst
   IF(fgl_report_loadCurrentSettings(r_ruta_ejecutable CLIPPED||"/SEPC281.4rp"))THEN
      CALL fgl_report_selectDevice("PDF")
      --LET v_id_nom_rpt = p_id_expediente
      LET v_nom_reporte = p_usuario_cod CLIPPED, "-SEPC28-", 
                          "00000" USING "&&&&&", "-", 
                          "00000" USING "&&&&&", "-", 
                          "00000" USING "&&&&&",
                          ".pdf"
      # ruta de salida del reporte
      CALL fgl_report_setOutputFileName(r_ruta_lst CLIPPED||"/"||v_nom_reporte)
         
      # Indica que no hay previsualizacion
      CALL fgl_report_selectPreview(0)
      # se asigna la configuración en el menejo del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()
      
      START REPORT fn_genera_reporte TO XML HANDLER v_manejador_rpt
         CASE p_tpo_consulta
            WHEN 1 # tipo consulta detalle
               LET v_consulta = "\n SELECT mov.f_liquida,",
                                "\n        mov.folio_liquida,",
                                "\n        mov.movimiento,",
                                "\n        cat.movimiento_desc,",
                                "\n        mov.monto_pesos",
                                "\n   FROM sep_mov_deudor mov LEFT OUTER JOIN cat_movimiento cat",
                                "\n     ON cat.movimiento = mov.movimiento",
                                "\n        JOIN afi_derechohabiente afi",
                                "\n     ON afi.id_derechohabiente = mov.id_derechohabiente",
                                "\n  WHERE ",p_filtro,
                                "\n    AND mov.id_derechohabiente = ?"
               --DISPLAY v_consulta
               PREPARE prp_rec_det_deudor_rpt FROM v_consulta
               DECLARE cur_rec_det_deudor_rpt CURSOR FOR prp_rec_det_deudor_rpt
               LET v_indice_arr_rpt = 1
               FOR v_indice = 1 TO v_deudores.getLength()
                  
                  FOREACH cur_rec_det_deudor_rpt USING v_deudores[v_indice].v_id_derechohabiente
                                                  INTO v_detalle_deudor_aux.*
                     LET v_deudores_rpt.v_id_derechohabiente = v_deudores[v_indice].v_id_derechohabiente
                     LET v_deudores_rpt.v_nss                = v_deudores[v_indice].v_nss
                     LET v_deudores_rpt.v_nombre             = v_deudores[v_indice].v_nombre
                     LET v_deudores_rpt.v_monto_pesos_gen    = v_deudores[v_indice].v_monto_pesos
                     
                     LET v_deudores_rpt.v_f_liquida     = v_detalle_deudor_aux.v_f_liquida
                     LET v_deudores_rpt.v_folio_liquida = v_detalle_deudor_aux.v_folio_liquida
                     LET v_deudores_rpt.v_movimiento    = v_detalle_deudor_aux.v_movimiento," - ",v_detalle_deudor_aux.v_movimiento_desc CLIPPED
                     LET v_deudores_rpt.v_monto_pesos   = v_detalle_deudor_aux.v_monto_pesos

                     OUTPUT TO REPORT fn_genera_reporte(p_filtro_nss,
                                                        p_filtro_periodo_ini,
                                                        p_filtro_periodo_fin,
                                                        p_filtro_folio,
                                                        p_filtro_deudor,
                                                        p_filtro_compensacion,
                                                        p_total_restitucion_sol,
                                                        p_total_deudor_compensado,
                                                        p_total_activo,
                                                        v_deudores_rpt.*
                                                        ,
                                                        p_tpo_consulta
                                                        )
                  END FOREACH
               END FOR
               FREE cur_rec_det_deudor_rpt

            WHEN 2 # tipo consulta global
               INITIALIZE v_deudores_rpt.* TO NULL
               OUTPUT TO REPORT fn_genera_reporte(p_filtro_nss,
                                                  p_filtro_periodo_ini,
                                                  p_filtro_periodo_fin,
                                                  p_filtro_folio,
                                                  p_filtro_deudor,
                                                  p_filtro_compensacion,
                                                  p_total_restitucion_sol,
                                                  p_total_deudor_compensado,
                                                  p_total_activo,
                                                  v_deudores_rpt.*,
                                                  p_tpo_consulta)

         END CASE
      FINISH REPORT fn_genera_reporte
   ELSE
      CALL fn_mensaje("AVISO","Ocurrió un error al cargar plantilla de reporte","information")
   END IF
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPC28                                                   #
#Descripcion       => funcion que crea reporte de deudores                     #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 25 Octubre 2012                                          #
################################################################################
REPORT fn_genera_reporte(p_filtro_nss,
                         p_filtro_periodo_ini,
                         p_filtro_periodo_fin,
                         p_filtro_folio,
                         p_filtro_deudor,
                         p_filtro_compensacion,
                         p_total_restitucion_sol,
                         p_total_deudor_compensado,
                         p_total_activo,
                         p_deudores_rpt
                         ,
                         p_tpo_consulta
                         )
DEFINE # filtros de busqueda
       p_filtro_nss          LIKE afi_derechohabiente.nss,
       p_filtro_periodo_ini  LIKE sep_mov_deudor.f_liquida,
       p_filtro_periodo_fin  LIKE sep_mov_deudor.f_liquida,
       p_filtro_folio        LIKE sep_mov_deudor.folio_liquida,
       p_filtro_deudor       BOOLEAN,
       v_filtro_deudor_rpt   CHAR(1),
       p_filtro_compensacion BOOLEAN,
       v_filtro_compensacion_rpt CHAR(1),
       p_tpo_consulta            SMALLINT,
       p_total_restitucion_sol   LIKE sep_mov_deudor.monto_pesos,
       p_total_deudor_compensado LIKE sep_mov_deudor.monto_pesos,
       p_total_activo            LIKE sep_mov_deudor.monto_pesos,
       v_fecha_actual DATE,
       p_deudores_rpt RECORD
          v_id_derechohabiente LIKE sep_mov_deudor.id_derechohabiente,
          v_nss                LIKE afi_derechohabiente.nss,
          v_nombre             VARCHAR(200),--LIKE afi_derechohabiente.nombre_af,
          v_monto_pesos_gen    LIKE sep_mov_deudor.monto_pesos,
          v_f_liquida          LIKE sep_mov_deudor.f_liquida,
          v_folio_liquida      LIKE sep_mov_deudor.folio_liquida,
          v_movimiento         LIKE cat_movimiento.movimiento_desc,
          v_monto_pesos        LIKE sep_mov_deudor.monto_pesos
       END RECORD,
       v_pagina SMALLINT

   FORMAT

      FIRST PAGE HEADER
         LET v_fecha_actual = TODAY
         # imprime X si se seleccionó la casilla de deudor
         IF(p_filtro_deudor)THEN
            LET v_filtro_deudor_rpt = "X"
         ELSE 
            LET v_filtro_deudor_rpt = " "
         END IF
         # imprime X si se seleccionó la casilla de compensación
         IF(p_filtro_compensacion)THEN
            LET v_filtro_compensacion_rpt = "X"
         ELSE 
            LET v_filtro_compensacion_rpt = " "
         END IF
         
         PRINTX v_fecha_actual,
                p_filtro_nss,
                p_filtro_periodo_ini,
                p_filtro_periodo_fin,
                p_filtro_folio,
                v_filtro_deudor_rpt,
                v_filtro_compensacion_rpt,
                p_total_restitucion_sol,
                p_total_deudor_compensado,
                p_total_activo
                

      BEFORE GROUP OF p_deudores_rpt.v_id_derechohabiente
         PRINTX p_deudores_rpt.v_nss,
                p_deudores_rpt.v_nombre,
                p_deudores_rpt.v_monto_pesos_gen,
                p_tpo_consulta

      ON EVERY ROW
         PRINTX p_deudores_rpt.v_f_liquida,
                p_deudores_rpt.v_folio_liquida,
                p_deudores_rpt.v_movimiento,
                p_deudores_rpt.v_monto_pesos
                

      PAGE TRAILER
         LET v_pagina = PAGENO 
         PRINTX v_pagina
               
      ON LAST ROW
       
END REPORT

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPC28                                                   #
#Descripcion       => Muestra cifras globales                                  #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 25 Octubre 2012                                          #
################################################################################
FUNCTION fn_consulta_global(p_forma)
DEFINE p_forma               ui.Form,
       v_filtro              STRING,
       p_filtro_nss          LIKE afi_derechohabiente.nss,
       p_filtro_periodo_ini  LIKE sep_mov_deudor.f_liquida,
       p_filtro_periodo_fin  LIKE sep_mov_deudor.f_liquida,
       p_filtro_folio        LIKE sep_mov_deudor.folio_liquida,
       p_filtro_deudor       BOOLEAN,
       p_filtro_compensacion BOOLEAN
       

   MENU " "

      BEFORE MENU
         LET v_total_restitucion_sol   = 0
         LET v_total_deudor_compensado = 0
         LET v_total_activo            = 0
         INITIALIZE p_filtro_nss,
                    p_filtro_periodo_ini,
                    p_filtro_periodo_fin,
                    p_filtro_folio,
                    p_filtro_deudor,
                    p_filtro_compensacion TO NULL
         # Se indica que se recupere todo
         LET v_filtro = " 1 = 1"
         CALL fn_recupera_cifras_globales(v_filtro)
         DISPLAY v_total_restitucion_sol   TO restitucion_solicitada
         DISPLAY v_total_deudor_compensado TO deudor_compensado
         DISPLAY v_total_activo            TO total_activo

      ON ACTION reporte
         CALL fn_genera_reporte_deudores(p_filtro_nss,
                                         p_filtro_periodo_ini,
                                         p_filtro_periodo_fin,
                                         p_filtro_folio,
                                         p_filtro_deudor,
                                         p_filtro_compensacion,
                                         v_total_restitucion_sol,
                                         v_total_deudor_compensado,
                                         v_total_activo,
                                         "1 = 1", # todos los registros
                                         2) # tipo consulta global
         CALL p_forma.setElementHidden("gpo_reporte",FALSE)
         
         DISPLAY "<a gwc:attributes=\"href resourceUri('"||v_nom_reporte||"','sep')\" target='_blank'><img gwc:attributes=\"src resourceuri('logo_pdf_descarga.gif','glo')\" title='Visualizar reporte'/></a>"  TO reporte

      ON ACTION cancelar
         EXIT MENU

   END MENU

END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPC28                                                   #
#Descripcion       => Recupera cifras globales                                 #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 29 Octubre 2012                                          #
################################################################################
FUNCTION fn_recupera_cifras_globales(p_filtro)
DEFINE p_filtro   STRING,
       v_consulta STRING,
       v_indice   SMALLINT,
       v_deudores_aux RECORD
          --v_id_restitucion     LIKE sep_mov_deudor.id_restitucion,
          v_id_derechohabiente LIKE sep_mov_deudor.id_derechohabiente,
          v_nss                LIKE afi_derechohabiente.nss,
          v_nombre             LIKE afi_derechohabiente.nombre_af,
          v_ap_paterno         LIKE afi_derechohabiente.ap_paterno_af,
          v_ap_materno         LIKE afi_derechohabiente.ap_materno_af,
          v_monto_pesos        LIKE sep_mov_deudor.monto_pesos,
          v_movimiento         LIKE sep_mov_deudor.movimiento
       END RECORD

   LET v_indice = 1
   LET v_consulta = "\n SELECT mov.id_derechohabiente,",
                    "\n        afi.nss,",
                    "\n        afi.nombre_af,",
                    "\n        afi.ap_paterno_af,",
                    "\n        afi.ap_materno_af,",
                    "\n        mov.monto_pesos,",
                    "\n        mov.movimiento",
                    "\n   FROM sep_mov_deudor mov JOIN afi_derechohabiente afi",
                    "\n     ON afi.id_derechohabiente = mov.id_derechohabiente",
                    "\n  WHERE ",p_filtro
   PREPARE prp_rec_deudores FROM v_consulta
   DECLARE cur_rec_deudores CURSOR FOR prp_rec_deudores
   FOREACH cur_rec_deudores INTO v_deudores_aux.*

      CASE v_deudores_aux.v_movimiento
      
         WHEN 381 # Abono por restitución
            LET v_total_restitucion_sol = v_total_restitucion_sol + v_deudores_aux.v_monto_pesos

         WHEN 382 # Cargo por restitución
            LET v_total_deudor_compensado = v_total_deudor_compensado + v_deudores_aux.v_monto_pesos

      END CASE
      LET v_total_activo = v_total_activo + v_deudores_aux.v_monto_pesos

      LET v_indice = v_indice + 1
   END FOREACH
   FREE cur_rec_deudores
END FUNCTION