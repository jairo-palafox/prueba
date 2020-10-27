--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 
--===============================================================

#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETC372                                                                #
#Objetivo     => Consulta de la carga de AClaraciones del Fondo de Ahorro               #
#                                                                                       #
#Fecha inicio => 08 Junio 2015                                                          # 
#########################################################################################

DATABASE safre_viv

GLOBALS "RETG01.4gl"  -- se agrega archivo globales y se sustituyen las variables necesarias

GLOBALS
	DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado
          ,v_ventana                ui.WINDOW
END GLOBALS


PRIVATE DEFINE forma                         ui.Form

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
   CALL fn_consulta_archivo(p_usuario_cod)

END MAIN

{ ============================================================================

Nombre: fn_consulta_archivo
Fecha creacion: 08 Junio 2015   
Registro de modificaciones:
==============================================================================
}
FUNCTION fn_consulta_archivo(p_usuario_cod)
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario
       v_folio          DECIMAL(9,0), -- folio
       v_afore          INTEGER,
       v_nss            CHAR(11), 
       v_rfc            CHAR(13),
       v_tpo_movto      CHAR(2),
       v_f_ini_carga    DATE,
       v_f_fin_carga    DATE,
       
       v_f_inicial      LIKE ret_cza_transferencia.f_carga,
       v_f_final        LIKE ret_cza_transferencia.f_carga,
       v_cbx_folios     ui.ComboBox, -- combo de folios
       v_cbx_tpo_movto  ui.ComboBox, -- combo de tipos de movimiento
       v_s_cadena       STRING, -- cadena de texto
       v_r_glo_folio    RECORD LIKE glo_folio.*,
       v_r_tpo_movto    RECORD LIKE ret_cat_aclara_fa_movto.*,
       v_r_agrupador   RECORD -- registro de despliegue del agrupador
        folio            LIKE glo_folio.folio
       END RECORD,
       v_arr_agrupador  DYNAMIC ARRAY OF RECORD -- arreglo de despliegue del agrupador
        folio            LIKE glo_folio.folio
       END RECORD,
       arr_reg_aclara           DYNAMIC ARRAY OF RECORD
         v_tipo_movimiento      CHAR(40),
         v_folio_aclara         LIKE ret_det_aclara_fondo_ahorro.folio_aclara,
         v_folio                LIKE ret_det_aclara_fondo_ahorro.folio,
         v_rfc_saci             LIKE ret_det_aclara_fondo_ahorro.rfc_saci,
         v_nss_aclarado         LIKE ret_det_aclara_fondo_ahorro.nss_aclarado,
         v_nombre_aclarado      LIKE ret_det_aclara_fondo_ahorro.nombre_aclarado, 
         v_imp_movto            LIKE ret_det_aclara_fondo_ahorro.imp_movto,
         v_rfc_nuevo            LIKE ret_det_aclara_fondo_ahorro.rfc_nuevo,
         v_nss_dest             LIKE ret_det_aclara_fondo_ahorro.nss_aclarado,
         v_nombre_dest          LIKE ret_det_aclara_fondo_ahorro.nombre_aclarado, 
         v_saldo                DECIMAL(20,2),
         v_estado_solicitud     CHAR(40),
         v_cod_rechazo          CHAR(40)
       END RECORD,
       arr_reporte              DYNAMIC ARRAY OF RECORD
         v_folio                LIKE ret_transferencia.folio,
         v_f_carga              LIKE ret_cza_transferencia.f_carga,
         v_nss                  LIKE afi_derechohabiente.nss,
         v_nombre_trabajador    STRING, 
         v_aivs97_sol           LIKE ret_transferencia.aivs_viv97,
         v_aivs97_sol_pesos     DECIMAL (20,2),
         v_aivs97_sdo           LIKE ret_transferencia.aivs_viv97,
         v_aivs97_sdo_pesos     DECIMAL (20,2),
         v_aivs97_dif           DECIMAL (19,6),
         v_aivs97_dif_pesos     DECIMAL (20,2)
       END RECORD,
       v_query              STRING, -- detalle
       v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
       v_indice             SMALLINT, -- indice de arreglo
       v_nombre_trab    LIKE ret_transferencia.nombre_afore,
       v_paterno_trab   LIKE ret_transferencia.paterno_afore,
       v_materno_trab   LIKE ret_transferencia.materno_afore,
       v_precio_fondo   LIKE ret_cza_transferencia.precio_fondo,
       v_ruta_reporte            STRING ,-- ruta del archivo del reporte       
       v_ruta_listados           STRING ,-- ruta de los listados
       v_ruta_ejecutable         STRING ,-- ruta del ejecutable
       manejador_rpt            om.SaxDocumentHandler ,
       v_indice_reporte          SMALLINT,
       v_agrupador_folio_fecha_Afore STRING


       
   OPEN WINDOW w_consulta_folio WITH FORM "RETC3721"
       LET  v_ventana = UI.WINDOW.GETCURRENT()
       LET forma   = v_ventana.getForm()
       CALL v_ventana.SETTEXT("Consulta Archivos de Aclaraciones del Fondo de Ahorro")

   -- se le asigna el apuntado del combo a la variable
   LET v_cbx_folios    = ui.ComboBox.forName("formonly.cmb_folio")
   LET v_cbx_tpo_movto = ui.ComboBox.forName("formonly.cmb_tpo_movto")
 
   -- se inician los combobox en blanco
   CALL v_cbx_folios.clear()
   CALL v_cbx_tpo_movto.clear()

   INPUT v_folio, v_rfc, v_tpo_movto, v_f_ini_carga, v_f_fin_carga
      FROM cmb_folio, ed_rfc, cmb_tpo_movto, date_f_inicial, date_f_final 
      ATTRIBUTES (UNBUFFERED, WITHOUT DEFAULTS)

    BEFORE INPUT 
      -- se limpian las variables
      LET v_folio       = NULL    
      LET v_afore       = NULL    
      LET v_nss         = NULL    
      LET v_f_ini_carga = NULL 
      LET v_f_fin_carga = NULL 
      
      CALL forma.setElementHidden("Group2", 1)

      CALL forma.setElementHidden("tab_det_aclara", 1)
      
   
         -- se llena el arreglo de folios
         DECLARE cur_folios CURSOR FOR
         SELECT a.*
         FROM   glo_folio a
         WHERE  a.proceso_cod = g_proceso_aclara_fa
         AND    a.status >= 0
         ORDER BY folio DESC

         FOREACH cur_folios INTO v_r_glo_folio.*
            LET v_s_cadena = v_r_glo_folio.folio
            CALL v_cbx_folios.addItem(v_r_glo_folio.folio, v_s_cadena)
         END FOREACH

         FREE cur_folios

         -- se llena el arreglo de tipos de Movimiento
         DECLARE cur_tpos_movto CURSOR FOR
         SELECT *
         FROM   ret_cat_aclara_fa_movto a

         FOREACH cur_tpos_movto INTO v_r_tpo_movto.*
            LET v_s_cadena = v_r_tpo_movto.descripcion
            CALL v_cbx_tpo_movto.addItem(v_r_tpo_movto.tpo_movto, v_s_cadena)
         END FOREACH

         FREE cur_tpos_movto

         
      ON ACTION ACCEPT

         -- se borran los arreglos de despliegue
         CALL v_arr_agrupador.clear()
         CALL arr_reg_aclara.clear()
         CALL arr_reporte.clear()
         CALL forma.setElementHidden("Group2", 1)
         CALL forma.setElementHidden("tab_det_aclara", 1)
         


         --modificación de validación de  captura de parametros
         --valida que se ingrese al menos un parametro
         
         IF ( (v_folio IS NULL OR v_folio <= 0) AND 
              (v_rfc IS NULL)                   AND
              (v_tpo_movto IS NULL)             AND 
              (v_f_ini_carga IS NULL)           AND
              (v_f_fin_carga IS NULL)  )THEN 

            CALL fn_mensaje("Consulta","Debe ingresar al menos un parámetro de búsqueda","about")

         ELSE 
            
             -- se limpia el arreglo de despligue del detalle
             CALL arr_reg_aclara.clear()
                   
             -- se consulta del detalle de este agrupador
             -- query para obtener los grupos
             LET v_query = "\n SELECT * FROM (                                                                             ",
                           "\nSELECT c.tpo_movto || ' - ' || c.descripcion as tipo_movimiento,                             ",
                           "\n       a.folio_aclara, a.folio, a.rfc_saci, b.nss, b.nombre,                                 ",
                           "\n       a.imp_movto, a.rfc_nuevo, h.nss, h.nombre,                                            ",
                           "\n       g.saldo, a.estado_solicitud || ' - ' || d.des_corta as estado_solicitud,           ",
                           "\n       a.cod_rechazo || ' - ' || e.des_corta                                                 ",
                           "\nFROM   ret_det_aclara_fondo_ahorro a                                                         ",
                           "\n       LEFT OUTER JOIN ret_aclara_accion_fa g                                                ",
                           "\n                    ON a.id_solicitud = g.id_solicitud                                       ",
                           "\n       LEFT OUTER JOIN afi_fondo72 b                                                         ",
                           "\n                    ON g.id_afi_fondo72_cargo = b.id_afi_fondo72                             ",
                           "\n       LEFT OUTER JOIN afi_fondo72 h                                                         ",
                           "\n                    ON g.id_afi_fondo72_abono = h.id_afi_fondo72,                            ",
                           "\n       ret_cat_aclara_fa_movto c,                                                            ",
                           "\n       ret_estado_solicitud d,                                                               ",
                           "\n       ret_rechazo e                                                                         ",
                           "\n WHERE  1 = 1 "
            IF v_folio IS NOT NULL THEN 
                LET v_query = v_query CLIPPED, "\n AND    a.folio = ", v_folio
            END IF 
            IF v_tpo_movto IS NOT NULL THEN 
                LET v_query = v_query CLIPPED, "\n AND    a.tpo_movto = '", v_tpo_movto, "'"
            END IF 
            IF v_rfc IS NOT NULL THEN 
                LET v_query = v_query CLIPPED, "\n AND    (a.rfc_saci = '", v_rfc, "' OR a.rfc_nuevo = '", v_rfc, "') "
            END IF 
            IF v_f_ini_carga IS NOT NULL AND v_f_fin_carga IS NOT NULL THEN
                LET v_query = v_query CLIPPED, "\n AND  a.folio in (SELECT folio FROM glo_folio WHERE f_actualiza BETWEEN '", v_f_ini_carga,
                                               "' AND '", v_f_fin_carga, "' AND proceso_cod = 1561)"
            END IF 
            LET v_query = v_query CLIPPED, "\n AND    a.tpo_movto = c.tpo_movto                                                            ",
                           "\n AND    a.estado_solicitud = d.estado_solicitud                                              ",
                           "\n AND    a.cod_rechazo = e.cod_rechazo                                                        ",
                           "\n AND    a.tpo_movto IN ('CI', 'UC')                                                          ",
                           "\nUNION ALL                                                                                    ",
                           "\nSELECT c.tpo_movto || ' - ' || c.descripcion as tipo_movimiento,                             ",
                           "\n       a.folio_aclara, a.folio, a.rfc_saci, b.nss, b.nombre,                                 ",
                           "\n       a.imp_movto, g.rfc_abono, h.nss, h.nombre,                                            ",
                           "\n       g.saldo, a.estado_solicitud || ' - ' || d.des_corta as estado_solicitud,           ",
                           "\n       a.cod_rechazo || ' - ' || e.des_corta                                                 ",
                           "\nFROM   ret_det_aclara_fondo_ahorro a                                                         ",
                           "\n       LEFT OUTER JOIN ret_aclara_accion_fa g                                                ",
                           "\n                    ON a.id_solicitud = g.id_solicitud                                       ",
                           "\n       LEFT OUTER JOIN afi_fondo72 b                                                         ",
                           "\n                    ON g.id_afi_fondo72_cargo = b.id_afi_fondo72                             ",
                           "\n       LEFT OUTER JOIN afi_fondo72 h                                                         ",
                           "\n                    ON g.id_afi_fondo72_abono = h.id_afi_fondo72,                            ",
                           "\n       ret_cat_aclara_fa_movto c,                                                            ",
                           "\n       ret_estado_solicitud d,                                                               ",
                           "\n       ret_rechazo e                                                                         ", 	
                           "\n WHERE  1 = 1 "
            IF v_folio IS NOT NULL THEN 
                LET v_query = v_query CLIPPED, "\n AND    a.folio = ", v_folio
            END IF 
            IF v_tpo_movto IS NOT NULL THEN 
                LET v_query = v_query CLIPPED, "\n AND    a.tpo_movto = '", v_tpo_movto, "'"
            END IF 
            IF v_rfc IS NOT NULL THEN 
                LET v_query = v_query CLIPPED, "\n AND    (a.rfc_saci = '", v_rfc, "' OR a.rfc_nuevo = '", v_rfc, "') "
            END IF 
            IF v_f_ini_carga IS NOT NULL AND v_f_fin_carga IS NOT NULL THEN
                LET v_query = v_query CLIPPED, "\n AND  a.folio in (SELECT folio FROM glo_folio WHERE f_actualiza BETWEEN '", v_f_ini_carga,
                                               "' AND '", v_f_fin_carga, "' AND proceso_cod = 1561)"
            END IF 
            LET v_query = v_query CLIPPED, "\n AND    a.tpo_movto = c.tpo_movto                                                            ",
                           "\n AND    a.estado_solicitud = d.estado_solicitud                                              ",
                           "\n AND    a.cod_rechazo = e.cod_rechazo                                                        ",
                           "\n AND    a.tpo_movto IN ('FA'))                                                               ",
                           "\n ORDER  BY folio_aclara                                                                      "
             -- consulta del agrupador
             DISPLAY "Query:\n", v_query


             PREPARE sid_detalle FROM v_query
             DECLARE cur_detalle  CURSOR FOR sid_detalle
                     
                     
             --llena el arreglo        
             LET v_indice = 1
                     
             FOREACH cur_detalle INTO 
                 arr_reg_aclara[v_indice].v_tipo_movimiento  ,              
                 arr_reg_aclara[v_indice].v_folio_aclara     ,
                 arr_reg_aclara[v_indice].v_folio            ,
                 arr_reg_aclara[v_indice].v_rfc_saci         ,
                 arr_reg_aclara[v_indice].v_nss_aclarado     ,
                 arr_reg_aclara[v_indice].v_nombre_aclarado  ,
                 arr_reg_aclara[v_indice].v_imp_movto        ,
                 arr_reg_aclara[v_indice].v_rfc_nuevo        ,
                 arr_reg_aclara[v_indice].v_nss_dest         ,
                 arr_reg_aclara[v_indice].v_nombre_dest      ,
                 arr_reg_aclara[v_indice].v_saldo            ,
                 arr_reg_aclara[v_indice].v_estado_solicitud ,
                 arr_reg_aclara[v_indice].v_cod_rechazo
                 LET v_indice = v_indice + 1
             END FOREACH
                    
             -- se borra el ultimo registro
             CALL arr_reg_aclara.deleteElement(arr_reg_aclara.getLength())
             CALL forma.setElementHidden("tab_det_aclara", 0)    
             CALL forma.setElementHidden("Group2", 0)           
             DISPLAY ARRAY arr_reg_aclara TO tab_det_aclara.*
         END IF                   

      ON ACTION CANCEL
         CALL forma.setElementHidden("tab_det_aclara", 1)    
         CALL forma.setElementHidden("Group2", 1)  
         EXIT INPUT
   
   END INPUT
   
   CLOSE WINDOW w_consulta_folio

END FUNCTION

{ ======================================================================
Clave: PAGC05
Nombre: rpt_detalle_saldos_insuf
Fecha creacion: Junio 20, 2012
Autor: Ivan Vega
Narrativa del proceso que realiza:
Genera el reporte de los saldos insuficientes 

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
REPORT rpt_detalle_saldos_insuf(p_total_regs, v_r_despliegue, p_usuario_cod, p_agrupador_folio_fecha_Afore)
DEFINE    v_r_despliegue          RECORD -- registro de consulta
           v_folio                LIKE ret_transferencia.folio,
           v_f_carga              LIKE ret_cza_transferencia.f_carga,
           v_nss                  LIKE afi_derechohabiente.nss,
           v_nombre_trabajador    STRING, 
           v_aivs97_sol           DECIMAL(24,6),
           v_aivs97_sol_pesos     DECIMAL(22,2),
           v_aivs97_sdo           DECIMAL(24,6),
           v_aivs97_sdo_pesos     DECIMAL(22,2),
           v_aivs97_dif           DECIMAL(24,6),
           v_aivs97_dif_pesos     DECIMAL(22,2)
          END RECORD,
          p_usuario_cod           LIKE seg_usuario.usuario_cod, -- usuario en linea
          v_fecha                 STRING, -- fecha de emision del reporte
          v_nombre_usuario        VARCHAR(100),
          -- variables para acumular TOTAL PARCIAL
          v_total_afore_aivs97_sol           DECIMAL(24,6),
          v_total_afore_aivs97_sol_pesos     DECIMAL(22,2),
          v_total_afore_aivs97_sdo           DECIMAL(24,6),
          v_total_afore_aivs97_sdo_pesos     DECIMAL(22,2),
          v_total_afore_aivs97_dif           DECIMAL(24,6),
          v_total_afore_aivs97_dif_pesos     DECIMAL(22,2),
          p_total_afore_regs                 DECIMAL(9,0),
          -- variables para acumular GRAN TOTAL
          v_total_aivs97_sol           DECIMAL(24,6),
          v_total_aivs97_sol_pesos     DECIMAL(22,2),
          v_total_aivs97_sdo           DECIMAL(24,6),
          v_total_aivs97_sdo_pesos     DECIMAL(22,2),
          v_total_aivs97_dif           DECIMAL(24,6),
          v_total_aivs97_dif_pesos     DECIMAL(22,2),
          p_total_regs                 DECIMAL(9,0),
          v_fecha_carga                STRING,
          p_agrupador_folio_fecha_Afore STRING
          
FORMAT

   FIRST PAGE HEADER
      
      -- variables para acumular gran total
      LET v_total_aivs97_sol       = 0 
      LET v_total_aivs97_sol_pesos = 0
      LET v_total_aivs97_sdo       = 0
      LET v_total_aivs97_sdo_pesos = 0
      LET v_total_aivs97_dif       = 0
      LET v_total_aivs97_dif_pesos = 0
      
      -- variables para acumular por afore, fecha y folio
      LET v_total_afore_aivs97_sol       = 0
      LET v_total_afore_aivs97_sol_pesos = 0
      LET v_total_afore_aivs97_sdo       = 0
      LET v_total_afore_aivs97_sdo_pesos = 0
      LET v_total_afore_aivs97_dif       = 0
      LET v_total_afore_aivs97_dif_pesos = 0
      LET p_total_afore_regs             = 0

      -- se envia folio, usuario y fecha
      LET v_fecha = TODAY USING "dd-mm-yyyy"
      -- se obtiene el nombre del usuario
      SELECT usuario_desc
      INTO v_nombre_usuario
      FROM seg_usuario
      WHERE usuario_cod = p_usuario_cod

      LET v_nombre_usuario = v_nombre_usuario CLIPPED
      
      PRINTX p_usuario_cod, v_fecha, v_nombre_usuario
      
   --BEFORE GROUP OF v_r_despliegue.v_afore
   BEFORE GROUP OF p_agrupador_folio_fecha_Afore
      LET v_fecha_carga = v_r_despliegue.v_f_carga USING "dd-mm-yyyy"
      
      -- se reinician los totales por afore, fecha y folio   
      LET v_total_afore_aivs97_sol       = 0
      LET v_total_afore_aivs97_sol_pesos = 0
      LET v_total_afore_aivs97_sdo       = 0
      LET v_total_afore_aivs97_sdo_pesos = 0
      LET v_total_afore_aivs97_dif       = 0
      LET v_total_afore_aivs97_dif_pesos = 0
      LET p_total_afore_regs             = 0
   
      PRINTX v_r_despliegue.v_folio,
             v_fecha_carga
             

   ON EVERY ROW
      PRINTX v_r_despliegue.*
      DISPLAY v_r_despliegue.*
      
      -- se acumulan los montos para total por afore, folio y fecha de carga
      LET v_total_afore_aivs97_sol       = v_total_afore_aivs97_sol       + v_r_despliegue.v_aivs97_sol      
      LET v_total_afore_aivs97_sol_pesos = v_total_afore_aivs97_sol_pesos + v_r_despliegue.v_aivs97_sol_pesos
      LET v_total_afore_aivs97_sdo       = v_total_afore_aivs97_sdo       + v_r_despliegue.v_aivs97_sdo      
      LET v_total_afore_aivs97_sdo_pesos = v_total_afore_aivs97_sdo_pesos + v_r_despliegue.v_aivs97_sdo_pesos
      LET v_total_afore_aivs97_dif       = v_total_afore_aivs97_dif       + v_r_despliegue.v_aivs97_dif      
      LET v_total_afore_aivs97_dif_pesos = v_total_afore_aivs97_dif_pesos + v_r_despliegue.v_aivs97_dif_pesos
      LET p_total_afore_regs             = p_total_afore_regs             + 1
      
      
      -- se acumulan los montos para gran total
      LET v_total_aivs97_sol       = v_total_aivs97_sol       + v_r_despliegue.v_aivs97_sol      
      LET v_total_aivs97_sol_pesos = v_total_aivs97_sol_pesos + v_r_despliegue.v_aivs97_sol_pesos
      LET v_total_aivs97_sdo       = v_total_aivs97_sdo       + v_r_despliegue.v_aivs97_sdo      
      LET v_total_aivs97_sdo_pesos = v_total_aivs97_sdo_pesos + v_r_despliegue.v_aivs97_sdo_pesos
      LET v_total_aivs97_dif       = v_total_aivs97_dif       + v_r_despliegue.v_aivs97_dif      
      LET v_total_aivs97_dif_pesos = v_total_aivs97_dif_pesos + v_r_despliegue.v_aivs97_dif_pesos

   --AFTER GROUP OF v_r_despliegue.v_afore
   AFTER GROUP OF p_agrupador_folio_fecha_Afore
      PRINTX v_total_afore_aivs97_sol       ,
             v_total_afore_aivs97_sol_pesos ,
             v_total_afore_aivs97_sdo       ,
             v_total_afore_aivs97_sdo_pesos ,
             v_total_afore_aivs97_dif       ,
             v_total_afore_aivs97_dif_pesos ,
             p_total_afore_regs            
                                                          
   
   ON LAST ROW
      PRINTX p_total_regs             ,
             v_total_aivs97_sol       ,
             v_total_aivs97_sol_pesos ,
             v_total_aivs97_sdo       ,
             v_total_aivs97_sdo_pesos ,
             v_total_aivs97_dif       ,
             v_total_aivs97_dif_pesos 

END REPORT
