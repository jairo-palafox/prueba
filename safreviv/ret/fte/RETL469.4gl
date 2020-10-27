--===============================================================
-- Version: 1.0.0
--===============================================================

#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETL469                                                                #
#Objetivo     => Consulta y Extracción de Pagos de la Devolición del SSV  vía BATCH     #
#Fecha inicio => Abril 23, 2018                                                         # 
#########################################################################################

DATABASE safre_viv

GLOBALS "RETG01.4gl"  -- se agrega archivo globales y se sustituyen las variables necesarias

GLOBALS
	DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod -- clave del usuario firmado
          ,v_ventana        ui.WINDOW
          ,f_forma          ui.Form  
   DEFINE g_proceso_cod LIKE cat_proceso.proceso_cod, -- código del proceso
          g_opera_cod   LIKE cat_operacion.opera_cod, -- código de operacion
          g_folio       LIKE dis_det_avance_pago.folio,
          g_pid          LIKE bat_ctr_proceso.pid,
       g_reg_modulo   RECORD
        ruta_exp         CHAR(40),
        ruta_rescate     CHAR(40),
        ruta_listados    CHAR(40)
       END RECORD,
       seg_modulo_bat RECORD
        ruta_listados    CHAR(40)
       END RECORD

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

   LET g_proceso_cod = g_proceso_cod_extractor_pagos
   LET g_opera_cod = g_opera_extractor_pago

   -- se obtienen las rutas de control del modulo
   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
     INTO g_reg_modulo.*
     FROM seg_modulo s
    WHERE s.modulo_cod = 'ret'

   SELECT b.ruta_listados
     INTO seg_modulo_bat.ruta_listados
     FROM seg_modulo b
    WHERE b.modulo_cod = 'bat'

   -- consulta de informacion recibida 
  
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   --Se inicia el log del programa
   CALL STARTLOG (p_usuario_cod CLIPPED||".RETL469.log")
   
   -- consulta de informacion recibida 
   CALL fn_consulta_detalle(p_usuario_cod)

END MAIN

{ ============================================================================
Clave: RETL469
Nombre: fn_consulta_detalle
Fecha creacion: Abril 23, 2018
Registro de modificaciones:
Descrip: Consulta y exportación del detalle de pagos de la devolución del SSV
==============================================================================
}
FUNCTION fn_consulta_detalle(p_usuario_cod)
DEFINE 
      p_usuario_cod         LIKE seg_usuario.usuario_cod, -- clave del usuario
      v_archivo             LIKE glo_ctr_archivo.nombre_archivo,
      v_archivo_consulta    LIKE glo_ctr_archivo.nombre_archivo,

      l_arr_estatus_sol     DYNAMIC ARRAY OF RECORD 
         chk_consulta       BOOLEAN,
         estatus            CHAR(50)
          
      END RECORD,
      l_arr_estatus_sol_c   DYNAMIC ARRAY OF CHAR(50),

      l_dnd                 ui.DragDrop, -- manejador del (drag and drop)
      l_drag_index          INT, -- indice del drag
      l_drop_index          INT, -- indice del drop
      l_drag_source         STRING, -- fuente del drag
      l_drag_value          STRING, -- valor del drag

      v_nss                    STRING,
      v_chk_gpo_todos          BOOLEAN,
      v_chk_gpo_1              BOOLEAN,
      v_chk_gpo_2              BOOLEAN,
      v_chk_gpo_3              BOOLEAN,
      v_chk_gpo_4              BOOLEAN,
      v_chk_ori_todos          BOOLEAN,
      v_chk_ori_infonavit            BOOLEAN,
      v_chk_ori_excep          BOOLEAN,
      v_chk_ori_afore          BOOLEAN,
      v_chk_ori_siaff          BOOLEAN,
      v_chk_via_daps           BOOLEAN,
      v_chk_via_afore          BOOLEAN,
      v_chk_via_amparos        BOOLEAN,
      v_chk_via_convenios      BOOLEAN,
      v_chk_via_embargos       BOOLEAN,
      v_chk_via_historicos     BOOLEAN,
      v_chk_via_infonavit      BOOLEAN,
      v_chk_via_laudos         BOOLEAN,
      v_chk_via_mayores        BOOLEAN,
      v_chk_via_mayores_plus   BOOLEAN,
      v_chk_via_menores        BOOLEAN,
      v_chk_via_menores_plus   BOOLEAN,
      v_chk_via_tableta        BOOLEAN,
      v_chk_via_dev_auto       BOOLEAN,
      v_chk_via_crm            BOOLEAN,
      v_chk_via_asesor_tel     BOOLEAN,
      v_chk_estatus_todos      BOOLEAN,
      v_chk_estatus_8          BOOLEAN,
      v_chk_estatus_15         BOOLEAN,
      v_chk_estatus_10         BOOLEAN,
      v_chk_estatus_50         BOOLEAN,
      v_chk_estatus_60         BOOLEAN,
      v_chk_estatus_69         BOOLEAN,
      v_chk_estatus_70         BOOLEAN,
      v_chk_estatus_71         BOOLEAN,
      v_chk_estatus_72         BOOLEAN,
      v_chk_estatus_73         BOOLEAN,
      v_chk_estatus_90         BOOLEAN,
      v_chk_estatus_100        BOOLEAN,
      v_chk_estatus_200        BOOLEAN,
      v_chk_estatus_209        BOOLEAN,
      v_chk_estatus_210        BOOLEAN,
      v_chk_estatus_214        BOOLEAN,
      v_chk_estatus_700        BOOLEAN,
      v_chk_estatus_710        BOOLEAN,
      v_chk_estatus_720        BOOLEAN,
      v_chk_estatus_730        BOOLEAN,
      v_chk_estatus_790        BOOLEAN,
      v_tot_registros          INTEGER,
      v_suma_total             DECIMAL(22,2),

      v_reg_cifras  DYNAMIC ARRAY OF RECORD 
         grupo        CHAR(40),
         gpo_desc     CHAR(40),
         estado_sol   CHAR(04),
         estado_desc  CHAR(40),
         num_regs     CHAR(18),
         sum_imp_pago CHAR(25)
      END RECORD,
      v_reg_cifras_paso  RECORD 
         grupo        SMALLINT,
         gpo_desc     CHAR(40),
         estado_sol   SMALLINT,
         estado_desc  CHAR(40),
         num_regs     INTEGER,
         sum_imp_pago DECIMAL(22,2)
      END RECORD,
      
      v_tipo_solicitud      CHAR(4),
      v_desc_tipo_sol       CHAR(30),
      v_contador            INTEGER,
      v_cbx_folios          ui.ComboBox, -- combo de folios
      v_cbx_tipo_solicitud  ui.ComboBox, -- combo de folios
      v_cbx_folios_archivo  ui.ComboBox, -- combo de folios
      cmb_folios            SMALLINT,
      cmb_tipo_solicitud    CHAR(4),
      cmb_folios_archivo    SMALLINT,
      v_ed_nss              CHAR(11),
      v_poner_coma          SMALLINT,
      v_regs_insertados     INTEGER,
      
      v_d_f_pago_ini        DATE,
      v_d_f_pago_fin        DATE,
      v_d_f_sol_ini         DATE,
      v_d_f_sol_fin         DATE,
      v_d_f_autoriza_ini    DATE,
      v_d_f_autoriza_fin    DATE,
      v_ed_doc_fico         CHAR(10),
      v_resultado           SMALLINT,
      v_nss_paso            CHAR(11), 
      v_r_grupo   DYNAMIC ARRAY OF RECORD -- registro de despliegue del agrupador
         nss              CHAR(11),
         rfc              CHAR(13),
         doc_fico         CHAR(10),
         ejercicio        CHAR(4),
         fch_contable     DATE,
         fch_liquida      DATE,
         desc_tipo_sol    CHAR(25),
         folio            DECIMAL(9,0),
         subcta_47        SMALLINT,
         pesos_tesofe     DECIMAL(18,2),
         subcta_4         SMALLINT,
         acciones_viv97   DECIMAL(18,6),
         subcta_8         SMALLINT,
         acciones_viv92   DECIMAL(18,6),
         estado_solicitud SMALLINT
      END RECORD,

      v_r_grupo_indiv   RECORD -- registro de despliegue del agrupador
         nss              CHAR(11),
         rfc              CHAR(13),
         doc_fico         CHAR(10),
         ejercicio        CHAR(4),
         fch_contable     DATE,
         fch_liquida      DATE,
         desc_tipo_sol    CHAR(25),
         folio            DECIMAL(9,0),
         subcta_47        SMALLINT,
         pesos_tesofe     DECIMAL(18,2),
         subcta_4         SMALLINT,
         acciones_viv97   DECIMAL(18,6),
         subcta_8         SMALLINT,
         acciones_viv92   DECIMAL(18,6),
         estado_solicitud SMALLINT
      END RECORD,

      v_arr_reg_detalle         RECORD
         ed_tot_reg               INTEGER                   ,
         ed_pesos_debita          DECIMAL(14,2)                                 ,
         ed_aivs_debita           DECIMAL(18,6),
         ed_total_pagos           INTEGER, 
         ed_sin_dif               INTEGER,
         ed_fch_carga             DATE,
         ed_aivs_viv_97           DECIMAL(18,6),
         ed_aivs_viv_92           DECIMAL(18,6),
         ed_pesos_viv_97          DECIMAL(14,2),
         ed_pesos_viv_92          DECIMAL(14,2),
         ed_nss_no_loc            INTEGER,
         ed_pesos_nss_no_loc      DECIMAL(14,2),
         ed_aivs_nss_no_loc       DECIMAL(18,6)
       END RECORD,
       v_query                       STRING, -- detalle
       v_query_where                 STRING,
       v_query_where_act             STRING,
       v_indice                      INTEGER, -- indice de arreglo
       v_precio_fondo                LIKE ret_cza_transferencia.precio_fondo,
       v_id_peticion                 DECIMAL(9,0),
       v_s_comando                   STRING,
       v_mensaje                     STRING
       
   CONSTANT l_nom_tbl_estados = "tbl_estados" -- Tabla de estados
   CONSTANT l_nom_tbl_estados_cons = "tbl_estados_cons" -- Tabla de estados a consultar

   LET v_indice = 1
   LET v_regs_insertados = 0
   -- se crea la sentencia que busca los estado de solicitudes 
   LET v_query = " SELECT 0,estado || '-' || descripcion       \n",
                    " FROM ret_cat_estado_sol_dev_ssv   "
   PREPARE prp_estatus_sol FROM v_query

   DECLARE cur_estatus_sol CURSOR FOR prp_estatus_sol 
      FOREACH cur_estatus_sol INTO l_arr_estatus_sol[v_indice].*
         -- se incrementa el indice del arreglo
         LET v_indice = v_indice + 1
      END FOREACH

   -- se borra el ultimo indice del arreglo porque es nulo
   CALL l_arr_estatus_sol.deleteElement(v_indice)

   LET v_indice = 1
   OPEN WINDOW w_consulta_pagos WITH FORM "RETL4691"
   LET  v_ventana = UI.WINDOW.GETCURRENT()
   LET f_forma = v_ventana.getForm()
   CALL v_ventana.SETTEXT("Consulta Cifras Control")
   
   CALL f_forma.setElementHidden("gpo_cifras",1) 
                       -- se limpian las variables
   LET v_tipo_solicitud = "";
   LET v_d_f_sol_fin         = NULL;
   LET v_d_f_sol_ini         = NULL;
   LET v_d_f_pago_fin        = NULL;
   LET v_d_f_pago_ini        = NULL;
   LET v_d_f_autoriza_ini    = NULL;
   LET v_d_f_autoriza_fin    = NULL;
   LET v_tot_registros       = 0;
   LET v_suma_total          = 0;
         
   INPUT 
         v_nss,                   v_chk_gpo_todos,          v_chk_gpo_1,             v_chk_gpo_2,
         v_chk_gpo_3,             v_chk_gpo_4,              v_chk_ori_todos,         v_chk_ori_infonavit,
         v_chk_ori_excep,         v_chk_ori_afore,          v_chk_ori_siaff,         v_chk_via_daps,          
         v_chk_via_afore,
         v_chk_via_amparos,       v_chk_via_convenios,      v_chk_via_embargos,      v_chk_via_historicos,
         v_chk_via_infonavit,     v_chk_via_laudos,         v_chk_via_mayores,       v_chk_via_mayores_plus,
         v_chk_via_menores,       v_chk_via_menores_plus,   v_chk_via_tableta,       v_chk_via_dev_auto,
         v_chk_via_crm,           v_chk_via_asesor_tel,
         v_d_f_pago_ini,          v_d_f_pago_fin,           v_d_f_sol_ini,           v_d_f_sol_fin, 
         v_d_f_autoriza_ini,      v_d_f_autoriza_fin,       v_chk_estatus_todos,     v_chk_estatus_10,
         v_chk_estatus_8,         v_chk_estatus_60,         v_chk_estatus_69,        v_chk_estatus_70,
         v_chk_estatus_15,        v_chk_estatus_71,         v_chk_estatus_72,        v_chk_estatus_73,
         v_chk_estatus_90,        v_chk_estatus_100,        v_chk_estatus_200,       v_chk_estatus_209,
         v_chk_estatus_210,        v_chk_estatus_214,       v_chk_estatus_700,       v_chk_estatus_50,
         v_chk_estatus_710,       v_chk_estatus_720,        v_chk_estatus_730,       v_chk_estatus_790
      FROM txed_nss,              chk_gpo_todos,            chk_gpo_1,               chk_gpo_2,
         chk_gpo_3,               chk_gpo_4,                chk_ori_todos,           chk_ori_infonavit,
         chk_ori_excep,           chk_ori_afore,            chk_ori_siaff,           chk_via_daps,            
         chk_via_afore,           
         chk_via_amparos,         chk_via_convenios,        chk_via_embargos,        chk_via_historicos,
         chk_via_infonavit,       chk_via_laudos,           chk_via_mayores,         chk_via_mayores_plus,
         chk_via_menores,         chk_via_menores_plus,     chk_via_tableta,         chk_via_dev_auto,
         chk_via_crm,             chk_via_asesor_tel,
         d_f_pago_ini,            d_f_pago_fin,             d_f_sol_ini,             d_f_sol_fin,
         d_f_autoriza_ini,        d_f_autoriza_fin,         chk_estatus_todos,       chk_estatus_10,
         chk_estatus_8,           chk_estatus_60,           chk_estatus_69,          chk_estatus_70,
         chk_estatus_15,          chk_estatus_71,           chk_estatus_72,          chk_estatus_73,
         chk_estatus_90,          chk_estatus_100,          chk_estatus_200,         chk_estatus_209,
         chk_estatus_210,         chk_estatus_214,          chk_estatus_700,         chk_estatus_50,
         chk_estatus_710,         chk_estatus_720,          chk_estatus_730,         chk_estatus_790
      ATTRIBUTES (UNBUFFERED, WITHOUT DEFAULTS)
      BEFORE INPUT 
      LET v_query_where     = " "
      LET v_query_where_act = " "
      ON CHANGE chk_ori_afore
         IF v_chk_ori_afore THEN 
            LET v_chk_via_afore = 1
         ELSE 
            LET v_chk_via_afore = 0
            LET v_chk_ori_todos = 0
         END IF 
      ON CHANGE chk_ori_infonavit
         IF v_chk_ori_infonavit THEN 
            LET v_chk_via_infonavit  = 1
            LET v_chk_via_tableta    = 1
            LET v_chk_via_dev_auto   = 1
            LET v_chk_via_crm        = 1
            LET v_chk_via_asesor_tel = 1
         ELSE 
            LET v_chk_via_infonavit  = 0
            LET v_chk_via_tableta    = 0
            LET v_chk_via_dev_auto   = 0
            LET v_chk_via_crm        = 0
            LET v_chk_via_asesor_tel = 0
            LET v_chk_ori_todos = 0
         END IF 
      ON CHANGE chk_ori_siaff
         IF v_chk_ori_siaff THEN 
            LET v_chk_via_mayores      = 1
            LET v_chk_via_mayores_plus = 1
            LET v_chk_via_menores      = 1
            LET v_chk_via_menores_plus = 1
         ELSE
            LET v_chk_via_mayores      = 0
            LET v_chk_via_mayores_plus = 0
            LET v_chk_via_menores      = 0
            LET v_chk_via_menores_plus = 0
            LET v_chk_ori_todos = 0
         END IF 
      ON CHANGE chk_ori_excep
         IF v_chk_ori_excep THEN 
            LET v_chk_via_convenios  = 1
            LET v_chk_via_historicos = 1
            LET v_chk_via_laudos     = 1
            LET v_chk_via_amparos    = 1
            LET v_chk_via_daps       = 1
            LET v_chk_via_embargos   = 1
         ELSE 
            LET v_chk_via_convenios  = 0
            LET v_chk_via_historicos = 0
            LET v_chk_via_laudos     = 0
            LET v_chk_via_amparos    = 0
            LET v_chk_via_daps       = 0
            LET v_chk_via_embargos   = 0
            LET v_chk_ori_todos = 0
         END IF 
      ON CHANGE chk_gpo_todos
         IF v_chk_gpo_todos THEN 
            LET v_chk_gpo_1 = 1
            LET v_chk_gpo_2 = 1
            LET v_chk_gpo_3 = 1
            LET v_chk_gpo_4 = 1
         ELSE 
            LET v_chk_gpo_1 = 0
            LET v_chk_gpo_2 = 0
            LET v_chk_gpo_3 = 0
            LET v_chk_gpo_4 = 0
         END IF 
      ON CHANGE chk_via_afore
         IF NOT v_chk_via_afore THEN
            LET v_chk_ori_afore = 0
            LET v_chk_ori_todos = 0
         END IF 
      ON CHANGE chk_via_infonavit
         IF NOT v_chk_via_infonavit THEN
            LET v_chk_ori_infonavit = 0
            LET v_chk_ori_todos = 0
         END IF 
      ON CHANGE chk_via_tableta
         IF NOT v_chk_via_tableta THEN 
            LET v_chk_ori_infonavit = 0
            LET v_chk_ori_todos = 0
         END IF 
      ON CHANGE chk_via_dev_auto
         IF NOT v_chk_via_dev_auto THEN 
            LET v_chk_ori_infonavit = 0
            LET v_chk_ori_todos = 0
         END IF 
      ON CHANGE chk_via_crm
         IF NOT v_chk_via_crm THEN 
            LET v_chk_ori_infonavit = 0
            LET v_chk_ori_todos = 0
         END IF 
      ON CHANGE chk_via_asesor_tel
         IF NOT v_chk_via_asesor_tel THEN 
            LET v_chk_ori_infonavit = 0
            LET v_chk_ori_todos = 0
         END IF 
      ON CHANGE chk_via_mayores
         IF NOT v_chk_via_mayores THEN 
            LET v_chk_ori_siaff = 0
            LET v_chk_ori_todos = 0
         END IF 
      ON CHANGE chk_via_menores
         IF NOT v_chk_via_menores THEN 
            LET v_chk_ori_siaff = 0
            LET v_chk_ori_todos = 0
         END IF 
      ON CHANGE chk_via_mayores_plus
         IF NOT v_chk_via_mayores_plus THEN 
            LET v_chk_ori_siaff = 0
            LET v_chk_ori_todos = 0
         END IF 
      ON CHANGE chk_via_menores_plus
         IF NOT v_chk_via_menores_plus THEN 
            LET v_chk_ori_siaff = 0
            LET v_chk_ori_todos = 0
         END IF 
      ON CHANGE chk_via_convenios
         IF NOT v_chk_via_convenios THEN 
            LET v_chk_ori_excep = 0
            LET v_chk_ori_todos = 0
         END IF 
      ON CHANGE chk_via_historicos
         IF NOT v_chk_via_historicos THEN 
            LET v_chk_ori_excep = 0
            LET v_chk_ori_todos = 0
         END IF 
      ON CHANGE chk_via_laudos
         IF NOT v_chk_via_laudos THEN 
            LET v_chk_ori_excep = 0
            LET v_chk_ori_todos = 0
         END IF 
      ON CHANGE chk_via_amparos
         IF NOT v_chk_via_amparos THEN 
            LET v_chk_ori_excep = 0
            LET v_chk_ori_todos = 0
         END IF 
      ON CHANGE chk_via_daps
         IF NOT v_chk_via_daps THEN 
            LET v_chk_ori_excep = 0
            LET v_chk_ori_todos = 0
         END IF 
      ON CHANGE chk_via_embargos
         IF NOT v_chk_via_embargos THEN 
            LET v_chk_ori_excep = 0
            LET v_chk_ori_todos = 0
         END IF 
      ON CHANGE chk_ori_todos
         IF v_chk_ori_todos THEN 
            LET v_chk_ori_infonavit   = 1
            LET v_chk_ori_excep = 1
            LET v_chk_ori_afore = 1
            LET v_chk_ori_siaff = 1
            -- Se marcan tambien las vias de pago
            LET v_chk_via_daps           = 1
            LET v_chk_via_afore          = 1
            LET v_chk_via_amparos        = 1
            LET v_chk_via_convenios      = 1
            LET v_chk_via_embargos       = 1
            LET v_chk_via_historicos     = 1
            LET v_chk_via_infonavit      = 1
            LET v_chk_via_laudos         = 1
            LET v_chk_via_mayores        = 1
            LET v_chk_via_mayores_plus   = 1
            LET v_chk_via_menores        = 1
            LET v_chk_via_menores_plus   = 1
            LET v_chk_via_tableta        = 1
            LET v_chk_via_dev_auto       = 1
            LET v_chk_via_crm            = 1
            LET v_chk_via_asesor_tel     = 1
            
         ELSE 
            LET v_chk_ori_infonavit   = 0
            LET v_chk_ori_excep = 0
            LET v_chk_ori_afore = 0
            LET v_chk_ori_siaff = 0
            -- Se desmarcan tambien las vias de pago
            LET v_chk_via_daps           = 0
            LET v_chk_via_afore          = 0
            LET v_chk_via_amparos        = 0
            LET v_chk_via_convenios      = 0
            LET v_chk_via_embargos       = 0
            LET v_chk_via_historicos     = 0
            LET v_chk_via_infonavit      = 0
            LET v_chk_via_laudos         = 0
            LET v_chk_via_mayores        = 0
            LET v_chk_via_mayores_plus   = 0
            LET v_chk_via_menores        = 0
            LET v_chk_via_menores_plus   = 0
            LET v_chk_via_tableta        = 0
            LET v_chk_via_dev_auto       = 0
            LET v_chk_via_crm            = 0
            LET v_chk_via_asesor_tel     = 0
         END IF 
      ON CHANGE chk_estatus_todos
         IF v_chk_estatus_todos THEN 
            LET v_chk_estatus_8 = 1
            LET v_chk_estatus_15 = 1
            LET v_chk_estatus_10 = 1
            LET v_chk_estatus_50 = 1
            LET v_chk_estatus_60 = 1
            LET v_chk_estatus_69 = 1
            LET v_chk_estatus_70 = 1
            LET v_chk_estatus_71 = 1
            LET v_chk_estatus_72 = 1
            LET v_chk_estatus_73 = 1
            LET v_chk_estatus_90 = 1
            LET v_chk_estatus_100 = 1
            LET v_chk_estatus_200 = 1
            LET v_chk_estatus_209 = 1
            LET v_chk_estatus_210 = 1
            LET v_chk_estatus_214 = 1
            LET v_chk_estatus_700 = 1
            LET v_chk_estatus_710 = 1
            LET v_chk_estatus_720 = 1
            LET v_chk_estatus_730 = 1
            LET v_chk_estatus_790 = 1
         ELSE 
            LET v_chk_estatus_8 = 0
            LET v_chk_estatus_10 = 0
            LET v_chk_estatus_15 = 0
            LET v_chk_estatus_50 = 0
            LET v_chk_estatus_60 = 0
            LET v_chk_estatus_69 = 0
            LET v_chk_estatus_70 = 0
            LET v_chk_estatus_71 = 0
            LET v_chk_estatus_72 = 0
            LET v_chk_estatus_73 = 0
            LET v_chk_estatus_90 = 0
            LET v_chk_estatus_100 = 0
            LET v_chk_estatus_200 = 0
            LET v_chk_estatus_209 = 0
            LET v_chk_estatus_210 = 0
            LET v_chk_estatus_214 = 0
            LET v_chk_estatus_700 = 0
            LET v_chk_estatus_710 = 0
            LET v_chk_estatus_720 = 0
            LET v_chk_estatus_730 = 0
            LET v_chk_estatus_790 = 0
         END IF 
      ON ACTION ACCEPT

         -- se borran los arreglos de despliegue
         DISPLAY "El nss es ", v_nss
         --modificación de validación de  captura de parametros
         --valida que se ingrese al menos un parametro
         IF ( v_nss                    IS NULL AND 
              v_d_f_pago_ini           IS NULL AND v_d_f_pago_fin          IS NULL AND
              v_d_f_sol_ini            IS NULL AND v_d_f_sol_fin           IS NULL AND
              v_d_f_autoriza_ini       IS NULL AND v_d_f_autoriza_fin      IS NULL AND
              v_chk_gpo_todos          = 0     AND v_chk_gpo_1             = 0     AND
              v_chk_gpo_2              = 0     AND v_chk_gpo_3             = 0     AND
              v_chk_gpo_4              = 0     AND v_chk_ori_todos         = 0     AND
              v_chk_ori_infonavit      = 0     AND v_chk_ori_excep         = 0     AND
              v_chk_ori_afore          = 0     AND v_chk_ori_siaff         = 0     AND
              v_chk_via_daps           = 0     AND
              v_chk_via_afore          = 0     AND v_chk_via_amparos       = 0     AND
              v_chk_via_convenios      = 0     AND v_chk_via_embargos      = 0     AND
              v_chk_via_historicos     = 0     AND v_chk_via_infonavit     = 0     AND
              v_chk_via_laudos         = 0     AND v_chk_via_mayores       = 0     AND
              v_chk_via_mayores_plus   = 0     AND v_chk_via_menores       = 0     AND
              v_chk_via_menores_plus   = 0     AND v_chk_via_tableta       = 0     AND
              v_chk_via_dev_auto       = 0     AND v_chk_via_crm           = 0     AND
              v_chk_estatus_todos      = 0     AND v_chk_via_asesor_tel    = 0     AND 
              v_chk_estatus_10         = 0     AND v_chk_estatus_8         = 0     AND
              v_chk_estatus_60         = 0     AND v_chk_estatus_69        = 0     AND
              v_chk_estatus_70         = 0     AND v_chk_estatus_15        = 0     AND
              v_chk_estatus_71         = 0     AND v_chk_estatus_72        = 0     AND
              v_chk_estatus_73         = 0     AND v_chk_estatus_90        = 0     AND
              v_chk_estatus_100        = 0     AND v_chk_estatus_200       = 0     AND
              v_chk_estatus_209        = 0     AND v_chk_estatus_210       = 0     AND
              v_chk_estatus_214        = 0     AND v_chk_estatus_700       = 0     AND
              v_chk_estatus_710        = 0     AND v_chk_estatus_720       = 0     AND
              v_chk_estatus_730        = 0     AND
              v_chk_estatus_790        = 0     AND v_chk_estatus_50        = 0     ) THEN
            CALL fn_mensaje("Consulta","Debe de ingresar al menos un criterio de búsqueda","about")
            CONTINUE INPUT
         ELSE 
            IF (v_d_f_sol_ini IS NOT NULL AND v_d_f_sol_fin IS NULL ) OR
               (v_d_f_sol_ini IS NULL AND v_d_f_sol_fin IS NOT NULL ) THEN 
               CALL fn_mensaje("Consulta","Debe Capturar ambas fechas de solicitud","about")
               CONTINUE INPUT 
            ELSE 
               IF (v_d_f_pago_ini IS NOT NULL AND v_d_f_pago_fin IS NULL ) OR
                  (v_d_f_pago_ini IS NULL AND v_d_f_pago_fin IS NOT NULL ) THEN 
                  CALL fn_mensaje("Consulta","Debe Capturar ambas fechas de pago","about")
                  CONTINUE INPUT 
               ELSE 
                  IF (v_d_f_autoriza_ini IS NOT NULL AND v_d_f_autoriza_fin IS NULL ) OR
                     (v_d_f_autoriza_ini IS NULL AND v_d_f_autoriza_fin IS NOT NULL ) THEN 
                     CALL fn_mensaje("Consulta","Debe Capturar ambas fechas de autorización","about")
                     CONTINUE INPUT 
                  ELSE
                     -- Obtenemos el id_peticion al que se asocia la consulta
                     SELECT seq_ret_datos_extrac_ssv.nextval 
                     INTO   v_id_peticion
                     FROM   systables
                     WHERE  tabid = 1;

                     -- Relación de datos pasados a la funcion fn_llena_tabla
                     --  1 - NSS
                     --  2 - Fecha inicio Pago FICO
                     --  3 - Fecha fin Pago FICO
                     --  4 - Fecha inicio Solicitud
                     --  5 - Fecha fin Solicitud
                     --  6 - Fecha inicio Autorización
                     --  7 - Fecha fin Autorización
                     -- 23 - Grupo
                     -- 25 - Estado Solicitud
                     -- 27 - Origen
                     -- 28 - Origen Clave
                     -- 29 - Vias de Pago
                     IF v_nss IS NOT NULL THEN
                        CALL fn_llena_tabla (v_id_peticion,v_nss, 1) RETURNING v_resultado  
                     END IF 
                     IF v_d_f_sol_ini IS NOT NULL AND v_d_f_sol_fin IS NOT NULL THEN
                        IF v_d_f_sol_ini <= v_d_f_sol_fin THEN 
                           INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,v_d_f_sol_ini,4)
                           INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,v_d_f_sol_fin,5)
                           DISPLAY "Inserto fechas de solicitud"
                        END IF 
                     END IF 
                     IF v_d_f_pago_ini IS NOT NULL AND v_d_f_pago_fin IS NOT NULL THEN
                        IF v_d_f_pago_ini <= v_d_f_pago_fin THEN 
                           INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,v_d_f_pago_ini,2)
                           INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,v_d_f_pago_fin,3)
                           DISPLAY "Inserto fechas de pago"
                        END IF 
                     END IF 
                     IF v_d_f_autoriza_ini IS NOT NULL AND v_d_f_autoriza_fin IS NOT NULL THEN
                        IF v_d_f_autoriza_ini <= v_d_f_autoriza_fin THEN 
                           INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,v_d_f_autoriza_ini,6)
                           INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,v_d_f_autoriza_fin,7)
                           DISPLAY "Inserto fechas de autorizacion"
                        END IF 
                     END IF 
                             
                     IF v_chk_gpo_1 OR v_chk_gpo_2 OR v_chk_gpo_3 OR v_chk_gpo_4 THEN
                        DISPLAY "Se marco al menos una grupo"
                        IF v_chk_gpo_1 THEN 
                           INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,"201",23)
                           INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,"101",23)
                           INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,"1",23)
                        END IF 
                        IF v_chk_gpo_2 THEN 
                           INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,"102",23)
                           INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,"2",23)
                        END IF 
                        IF v_chk_gpo_3 THEN 
                           INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,"103",23)
                           INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,"3",23)
                        END IF 
                        IF v_chk_gpo_4 THEN 
                           INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,"504",23)
                           INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,"104",23)
                           INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,"114",23)
                           INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,"124",23)
                           INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,"4",23)
                        END IF 
                     END IF
                     IF (v_chk_ori_infonavit OR v_chk_ori_excep     OR 
                         v_chk_ori_afore     OR v_chk_ori_siaff ) THEN
                        DISPLAY "Se marco al menos un origen"
                        IF v_chk_ori_infonavit THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'INFONAVIT',27) END IF 
                        IF v_chk_ori_excep     THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'EXCEPCIONES',27) END IF 
                        IF v_chk_ori_afore     THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'AFORE',27) END IF 
                        IF v_chk_ori_siaff     THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'SIAFF',27) END IF
                        
                        IF v_chk_ori_infonavit THEN 
                           INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'1',28) 
                           INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'2',28) 
                           INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'3',28) 
                        END IF 
                        IF v_chk_ori_excep     THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'0',28) END IF 
                        IF v_chk_ori_afore     THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'5',28) END IF 
                        IF v_chk_ori_siaff     THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'0',28) END IF 
                     END IF
                     IF (v_chk_via_daps           OR v_chk_via_afore          OR
                         v_chk_via_amparos        OR v_chk_via_convenios      OR
                         v_chk_via_embargos       OR v_chk_via_historicos     OR
                         v_chk_via_infonavit      OR v_chk_via_laudos         OR
                         v_chk_via_mayores        OR v_chk_via_mayores_plus   OR
                         v_chk_via_menores        OR v_chk_via_menores_plus   OR
                         v_chk_via_tableta        OR v_chk_via_dev_auto       OR
                         v_chk_via_crm            OR v_chk_via_asesor_tel ) THEN
                        DISPLAY "Se marco al menos una via de pago"
                        IF v_chk_via_daps         THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'DAPS',29) END IF 
                        IF v_chk_via_afore        THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'AFORE',29) END IF 
                        IF v_chk_via_amparos      THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'Amparo',29) END IF 
                        IF v_chk_via_convenios    THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'CONVENIOS',29) END IF 
                        IF v_chk_via_embargos     THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'EMBARGOS',29) END IF 
                        IF v_chk_via_historicos   THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'HISTORICOS',29) END IF 
                        IF v_chk_via_infonavit    THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'INFONAVIT',29) END IF 
                        IF v_chk_via_laudos       THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'Laudo',29) END IF 
                        IF v_chk_via_mayores      THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'Mayores',29) END IF 
                        IF v_chk_via_mayores_plus THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'Mayores$',29) END IF 
                        IF v_chk_via_mayores_plus THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'Mayores$',29) END IF 
                        IF v_chk_via_menores      THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'Menores',29) END IF 
                        IF v_chk_via_menores_plus THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'Menores$',29) END IF 
                        IF v_chk_via_tableta      THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'TABLETA',29) END IF 
                        IF v_chk_via_dev_auto     THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'Dev Auto',29) END IF 
                        IF v_chk_via_crm          THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'CRM',29) END IF 
                        IF v_chk_via_asesor_tel   THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'ASESOR TELEFONICO',29) END IF 
                     END IF
                     IF (v_chk_estatus_10 OR v_chk_estatus_8 OR v_chk_estatus_60 OR v_chk_estatus_69 OR 
                        v_chk_estatus_70 OR v_chk_estatus_15 OR v_chk_estatus_71 OR v_chk_estatus_72 OR 
                        v_chk_estatus_73 OR v_chk_estatus_90 OR v_chk_estatus_100 OR v_chk_estatus_200 OR 
                        v_chk_estatus_209 OR v_chk_estatus_210 OR v_chk_estatus_214 OR v_chk_estatus_700 OR 
                        v_chk_estatus_710 OR v_chk_estatus_720 OR v_chk_estatus_730 OR v_chk_estatus_790 OR 
                        v_chk_estatus_50)  THEN
                        DISPLAY "Entra a validar los estados "
                        IF v_chk_estatus_8 THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'8',25) END IF 
                        IF v_chk_estatus_10 THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'10',25) END IF 
                        IF v_chk_estatus_15 THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'15',25) END IF 
                        IF v_chk_estatus_50 THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'50',25) END IF 
                        IF v_chk_estatus_60 THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'60',25) END IF 
                        IF v_chk_estatus_69 THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'69',25) END IF 
                        IF v_chk_estatus_70 THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'70',25) END IF 
                        IF v_chk_estatus_71 THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'71',25) END IF 
                        IF v_chk_estatus_72 THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'72',25) END IF 
                        IF v_chk_estatus_73 THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'73',25) END IF 
                        IF v_chk_estatus_90 THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'90',25) END IF 
                        IF v_chk_estatus_100 THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'100',25) END IF 
                        IF v_chk_estatus_200 THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'200',25) END IF 
                        IF v_chk_estatus_209 THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'209',25) END IF 
                        IF v_chk_estatus_210 THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'210',25) END IF 
                        IF v_chk_estatus_214 THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'214',25) END IF 
                        IF v_chk_estatus_700 THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'700',25) END IF 
                        IF v_chk_estatus_710 THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'710',25) END IF 
                        IF v_chk_estatus_720 THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'720',25) END IF 
                        IF v_chk_estatus_730 THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'730',25) END IF 
                        IF v_chk_estatus_790 THEN INSERT INTO ret_datos_extrac_ssv VALUES (v_id_peticion,'790',25) END IF 
                     END IF
                     -- se verifica si se puede continuar con la operacion
                     LET v_resultado = fn_valida_operacion(0,g_proceso_cod,g_opera_cod)
                     
                     IF ( v_resultado = 0 ) THEN

                        -- se genera el pid 
                        CALL fn_genera_pid(g_proceso_cod, g_opera_cod, p_usuario_cod) RETURNING g_pid

                        CALL fn_inicializa_proceso(g_pid             ,
                                                   g_proceso_cod     ,
                                                   g_opera_cod       ,
                                                   0                 ,
                                                   "RETL469"         ,
                                                   "NA"              ,
                                                   p_usuario_cod)  RETURNING v_resultado
                                                    
                        -- el proceso se registro correctamente
                        IF ( v_resultado = 0 ) THEN

                           -- inicia la operacion
                           CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,g_opera_cod,0,"RETL469","NA",p_usuario_cod)
                           RETURNING v_resultado
                        
                           -- se invoca la ejecucion del programa lanzado
                           LET v_s_comando = "nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/RETP469 ",
                                              p_usuario_cod CLIPPED, " ",
                                              g_pid                , " " ,
                                              g_proceso_cod        , " " ,
                                              g_opera_cod          , " ",
                                              0                    , " ",
                                              "NA"                 , " ",
                                              v_id_peticion        , " ",
                                              " 1>",seg_modulo_bat.ruta_listados CLIPPED ,
                                              "/nohup:",g_pid  USING "&&&&&",":",
                                              g_proceso_cod    USING "&&&&&",":",
                                              g_opera_cod      USING "&&&&&" ,
                                              " 2>&1 &"
                                            
                           DISPLAY v_s_comando
                           RUN v_s_comando
                           CALL fn_mensaje("Atención",
                                  "Se ha enviado el programa extractor de datos del SSV.\nPodrá revisar el resultado en el monitor de ejecución de procesos",
                                  "information")

                        ELSE
                           CALL fn_mensaje("Atención","No se pudo iniciar el proceso","information")
                        END IF
                     ELSE
                        -- no se puede ejecutar la operacion
                        CALL fn_recupera_inconsis_opera(v_resultado) RETURNING v_mensaje
                        CALL fn_mensaje("Atención", v_mensaje, "stop")
                     END IF 
                  END IF -- No se capturaron ambas dechas de autorización 
               END IF -- No se capturaron ambas fechas de Pago
            END IF -- No se capturaron ambas fechas de solicitud
         END IF  -- No se digitaron criterios de búsqueda         
        
      ON ACTION CANCEL
         EXIT INPUT
   
   END INPUT
   
   CLOSE WINDOW w_consulta_pagos

END FUNCTION

FUNCTION fn_llena_tabla(p_id_peticion,p_dato,p_campo)
DEFINE  p_id_peticion  DECIMAL(9,0)
DEFINE  p_dato         STRING
DEFINE  p_campo        SMALLINT
DEFINE  v_respuesta SMALLINT 
DEFINE  i           INTEGER 
DEFINE  v_dato       CHAR(25)
DEFINE  v_num_reg   INTEGER 
DEFINE  v_query     STRING 



   LET v_dato = ""
   LET v_respuesta = 0
   LET v_num_reg = 0
   LET v_query = "INSERT INTO ret_datos_extrac_ssv VALUES (?,?,?)"
   PREPARE prp_inserta_datos FROM v_query

   LET v_query = "SELECT COUNT(*)",
                  " FROM ret_datos_extrac_ssv  WHERE id_peticion = ", p_id_peticion, " AND ind_dato = ", p_campo
                  
   PREPARE prp_cuenta_insertados FROM v_query

   FOR i = 1 TO p_dato.getLength()
      IF i = 1 THEN
         LET v_dato = v_dato CLIPPED, p_dato.subString(i,i)
      ELSE
         IF p_dato.subString(i,i) = "\n" THEN 
            EXECUTE prp_inserta_datos USING  p_id_peticion, v_dato, p_campo
            LET v_dato = ""
         ELSE 
            LET v_dato = v_dato CLIPPED, p_dato.subString(i,i)
         END IF 
      END IF 
   END FOR 
   IF i > 1 AND p_dato.subString(i,i) <> "\n" THEN 
      EXECUTE prp_inserta_datos USING  v_dato
   END IF 

   EXECUTE prp_cuenta_insertados INTO v_num_reg 

   DISPLAY "Se insertaron en la temporal :", v_num_reg, " de la petición :", p_id_peticion, " del campo ", p_campo
      

RETURN v_respuesta
END FUNCTION 