--===============================================================
-- Version: 1.0.0
--===============================================================

#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETC469                                                                #
#Objetivo     => Consulta y Extracción de Pagos de la Devolición del SSV                #
#Fecha inicio => Abril 23, 2018                                                         # 
#########################################################################################

DATABASE safre_viv

GLOBALS "RETG01.4gl"  -- se agrega archivo globales y se sustituyen las variables necesarias

GLOBALS
	DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod -- clave del usuario firmado
          ,v_ventana        ui.WINDOW
          ,f_forma          ui.Form   
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

   --Se inicia el log del programa
   CALL STARTLOG (p_usuario_cod CLIPPED||".RETC469.log")
   
   -- consulta de informacion recibida 
   CALL fn_consulta_detalle(p_usuario_cod)

END MAIN

{ ============================================================================
Clave: RETC469
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
       v_precio_fondo                LIKE ret_cza_transferencia.precio_fondo
       
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
   OPEN WINDOW w_consulta_pagos WITH FORM "RETC4691"
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
         v_chk_estatus_710,       v_chk_estatus_720,        v_chk_estatus_790
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
         chk_estatus_710,         chk_estatus_720,          chk_estatus_790
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
                     IF v_nss IS NOT NULL THEN 
                        LET v_query_where = v_query_where, " AND nss IN (SELECT nss FROM tmp_nss) \n"
                        LET v_query_where_act = v_query_where_act, " AND rsg.nss IN (SELECT nss FROM tmp_nss) \n"
                        CALL fn_llena_tabla_nss (v_nss) RETURNING v_resultado
                     END IF 
                     IF v_d_f_sol_ini IS NOT NULL AND v_d_f_sol_fin IS NOT NULL THEN
                        IF v_d_f_sol_ini < v_d_f_sol_fin THEN 
                           LET v_query_where = v_query_where, " AND fecha_tramite BETWEEN '", v_d_f_sol_ini, "' AND '" , v_d_f_sol_fin, "' \n"
                           LET v_query_where_act = v_query_where_act, " AND rsg.f_solicitud BETWEEN '", v_d_f_sol_ini, "' AND '" , v_d_f_sol_fin, "' \n"
                        END IF 
                     END IF 
                     IF v_d_f_pago_ini IS NOT NULL AND v_d_f_pago_fin IS NOT NULL THEN
                        IF v_d_f_pago_ini < v_d_f_pago_fin THEN 
                           LET v_query_where = v_query_where, " AND fecha_pago BETWEEN '", v_d_f_pago_ini, "' AND '" , v_d_f_pago_fin, "' \n"
                           LET v_query_where_act = v_query_where_act, " AND MDY(SUBSTR(rwcpf.rsp_f_pago,5,2),SUBSTR(rwcpf.rsp_f_pago,7,2),SUBSTR(rwcpf.rsp_f_pago,1,4)) BETWEEN '", v_d_f_pago_ini, "' AND '" , v_d_f_pago_fin, "' \n"
                        END IF 
                     END IF 
                     IF v_d_f_autoriza_ini IS NOT NULL AND v_d_f_autoriza_fin IS NOT NULL THEN
                        IF v_d_f_autoriza_ini < v_d_f_autoriza_fin THEN 
                           LET v_query_where = v_query_where, " AND fecha_autorizacion BETWEEN '", v_d_f_autoriza_ini, "' AND '" , v_d_f_autoriza_fin, "' \n"
                           LET v_query_where_act = v_query_where_act, " AND rwpm.fecha_autorizacion BETWEEN '", v_d_f_autoriza_ini, "' AND '" , v_d_f_autoriza_fin, "' \n"
                        END IF 
                     END IF 
                     
                     IF v_chk_gpo_1 OR v_chk_gpo_2 OR v_chk_gpo_3 OR v_chk_gpo_4 THEN
                        DROP TABLE IF EXISTS tmp_grupo;
                        CREATE TEMP TABLE tmp_grupo (grupo SMALLINT);
                        DISPLAY "Se marco al menos una grupo"
                        IF v_chk_gpo_1 THEN 
                           INSERT INTO tmp_grupo VALUES (201) 
                           INSERT INTO tmp_grupo VALUES (101)
                           INSERT INTO tmp_grupo VALUES (1)
                        END IF 
                        IF v_chk_gpo_2 THEN 
                           INSERT INTO tmp_grupo VALUES (102) 
                           INSERT INTO tmp_grupo VALUES (2)
                        END IF 
                        IF v_chk_gpo_3 THEN 
                           INSERT INTO tmp_grupo VALUES (103) 
                           INSERT INTO tmp_grupo VALUES (3) 
                        END IF 
                        IF v_chk_gpo_4 THEN 
                           INSERT INTO tmp_grupo VALUES (504) 
                           INSERT INTO tmp_grupo VALUES (104)
                           INSERT INTO tmp_grupo VALUES (114)
                           INSERT INTO tmp_grupo VALUES (124)
                           INSERT INTO tmp_grupo VALUES (4) 
                        END IF 
                        LET v_query_where = v_query_where, " AND grupo IN (SELECT grupo FROM tmp_grupo) \n"
                        LET v_query_where_act = v_query_where_act, " AND rlg.gpo_ley73 IN (SELECT grupo FROM tmp_grupo) \n"
                     END IF
                     IF (v_chk_ori_infonavit OR v_chk_ori_excep     OR 
                         v_chk_ori_afore     OR v_chk_ori_siaff ) THEN
                        DROP TABLE IF EXISTS tmp_origen;
                        CREATE TEMP TABLE tmp_origen (origen CHAR(14));
                        DROP TABLE IF EXISTS tmp_origen_codigo;
                        CREATE TEMP TABLE tmp_origen_codigo (origen_codigo SMALLINT);
                        DISPLAY "Se marco al menos un origen"
                        IF v_chk_ori_infonavit THEN INSERT INTO tmp_origen VALUES ('INFONAVIT') END IF 
                        IF v_chk_ori_excep     THEN INSERT INTO tmp_origen VALUES ('EXCEPCIONES') END IF 
                        IF v_chk_ori_afore     THEN INSERT INTO tmp_origen VALUES ('AFORE') END IF 
                        IF v_chk_ori_siaff     THEN INSERT INTO tmp_origen VALUES ('SIAFF') END IF 
                        IF v_chk_ori_infonavit THEN 
                           INSERT INTO tmp_origen_codigo VALUES (1) 
                           INSERT INTO tmp_origen_codigo VALUES (2) 
                           INSERT INTO tmp_origen_codigo VALUES (3) 
                           INSERT INTO tmp_origen_codigo VALUES (4) 
                        END IF 
                        IF v_chk_ori_excep     THEN INSERT INTO tmp_origen_codigo VALUES (0) END IF 
                        IF v_chk_ori_afore     THEN INSERT INTO tmp_origen_codigo VALUES (5) END IF 
                        IF v_chk_ori_siaff     THEN INSERT INTO tmp_origen_codigo VALUES (0) END IF 
                        LET v_query_where = v_query_where, " AND origen IN (SELECT origen FROM tmp_origen) \n"
                        LET v_query_where_act = v_query_where_act, " AND rsme.medio_entrega IN (SELECT origen_codigo FROM tmp_origen_codigo) \n"
                     END IF
                     IF (v_chk_via_daps           OR v_chk_via_afore          OR
                         v_chk_via_amparos        OR v_chk_via_convenios      OR
                         v_chk_via_embargos       OR v_chk_via_historicos     OR
                         v_chk_via_infonavit      OR v_chk_via_laudos         OR
                         v_chk_via_mayores        OR v_chk_via_mayores_plus   OR
                         v_chk_via_menores        OR v_chk_via_menores_plus   OR
                         v_chk_via_tableta        OR v_chk_via_dev_auto       OR
                         v_chk_via_crm            OR v_chk_via_asesor_tel ) THEN
                        DROP TABLE IF EXISTS tmp_via_pago;
                        CREATE TEMP TABLE tmp_via_pago (via_pago CHAR(14));
                        DISPLAY "Se marco al menos una via de pago"
                        IF v_chk_via_daps         THEN INSERT INTO tmp_via_pago VALUES ('DAPS') END IF 
                        IF v_chk_via_afore        THEN INSERT INTO tmp_via_pago VALUES ('AFORE') END IF 
                        IF v_chk_via_amparos      THEN INSERT INTO tmp_via_pago VALUES ('Amparo') END IF 
                        IF v_chk_via_convenios    THEN INSERT INTO tmp_via_pago VALUES ('CONVENIOS') END IF 
                        IF v_chk_via_embargos     THEN INSERT INTO tmp_via_pago VALUES ('EMBARGOS') END IF 
                        IF v_chk_via_historicos   THEN INSERT INTO tmp_via_pago VALUES ('HISTORICOS') END IF 
                        IF v_chk_via_infonavit    THEN INSERT INTO tmp_via_pago VALUES ('INFONAVIT') END IF 
                        IF v_chk_via_laudos       THEN INSERT INTO tmp_via_pago VALUES ('Laudo') END IF 
                        IF v_chk_via_mayores      THEN INSERT INTO tmp_via_pago VALUES ('Mayores') END IF 
                        IF v_chk_via_mayores_plus THEN INSERT INTO tmp_via_pago VALUES ('Mayores$') END IF 
                        IF v_chk_via_mayores_plus THEN INSERT INTO tmp_via_pago VALUES ('Mayores$') END IF 
                        IF v_chk_via_menores      THEN INSERT INTO tmp_via_pago VALUES ('Menores') END IF 
                        IF v_chk_via_menores_plus THEN INSERT INTO tmp_via_pago VALUES ('Menores$') END IF 
                        IF v_chk_via_tableta      THEN INSERT INTO tmp_via_pago VALUES ('TABLETA') END IF 
                        IF v_chk_via_dev_auto     THEN INSERT INTO tmp_via_pago VALUES ('Dev Auto') END IF 
                        IF v_chk_via_crm          THEN INSERT INTO tmp_via_pago VALUES ('CRM') END IF
                       IF v_chk_via_asesor_tel    THEN INSERT INTO tmp_via_pago VALUES ('ASESOR TELEFONICO') END IF  
                        LET v_query_where = v_query_where, " AND via_pago IN (SELECT via_pago FROM tmp_via_pago) \n"
                        LET v_query_where_act = v_query_where_act, " AND rcme.descripcion IN (SELECT via_pago FROM tmp_via_pago) \n"
                     END IF
                     IF (v_chk_estatus_10 OR v_chk_estatus_8 OR v_chk_estatus_60 OR v_chk_estatus_69 OR 
                        v_chk_estatus_70 OR v_chk_estatus_15 OR v_chk_estatus_71 OR v_chk_estatus_72 OR 
                        v_chk_estatus_73 OR v_chk_estatus_90 OR v_chk_estatus_100 OR v_chk_estatus_200 OR 
                        v_chk_estatus_209 OR v_chk_estatus_210 OR v_chk_estatus_214 OR v_chk_estatus_700 OR 
                        v_chk_estatus_710 OR v_chk_estatus_720 OR v_chk_estatus_790 OR 
                        v_chk_estatus_50)  THEN
                        DROP TABLE IF EXISTS tmp_estados;
                        CREATE TEMP TABLE tmp_estados (estado SMALLINT);
                        DISPLAY "Entra a validar los estados "
                        IF v_chk_estatus_8 THEN INSERT INTO tmp_estados VALUES (8) END IF 
                        IF v_chk_estatus_10 THEN INSERT INTO tmp_estados VALUES (10) END IF 
                        IF v_chk_estatus_15 THEN INSERT INTO tmp_estados VALUES (15) END IF 
                        IF v_chk_estatus_50 THEN INSERT INTO tmp_estados VALUES (50) END IF 
                        IF v_chk_estatus_60 THEN INSERT INTO tmp_estados VALUES (60) END IF 
                        IF v_chk_estatus_69 THEN INSERT INTO tmp_estados VALUES (69) END IF 
                        IF v_chk_estatus_70 THEN INSERT INTO tmp_estados VALUES (70) END IF 
                        IF v_chk_estatus_71 THEN INSERT INTO tmp_estados VALUES (71) END IF 
                        IF v_chk_estatus_72 THEN INSERT INTO tmp_estados VALUES (72) END IF 
                        IF v_chk_estatus_73 THEN INSERT INTO tmp_estados VALUES (73) END IF 
                        IF v_chk_estatus_90 THEN INSERT INTO tmp_estados VALUES (90) END IF 
                        IF v_chk_estatus_100 THEN INSERT INTO tmp_estados VALUES (100) END IF 
                        IF v_chk_estatus_200 THEN INSERT INTO tmp_estados VALUES (200) END IF 
                        IF v_chk_estatus_209 THEN INSERT INTO tmp_estados VALUES (209) END IF 
                        IF v_chk_estatus_210 THEN INSERT INTO tmp_estados VALUES (210) END IF 
                        IF v_chk_estatus_214 THEN INSERT INTO tmp_estados VALUES (214) END IF 
                        IF v_chk_estatus_700 THEN INSERT INTO tmp_estados VALUES (700) END IF 
                        IF v_chk_estatus_710 THEN INSERT INTO tmp_estados VALUES (710) END IF 
                        IF v_chk_estatus_720 THEN INSERT INTO tmp_estados VALUES (720) END IF 
                        IF v_chk_estatus_790 THEN INSERT INTO tmp_estados VALUES (790) END IF 
                        LET v_query_where = v_query_where, " AND estado_solicitud IN (SELECT estado FROM tmp_estados) \n"
                        LET v_query_where_act = v_query_where_act, " AND rsg.estado_solicitud IN (SELECT estado FROM tmp_estados) \n"
                     END IF
                     DROP TABLE IF EXISTS tmp_ret_ley73_hist;
                     DROP TABLE IF EXISTS tmp_ret_ley73_actual;
                     --- Crea la tabla vacia
                     LET v_query = "\n SELECT *              ",
                                   "\n FROM   ret_ley73_hist ", 
                                   "\n WHERE  1 = 0 ",
                                   "INTO TEMP tmp_ret_ley73_hist "
                     -- consulta INSERT
                     DISPLAY "Creacion de la tabla:\n", v_query

                     -- se llena el arreglo con los historicos 
                     PREPARE sid_query_vacia FROM v_query
                     EXECUTE sid_query_vacia
                                     
                     LET v_query = "\n INSERT INTO tmp_ret_ley73_hist  ",
                                   "\n SELECT *                        ",
                                   "\n FROM   ret_ley73_hist           ", 
                                   "\n WHERE  1 = 1                    ",
                                   v_query_where

                     -- consulta INSERT
                     DISPLAY "Consulta Insert del Historico:\n", v_query

                     -- se llena el arreglo con los historicos 
                     PREPARE sid_query FROM v_query
                     EXECUTE sid_query

                     -- Se crea una temporal y su indice para eficientar la busqueda
                     LET v_query = "\n DROP TABLE IF EXISTS tmp_ret_ws_pet;              ",
                                   "\n SELECT MAX(rp.f_peticion) AS            ",
                                   "\n        fecha_autorizacion,              ",
                                   "\n        rp.caso_adai                     ", 
                                   "\n FROM   ret_ws_peticion_marca rp,        ",
                                   "\n        ret_ws_det_peticion_marca rd     ",
                                   "\n WHERE  rp.id_peticion = rd.id_peticion  ",
                                   "\n AND    rd.ind_marca = 3                 ",
                                   "\n AND    rd.modalidad_retiro = 3          ",
                                   "\n GROUP BY rp.caso_adai                   ",
                                   "\n INTO TEMP tmp_ret_ws_pet;               "
                     -- consulta INSERT
                     DISPLAY "Crea la temporal de las peticiones :\n", v_query

                     -- se llena el arreglo con los historicos 
                     PREPARE sid_query_pet FROM v_query
                     EXECUTE sid_query_pet

                     LET v_query = "\n INSERT INTO tmp_ret_ws_pet               ",
                                   "\n SELECT MAX(rp.f_peticion) AS             ",
                                   "\n        fecha_autorizacion,               ",       
                                   "\n        rp.caso_crm                       ",         
                                   "\n FROM   ret_ws_peticion_act_benef rp      ",
                                   "\n GROUP BY rp.caso_crm                     "
                                   
                     PREPARE sid_query_pet_benef FROM v_query
                     EXECUTE sid_query_pet_benef
                     
                     -- se crea el indice para la tabla temporal
                     LET v_query = "\n CREATE INDEX idxtmp_ret_ws_pet ON       ",
                                   "\n tmp_ret_ws_pet (caso_adai) USING btree ;"
                     -- se llena el arreglo con los historicos 
                     PREPARE sid_query_idx_pet FROM v_query
                     EXECUTE sid_query_idx_pet

                     
                     --- Se integran las solicitudes actuales
                     LET v_query = "SELECT rsg.nss AS nss,                                                                                                            \n", 
                                   "       CASE WHEN rbg.tpo_beneficiario IS NOT NULL                                                                                 \n", 
                                   "            THEN (CASE WHEN rbg.tpo_beneficiario = 1                                                                              \n", 
                                   "                  THEN rsg.estado_solicitud                                                                                       \n", 
                                   "                  ELSE (CASE WHEN rbj.estado_solicitud IS NULL                                                                    \n", 
                                   "                             THEN rsg.estado_solicitud                                                                            \n", 
                                   "                             ELSE rbj.estado_solicitud                                                                            \n", 
                                   "                             END)                                                                                                 \n", 
                                   "                  END)                                                                                                            \n", 
                                   "            ELSE rsg.estado_solicitud                                                                                             \n", 
                                   "            END AS estado_solicitud,                                                                                              \n", 
                                   "       CASE WHEN rbg.tpo_beneficiario IS NOT NULL                                                                                 \n", 
                                   "            THEN (CASE WHEN rbg.tpo_beneficiario = 1                                                                              \n", 
                                   "                       THEN (SELECT des_corta FROM ret_estado_solicitud WHERE estado_solicitud = rsg.estado_solicitud)            \n", 
                                   "                       ELSE (CASE WHEN rbj.estado_solicitud IS NULL                                                               \n", 
                                   "                                  THEN (SELECT des_corta FROM ret_estado_solicitud WHERE estado_solicitud = rsg.estado_solicitud) \n", 
                                   "                                  ELSE (SELECT des_corta FROM ret_estado_solicitud WHERE estado_solicitud = rbj.estado_solicitud) \n", 
                                   "                                  END)                                                                                            \n", 
                                   "                       END)                                                                                                       \n",  
                                   "            ELSE (SELECT des_corta FROM ret_estado_solicitud WHERE estado_solicitud = rsg.estado_solicitud)                       \n", 
                                   "            END AS descripcion,                                                                                                   \n", 
                                   "       CASE WHEN rsme.medio_entrega = 5 THEN 'AFORE'             \n",
                                   "                                        ELSE 'INFONAVIT'         \n",
                                   "       END AS origen,                                            \n",
                                   "       rcme.descripcion AS via_pago, rlg.gpo_ley73 AS grupo,     \n",
                                   "       ad.nombre_af AS nombre, ad.ap_paterno_af AS ap_paterno,   \n",
                                   "       ad.ap_materno_af AS ap_materno,                           \n",
                                   "       CASE WHEN rbg.tpo_beneficiario = 1 THEN 'TITULAR'         \n", 
                                   "            ELSE 'BENEFICIARIO' END AS beneficiario,             \n",
                                   "       rbg.nombre AS nombre_beneficiario,                        \n",
                                   "       rbg.ap_paterno AS ap_paterno_beneficiario,                \n",
                                   "       rbg.ap_materno AS ap_materno_beneficiario,                \n", 
                                   "       ad.curp AS curp, ad.rfc AS rfc,                           \n",
                                   "       rbg.id_entidad_federativa AS entidad,                     \n",
                                   "       rsg.f_solicitud AS fecha_tramite,                         \n",  
                                   "       rwpm.fecha_autorizacion AS fecha_autorizacion,            \n",
                                   "       rrf.cta_x_pagar AS documento, rrf.anho AS ejercicio,      \n",
                                   "       rwcpf.rsp_documento AS documento_pago,                    \n",
                                   "       NVL(MDY(SUBSTR(rwcpf.rsp_f_pago,5,2),SUBSTR(rwcpf.rsp_f_pago,7,2),SUBSTR(rwcpf.rsp_f_pago,1,4)),MDY(1,1,1)) AS fecha_pago,           \n",
                                   "       (ABS(NVL(rpteso.monto_pesos,0)+NVL(rpviv92.monto_pesos,0)+ \n",
                                   "       NVL(rpviv97.monto_pesos,0)))*(rbg.porcentaje/100) AS importe_pago,              \n",
--                                   "       rwcpf.rsp_importe AS importe_pago,                        \n",
                                   "       rwcpf.rsp_referencia AS referencia_pago,                  \n",
                                   "       rsg.caso_adai AS caso_adai, rbj.num_acuerdo AS num_laudo, \n",
                                   "       '' AS num_junta, '' AS importe_pago_anterior,             \n",
                                   "       '' AS fch_pago_anterior,                                  \n",
                                   "       NVL(SUBSTR(rrf.cta_clabe,1,3),'') AS clave_banco,         \n",
                                   "       rrf.cta_clabe AS cuenta, '' AS importe_transf,            \n",
                                   "       '' AS fch_transf, '' AS ssv_dif,                          \n",
                                   "       NVL(MDY(SUBSTR(rbj.fecha_ejecucion,6,2),SUBSTR(rbj.fecha_ejecucion,9,2),SUBSTR(rbj.fecha_ejecucion,1,4)),MDY(1,1,1)) AS fch_marca_tj, \n",
                                   "       '' AS error_fico, '' AS cve_afore,                        \n",
                                   "       rlg.importe_viv97*(rbg.porcentaje/100) AS pesos_viv97,                         \n",
                                   "       rlg.aivs_viv97*(rbg.porcentaje/100) AS aivs_viv97,                             \n",
                                   "       rlg.importe_viv92*(rbg.porcentaje/100) AS pesos_viv92,                         \n",
                                   "       rlg.aivs_viv92*(rbg.porcentaje/100) AS aivs_viv92, rlg.usuario AS usuario,     \n",
                                   "       '' AS cabecera, rrf.acreedor AS acreedor,                 \n",
                                   "       '' AS via_pago_ac, '' AS bloqueo_pago, '' AS anulado,     \n",
                                   "       '' AS ejercicio_anulacion, '' AS fch_carga                \n",
                                   "FROM   ret_estado_solicitud      res,                            \n",
                                   "       ret_sol_medio_entrega     rsme,                           \n",
                                   "       ret_cat_medio_entrega     rcme,                           \n",
                                   "       afi_derechohabiente       ad,                             \n",
                                   "       ret_solicitud_generico    rsg                             \n",
                                   "       LEFT OUTER JOIN ret_ley73_generico        rlg             \n",
                                   "                    ON rsg.id_solicitud = rlg.id_solicitud       \n",
                                   "       LEFT OUTER JOIN ret_preliquida            rpteso          \n",
                                   "                    ON rsg.id_solicitud = rpteso.id_referencia   \n",
                                   "                   AND rpteso.subcuenta = 47                     \n",
                                   "                   AND rpteso.folio_liquida = rsg.folio          \n",
                                   "       LEFT OUTER JOIN ret_preliquida            rpviv92         \n",
                                   "                    ON rsg.id_solicitud = rpviv92.id_referencia  \n",
                                   "                   AND rpviv92.subcuenta = 8                     \n",
                                   "                   AND rpviv92.folio_liquida = rsg.folio         \n",
                                   "       LEFT OUTER JOIN ret_preliquida            rpviv97         \n",
                                   "                    ON rsg.id_solicitud = rpviv97.id_referencia  \n",
                                   "                   AND rpviv97.subcuenta = 4                     \n",
                                   "                   AND rpviv97.folio_liquida = rsg.folio         \n",
                                   "       LEFT OUTER JOIN ret_beneficiario_generico rbg             \n",
                                   "                    ON rsg.id_solicitud = rbg.id_solicitud       \n",
                                   "       LEFT OUTER JOIN ret_beneficiario_juridico rbj             \n",
                                   "                    ON rbj.id_solicitud = rbg.id_solicitud       \n",
                                   "                   AND rbj.consec_beneficiario = rbg.consec_beneficiario  \n",
                                   "       LEFT OUTER JOIN ret_respuesta_fico rrf                    \n",
                                   "                    ON rrf.referencia = rsg.id_solicitud         \n",
                                   "                   AND ((rrf.folio = (SELECT MAX(folio)                             \n",               
                                   "                                      FROM   ret_respuesta_fico                     \n",
                                   "                                      WHERE  referencia = rsg.id_solicitud)) OR     \n", 
                                   "                        (rrf.folio = (SELECT MAX(folio)                             \n",
                                   "                                      FROM   ret_respuesta_fico                     \n",
                                   "                                      WHERE  referencia = rbj.id_solicitud||rbj.consec_beneficiario))) \n",
                                   "       LEFT OUTER JOIN tmp_ret_ws_pet  rwpm                      \n",
                                   "                    ON rwpm.caso_adai = rsg.caso_adai            \n",
                                   "       LEFT OUTER JOIN ret_ws_consulta_pago_fico rwcpf           \n",
                                   "                    ON (rwcpf.id_solicitud = rsg.id_solicitud OR \n",
                                   "                        rwcpf.id_solicitud = rbj.id_solicitud||rbj.consec_beneficiario) \n",
                                   "                   AND rwcpf.rsp_f_pago IS NOT NULL              \n", 
                                   "                   AND rwcpf.rsp_referencia IS NOT NULL          \n",
                                   "                   AND rwcpf.rsp_estatus = 2                     \n",
                                   "WHERE  rsg.estado_solicitud   = res.estado_solicitud             \n",
                                   "AND    rsg.id_solicitud       = rsme.id_solicitud                \n",
                                   "AND    rsme.medio_entrega     = rcme.medio_entrega               \n",
                                   "AND    rsg.id_derechohabiente = ad.id_derechohabiente            \n",
                                   v_query_where_act,
                                   "INTO TEMP tmp_ret_ley73_actual                                     "
                     
                     -- se llena el arreglo con los actuales
                     DISPLAY "El Query actual", v_query
                     PREPARE sid_query_tmp_act FROM v_query
                     EXECUTE sid_query_tmp_act

                     LET v_query = "\n INSERT INTO tmp_ret_ley73_hist ",  
                                   "\n SELECT *                       ",
                                   "\n FROM   tmp_ret_ley73_actual    ", 
                                   "\n WHERE  1 = 1                   ",
                                   v_query_where

                     -- consulta INSERT
                     DISPLAY "Consulta Insert de los actuales:\n", v_query

                     PREPARE sid_query_act FROM v_query
                     EXECUTE sid_query_act
                    
                     SELECT COUNT(*)
                     INTO   v_regs_insertados
                     FROM   tmp_ret_ley73_hist;

                     DISPLAY "Insertados en la tabla temporal de detalle >", v_regs_insertados

                     LET v_query = "\n SELECT a.grupo, '  ' AS gpo_desc, a.estado_solicitud, b.descripcion, ",
                                   "\n        COUNT(*), SUM(a.importe_pago)                                 ", 
                                   "\n FROM   tmp_ret_ley73_hist a                                         ", 
                                   "\n        LEFT OUTER JOIN ret_cat_estado_sol_dev_ssv b                  ",
                                   "\n                     ON a.estado_solicitud = b.estado                 ", 
                                   "\n GROUP BY 1,2,3,4 "
                     -- consulta
                     DISPLAY "Consulta Cifras:\n", v_query
                     PREPARE prp_cifras FROM v_query
                     DECLARE cur_cifras CURSOR FOR prp_cifras
                     --DISPLAY v_s_qryTxt
                     FOREACH cur_cifras INTO v_reg_cifras_paso.*
                        LET v_reg_cifras[v_indice].grupo        = v_reg_cifras_paso.grupo USING "&&&&"
                        LET v_reg_cifras[v_indice].gpo_desc     = v_reg_cifras_paso.gpo_desc
                        LET v_reg_cifras[v_indice].estado_sol   = v_reg_cifras_paso.estado_sol USING "&&&&"
                        LET v_reg_cifras[v_indice].estado_desc  = v_reg_cifras_paso.estado_desc
                        LET v_reg_cifras[v_indice].num_regs     = v_reg_cifras_paso.num_regs USING "###,###,###"
                        LET v_reg_cifras[v_indice].sum_imp_pago = v_reg_cifras_paso.sum_imp_pago USING "###,###,###,###,##&.&&"
                        LET v_tot_registros = v_tot_registros + v_reg_cifras_paso.num_regs
                        LET v_suma_total    = v_suma_total + v_reg_cifras_paso.sum_imp_pago
                        LET v_indice = v_indice + 1
                     END FOREACH
                     LET v_reg_cifras[v_indice].grupo        = " "
                     LET v_reg_cifras[v_indice].gpo_desc     = " "
                     LET v_reg_cifras[v_indice].estado_sol   = " "
                     LET v_reg_cifras[v_indice].estado_desc  = " "
                     LET v_reg_cifras[v_indice].num_regs     = " "
                     LET v_reg_cifras[v_indice].sum_imp_pago = " "
                     LET v_indice = v_indice + 1
                     LET v_reg_cifras[v_indice].grupo        = " "
                     LET v_reg_cifras[v_indice].gpo_desc     = "Total"
                     LET v_reg_cifras[v_indice].estado_sol   = " "
                     LET v_reg_cifras[v_indice].num_regs     = v_tot_registros USING "###,###,###"
                     LET v_reg_cifras[v_indice].sum_imp_pago = v_suma_total USING "###,###,###,###,##&.&&"
                     DISPLAY "El resultado "
                     IF v_indice > 0 THEN 
                        CALL f_forma.setElementHidden("gpo_cifras",0)
                        DISPLAY ARRAY v_reg_cifras TO tbl_cifras.*
                        ON ACTION Exporta
                           LET v_query = "SELECT * FROM tmp_ret_ley73_hist"
                           IF v_indice > 2 THEN 
                              CALL fn_exporta_archivo(v_query) RETURNING v_resultado;
                           ELSE 
                              CALL fn_mensaje("Atención", "No existe información para generar el archivo", "information")
                           END IF 
                        ON ACTION CANCEL 
                           CALL v_reg_cifras.clear()
                           LET v_nss                    = NULL
                           LET v_d_f_pago_ini           = NULL
                           LET v_d_f_pago_fin           = NULL
                           LET v_d_f_sol_ini            = NULL 
                           LET v_d_f_sol_fin            = NULL 
                           LET v_d_f_autoriza_ini       = NULL
                           LET v_d_f_autoriza_fin       = NULL
                           LET v_chk_gpo_todos          = 0
                           LET v_chk_gpo_1              = 0
                           LET v_chk_gpo_2              = 0
                           LET v_chk_gpo_3              = 0
                           LET v_chk_gpo_4              = 0
                           LET v_chk_ori_todos          = 0
                           LET v_chk_ori_infonavit      = 0
                           LET v_chk_ori_excep          = 0
                           LET v_chk_ori_afore          = 0
                           LET v_chk_ori_siaff          = 0
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
                           LET v_chk_estatus_todos      = 0
                           LET v_chk_estatus_8          = 0
                           LET v_chk_estatus_10         = 0
                           LET v_chk_estatus_15         = 0
                           LET v_chk_estatus_50         = 0   
                           LET v_chk_estatus_60         = 0
                           LET v_chk_estatus_69         = 0
                           LET v_chk_estatus_70         = 0
                           LET v_chk_estatus_71         = 0
                           LET v_chk_estatus_72         = 0
                           LET v_chk_estatus_73         = 0
                           LET v_chk_estatus_90         = 0
                           LET v_chk_estatus_100        = 0
                           LET v_chk_estatus_200        = 0
                           LET v_chk_estatus_209        = 0
                           LET v_chk_estatus_210        = 0
                           LET v_chk_estatus_214        = 0
                           LET v_chk_estatus_700        = 0
                           LET v_chk_estatus_710        = 0
                           LET v_chk_estatus_720        = 0
                           LET v_chk_estatus_790        = 0
                           CALL f_forma.setElementHidden("gpo_cifras",1)
                           LET v_query_where = " "
                           LET v_indice = 1
                           LET v_tot_registros = 0
                           LET v_suma_total = 0
                           EXIT DISPLAY 
                        END DISPLAY 
                     ELSE 
                        CALL fn_mensaje("Atención", "La consulta no arrojo resultados", "information")
                     END IF 
                  END IF 
               END IF 
            END IF 
         END IF           
        
      ON ACTION CANCEL
         EXIT INPUT
   
   END INPUT
   
   CLOSE WINDOW w_consulta_pagos

END FUNCTION

{ ======================================================================
Clave: 
Nombre: rpt_detalle_saldos_insuf
Fecha creacion: Junio 20, 2012
Autor: Ivan Vega
Narrativa del proceso que realiza:
Genera el reporte de los saldos insuficientes 

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
REPORT rpt_detalle_saldos_insuf(p_indice, v_r_despliegue, p_usuario_cod, p_agrupador_folio_fecha_Afore)
DEFINE    v_r_despliegue          RECORD -- registro de consulta
           v_folio                  LIKE ret_transferencia.folio,
           v_f_carga                LIKE ret_cza_transferencia.f_carga,
           v_nss                    LIKE afi_derechohabiente.nss,
           v_nombre_trabajador      STRING, 
           v_aivs92_sol             DECIMAL(24,6),
           v_aivs97_sol             DECIMAL(24,6),
           v_aivs92_sol_pesos       DECIMAL(22,2),
           v_aivs97_sol_pesos       DECIMAL(22,2),
           v_aivs92_sdo             DECIMAL(24,6),
           v_aivs97_sdo             DECIMAL(24,6),
           v_aivsvol_sdo            DECIMAL(24,6),
           v_aivssum97vol_sdo       DECIMAL(24,6),
           v_aivs92_sdo_pesos       DECIMAL(22,2),
           v_aivs97_sdo_pesos       DECIMAL(22,2),
           v_aivsvol_sdo_pesos      DECIMAL(22,2),
           v_aivssum97vol_sdo_pesos DECIMAL(22,2),
           v_aivs92_dif             DECIMAL(24,6),
           v_aivs97_dif             DECIMAL(24,6),
           v_aivs92_dif_pesos       DECIMAL(22,2),        
           v_aivs97_dif_pesos       DECIMAL(22,2)
          END RECORD,
          p_indice                DECIMAL(9,0),
          p_usuario_cod           LIKE seg_usuario.usuario_cod, -- usuario en linea
          v_fecha                 STRING, -- fecha de emision del reporte
          v_nombre_usuario        VARCHAR(100),
          -- variables para acumular TOTAL PARCIAL
          v_total_afore_aivs92_sol             DECIMAL(24,6),
          v_total_afore_aivs97_sol             DECIMAL(24,6),
          v_total_afore_aivs92_sol_pesos       DECIMAL(22,2),
          v_total_afore_aivs97_sol_pesos       DECIMAL(22,2),
          v_total_afore_aivs92_sdo             DECIMAL(24,6),
          v_total_afore_aivs97_sdo             DECIMAL(24,6),
          v_total_afore_aivsvol_sdo            DECIMAL(24,6),
          v_total_afore_aivssum97vol_sdo       DECIMAL(24,6),
          v_total_afore_aivs92_sdo_pesos       DECIMAL(22,2),
          v_total_afore_aivs97_sdo_pesos       DECIMAL(22,2),
          v_total_afore_aivsvol_sdo_pesos      DECIMAL(22,2),
          v_total_afore_aivssum97vol_sdo_pesos DECIMAL(22,2),
          v_total_afore_aivs92_dif             DECIMAL(24,6),
          v_total_afore_aivs97_dif             DECIMAL(24,6),
          v_total_afore_aivs92_dif_pesos       DECIMAL(22,2),
          v_total_afore_aivs97_dif_pesos       DECIMAL(22,2),
          p_total_afore_regs                   DECIMAL(9,0),
          -- variables para acumular GRAN TOTAL
          v_total_aivs92_sol             DECIMAL(24,6),
          v_total_aivs97_sol             DECIMAL(24,6),
          v_total_aivs92_sol_pesos       DECIMAL(22,2),
          v_total_aivs97_sol_pesos       DECIMAL(22,2),
          v_total_aivs92_sdo             DECIMAL(24,6),
          v_total_aivs97_sdo             DECIMAL(24,6),
          v_total_aivsvol_sdo            DECIMAL(24,6),
          v_total_aivssum97vol_sdo       DECIMAL(24,6),
          v_total_aivs92_sdo_pesos       DECIMAL(22,2),
          v_total_aivs97_sdo_pesos       DECIMAL(22,2),
          v_total_aivsvol_sdo_pesos      DECIMAL(22,2),
          v_total_aivssum97vol_sdo_pesos DECIMAL(22,2),
          v_total_aivs92_dif             DECIMAL(24,6),
          v_total_aivs97_dif             DECIMAL(24,6),
          v_total_aivs92_dif_pesos       DECIMAL(22,2),
          v_total_aivs97_dif_pesos       DECIMAL(22,2),
          p_total_regs                   DECIMAL(9,0),
          v_fecha_carga                  STRING,
          p_agrupador_folio_fecha_Afore  STRING
          
FORMAT

   FIRST PAGE HEADER
      
      -- variables para acumular gran total
      LET v_total_afore_aivs92_sol             = 0
      LET v_total_afore_aivs97_sol             = 0
      LET v_total_afore_aivs92_sol_pesos       = 0
      LET v_total_afore_aivs97_sol_pesos       = 0
      LET v_total_afore_aivs92_sdo             = 0
      LET v_total_afore_aivs97_sdo             = 0
      LET v_total_afore_aivsvol_sdo            = 0
      LET v_total_afore_aivssum97vol_sdo       = 0
      LET v_total_afore_aivs92_sdo_pesos       = 0
      LET v_total_afore_aivs97_sdo_pesos       = 0
      LET v_total_afore_aivsvol_sdo_pesos      = 0
      LET v_total_afore_aivssum97vol_sdo_pesos = 0
      LET v_total_afore_aivs92_dif             = 0
      LET v_total_afore_aivs97_dif             = 0
      LET v_total_afore_aivs92_dif_pesos       = 0
      LET v_total_afore_aivs97_dif_pesos       = 0
      LET p_total_afore_regs                   = 0
      
      -- variables para acumular por afore, fecha y folio
      LET v_total_aivs92_sol              = 0
      LET v_total_aivs97_sol              = 0
      LET v_total_aivs92_sol_pesos        = 0
      LET v_total_aivs97_sol_pesos        = 0
      LET v_total_aivs92_sdo              = 0
      LET v_total_aivs97_sdo              = 0
      LET v_total_aivsvol_sdo             = 0
      LET v_total_aivssum97vol_sdo        = 0
      LET v_total_aivs92_sdo_pesos        = 0
      LET v_total_aivs97_sdo_pesos        = 0
      LET v_total_aivsvol_sdo_pesos       = 0
      LET v_total_aivssum97vol_sdo_pesos  = 0
      LET v_total_aivs92_dif              = 0
      LET v_total_aivs97_dif              = 0
      LET v_total_aivs92_dif_pesos        = 0
      LET v_total_aivs97_dif_pesos        = 0
      LET p_total_regs                    = 0

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
      LET v_total_afore_aivs92_sol             = 0
      LET v_total_afore_aivs97_sol             = 0
      LET v_total_afore_aivs92_sol_pesos       = 0
      LET v_total_afore_aivs97_sol_pesos       = 0
      LET v_total_afore_aivs92_sdo             = 0
      LET v_total_afore_aivs97_sdo             = 0
      LET v_total_afore_aivsvol_sdo            = 0
      LET v_total_afore_aivssum97vol_sdo       = 0
      LET v_total_afore_aivs92_sdo_pesos       = 0
      LET v_total_afore_aivs97_sdo_pesos       = 0
      LET v_total_afore_aivsvol_sdo_pesos      = 0
      LET v_total_afore_aivssum97vol_sdo_pesos = 0
      LET v_total_afore_aivs92_dif             = 0
      LET v_total_afore_aivs97_dif             = 0
      LET v_total_afore_aivs92_dif_pesos       = 0
      LET v_total_afore_aivs97_dif_pesos       = 0
      LET p_total_afore_regs                   = 0
   
      PRINTX v_r_despliegue.v_folio,
             v_fecha_carga
             

   ON EVERY ROW
      PRINTX v_r_despliegue.*
      DISPLAY v_r_despliegue.*
      
      -- se acumulan los montos para total por afore, folio y fecha de carga
      LET v_total_afore_aivs92_sol             = v_total_afore_aivs92_sol             + v_r_despliegue.v_aivs92_sol      
      LET v_total_afore_aivs97_sol             = v_total_afore_aivs97_sol             + v_r_despliegue.v_aivs97_sol      
      LET v_total_afore_aivs92_sol_pesos       = v_total_afore_aivs92_sol_pesos       + v_r_despliegue.v_aivs92_sol_pesos
      LET v_total_afore_aivs97_sol_pesos       = v_total_afore_aivs97_sol_pesos       + v_r_despliegue.v_aivs97_sol_pesos
      LET v_total_afore_aivs92_sdo             = v_total_afore_aivs92_sdo             + v_r_despliegue.v_aivs92_sdo      
      LET v_total_afore_aivs97_sdo             = v_total_afore_aivs97_sdo             + v_r_despliegue.v_aivs97_sdo      
      LET v_total_afore_aivsvol_sdo            = v_total_afore_aivsvol_sdo            + v_r_despliegue.v_aivsvol_sdo      
      LET v_total_afore_aivssum97vol_sdo       = v_total_afore_aivssum97vol_sdo       + v_r_despliegue.v_aivssum97vol_sdo      
      LET v_total_afore_aivs92_sdo_pesos       = v_total_afore_aivs92_sdo_pesos       + v_r_despliegue.v_aivs92_sdo_pesos
      LET v_total_afore_aivs97_sdo_pesos       = v_total_afore_aivs97_sdo_pesos       + v_r_despliegue.v_aivs97_sdo_pesos
      LET v_total_afore_aivsvol_sdo_pesos      = v_total_afore_aivsvol_sdo_pesos      + v_r_despliegue.v_aivsvol_sdo_pesos
      LET v_total_afore_aivssum97vol_sdo_pesos = v_total_afore_aivssum97vol_sdo_pesos + v_r_despliegue.v_aivssum97vol_sdo_pesos
      LET v_total_afore_aivs92_dif             = v_total_afore_aivs92_dif             + v_r_despliegue.v_aivs92_dif      
      LET v_total_afore_aivs97_dif             = v_total_afore_aivs97_dif             + v_r_despliegue.v_aivs97_dif      
      LET v_total_afore_aivs92_dif_pesos       = v_total_afore_aivs92_dif_pesos       + v_r_despliegue.v_aivs92_dif_pesos
      LET v_total_afore_aivs97_dif_pesos       = v_total_afore_aivs97_dif_pesos       + v_r_despliegue.v_aivs97_dif_pesos
      LET p_total_afore_regs                   = p_total_afore_regs                   + 1
      
      
      -- se acumulan los montos para gran total
      LET v_total_aivs92_sol             = v_total_aivs92_sol             + v_r_despliegue.v_aivs92_sol      
      LET v_total_aivs97_sol             = v_total_aivs97_sol             + v_r_despliegue.v_aivs97_sol      
      LET v_total_aivs92_sol_pesos       = v_total_aivs92_sol_pesos       + v_r_despliegue.v_aivs92_sol_pesos
      LET v_total_aivs97_sol_pesos       = v_total_aivs97_sol_pesos       + v_r_despliegue.v_aivs97_sol_pesos
      LET v_total_aivs92_sdo             = v_total_aivs92_sdo             + v_r_despliegue.v_aivs92_sdo      
      LET v_total_aivs97_sdo             = v_total_aivs97_sdo             + v_r_despliegue.v_aivs97_sdo     
      LET v_total_aivsvol_sdo            = v_total_aivsvol_sdo            + v_r_despliegue.v_aivsvol_sdo     
      LET v_total_aivssum97vol_sdo       = v_total_aivssum97vol_sdo       + v_r_despliegue.v_aivssum97vol_sdo      
      LET v_total_aivs92_sdo_pesos       = v_total_aivs92_sdo_pesos       + v_r_despliegue.v_aivs92_sdo_pesos
      LET v_total_aivs97_sdo_pesos       = v_total_aivs97_sdo_pesos       + v_r_despliegue.v_aivs97_sdo_pesos
      LET v_total_aivsvol_sdo_pesos      = v_total_aivsvol_sdo_pesos      + v_r_despliegue.v_aivsvol_sdo_pesos
      LET v_total_aivssum97vol_sdo_pesos = v_total_aivssum97vol_sdo_pesos + v_r_despliegue.v_aivssum97vol_sdo_pesos
      LET v_total_aivs92_dif             = v_total_aivs92_dif             + v_r_despliegue.v_aivs92_dif      
      LET v_total_aivs97_dif             = v_total_aivs97_dif             + v_r_despliegue.v_aivs97_dif      
      LET v_total_aivs92_dif_pesos       = v_total_aivs92_dif_pesos       + v_r_despliegue.v_aivs92_dif_pesos
      LET v_total_aivs97_dif_pesos       = v_total_aivs97_dif_pesos       + v_r_despliegue.v_aivs97_dif_pesos
      LET p_total_regs                   = p_total_regs                   + 1

   --AFTER GROUP OF v_r_despliegue.v_afore
   AFTER GROUP OF p_agrupador_folio_fecha_Afore
      PRINTX v_total_afore_aivs92_sol             ,
             v_total_afore_aivs97_sol             ,
             v_total_afore_aivs92_sol_pesos       ,
             v_total_afore_aivs97_sol_pesos       ,
             v_total_afore_aivs92_sdo             ,
             v_total_afore_aivs97_sdo             ,
             v_total_afore_aivsvol_sdo            ,
             v_total_afore_aivssum97vol_sdo       ,
             v_total_afore_aivs92_sdo_pesos       ,
             v_total_afore_aivs97_sdo_pesos       ,
             v_total_afore_aivsvol_sdo_pesos      ,
             v_total_afore_aivssum97vol_sdo_pesos ,
             v_total_afore_aivs92_dif             ,
             v_total_afore_aivs97_dif             ,
             v_total_afore_aivs92_dif_pesos       ,
             v_total_afore_aivs97_dif_pesos       ,
             p_total_afore_regs            
                                                          
   
   ON LAST ROW 
      PRINTX p_total_regs                   ,
             v_total_aivs92_sol             ,
             v_total_aivs97_sol             ,
             v_total_aivs92_sol_pesos       ,
             v_total_aivs97_sol_pesos       ,
             v_total_aivs92_sdo             ,
             v_total_aivs97_sdo             ,
             v_total_aivsvol_sdo            ,
             v_total_aivssum97vol_sdo       ,
             v_total_aivs92_sdo_pesos       ,
             v_total_aivs97_sdo_pesos       ,
             v_total_aivsvol_sdo_pesos      ,
             v_total_aivssum97vol_sdo_pesos ,
             v_total_aivs92_dif             ,
             v_total_aivs97_dif             ,
             v_total_aivs92_dif_pesos       ,
             v_total_aivs97_dif_pesos 

END REPORT

FUNCTION f_limpia_arreglo(p_arr_detalle)
   DEFINE 
      p_arr_detalle         RECORD
         ed_tot_reg               INTEGER,
         ed_pesos_debita          DECIMAL(14,2),
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
      END RECORD

   LET p_arr_detalle.ed_tot_reg = 0;
   LET p_arr_detalle.ed_pesos_debita = 0;
   LET p_arr_detalle.ed_aivs_debita = 0;
   LET p_arr_detalle.ed_total_pagos = 0;
   LET p_arr_detalle.ed_sin_dif = 0;
   LET p_arr_detalle.ed_fch_carga = NULL;
   LET p_arr_detalle.ed_aivs_viv_97 = 0;
   LET p_arr_detalle.ed_aivs_viv_92 = 0;
   LET p_arr_detalle.ed_pesos_viv_97 = 0;
   LET p_arr_detalle.ed_pesos_viv_92 = 0;
   LET p_arr_detalle.ed_nss_no_loc = 0;
   LET p_arr_detalle.ed_pesos_nss_no_loc = 0;
   LET p_arr_detalle.ed_aivs_nss_no_loc = 0;


   RETURN p_arr_detalle.*
 
END FUNCTION

--Función que exporta datos a un archivo
FUNCTION fn_exporta_archivo(p_query)


DEFINE  v_registro_archivo  DYNAMIC ARRAY OF RECORD    -- registro del archivo
           nss                       CHAR(11),
           estado_solicitud          SMALLINT,
           descripcion               CHAR(25),
           origen                    CHAR(25),
           via_pago                  CHAR(11),
           grupo                     CHAR(04),
           nombre                    CHAR(50),
           ap_paterno                CHAR(40),
           ap_materno                CHAR(40),
           beneficiario              CHAR(15),
           nombre_beneficiario       CHAR(40),
           ap_paterno_beneficiario   CHAR(40),
           ap_materno_beneficiario   CHAR(40),
           curp                      CHAR(18),
           rfc                       CHAR(13),
           entidad                   SMALLINT,
           fecha_tramite             CHAR(23),
           fecha_autorizacion        CHAR(23),
           documento                 CHAR(10),
           ejercicio                 CHAR(04),
           documento_pago            CHAR(10),
           fecha_pago                CHAR(23),
           importe_pago              DECIMAL(22,2),
           referencia_pago           CHAR(25),
           caso_adai                 CHAR(10),
           num_laudo                 CHAR(10),
           num_junta                 CHAR(10),
           importe_pago_anterior     DECIMAL(22,2),
           fch_pago_anterior         CHAR(23),
           clave_banco               CHAR(05),
           cuenta                    CHAR(18),
           importe_transf            DECIMAL(22,2),
           fch_transf                CHAR(23),
           ssv_dif                   DECIMAL(22,2),
           fch_marca_tj              CHAR(23),
           error_fico                CHAR(10),
           cve_afore                 CHAR(03),
           pesos_viv97               DECIMAL(22,2),
           aivs_viv97                DECIMAL(22,2),
           pesos_viv92               DECIMAL(22,2),
           aivs_viv92                DECIMAL(22,2),
           usuario                   CHAR(20),
           cabecera                  CHAR(100),
           acreedor                  CHAR(10),
           via_pago_ac               CHAR(10),
           bloqueo_pago              CHAR(10),
           anulado                   CHAR(10),
           ejercicio_anulacion       CHAR(04)
      END RECORD 
DEFINE p_r_grupo   DYNAMIC ARRAY OF RECORD -- registro de despliegue del agrupador
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
      END RECORD
DEFINE p_query             STRING 
DEFINE p_detalle_exp      RECORD
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
         estado_solicitud SMALLINT      END RECORD
   DEFINE p_exporta_opcion       CHAR(15)
   DEFINE
      v_c_ruta_env            LIKE seg_modulo.ruta_envio,
      v_extension_txt         STRING,
      v_nom_archivo           STRING,
      v_archivo_txt           STRING,
      v_v_ruta_nomarch        STRING,
      v_v_ruta_nomarch2       STRING,
      v_mensaje_archivo       STRING,
      v_s_detalle             STRING,
      v_ch_arch_ret_generico  BASE.CHANNEL,  -- manejador de apuntador hacia archivo
      v_cuenta                INTEGER,
      v_solicitud             DECIMAL(9,0),
      v_tipo                  CHAR(1),
      v_hora                  CHAR(8),
      v_regresa               SMALLINT,
      v_contador              INTEGER,
      v_query                 STRING,
      v_nss_paso              CHAR(11),
      v_suma_aivs_97          DECIMAL(18,6),
      v_i                     INTEGER,
      v_indice                INTEGER,
      v_cve_banco             INTEGER  

   LET v_regresa = FALSE
   LET v_i       = 0
   -- se obtiene la ruta de envio y ejecutable
   SELECT ruta_envio
   INTO   v_c_ruta_env 
   FROM   seg_modulo
   WHERE  modulo_cod = "ret"

--   DISPLAY "el folio a procesar es >", p_folio, "<"
   
   -- las extensiones del archivo son csv para el detalle
   LET v_extension_txt = ".csv"
   LET v_hora = CURRENT HOUR TO SECOND

   -- Se genera el nombre del archivo de Rechazos
   LET v_nom_archivo = TODAY USING "yyyymmdd" 
   LET v_archivo_txt = v_nom_archivo CLIPPED, ".xhpdssv"

   -- El archivo con ruta destino que contiene el detalle 
   LET v_v_ruta_nomarch = v_c_ruta_env CLIPPED , "/", v_archivo_txt

   -- Se muestra mensaje
   LET v_mensaje_archivo = "Se generará el archivo:\n\n\t", v_v_ruta_nomarch
   
   CALL fn_mensaje("Atención", v_mensaje_archivo, "information")
   -- nombre de archivo generado

   -- Se crea el manejador de archivo
   LET v_ch_arch_ret_generico = base.Channel.create()
   CALL v_ch_arch_ret_generico.setDelimiter(NULL)

   -- Se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_ret_generico.openFile(v_v_ruta_nomarch, "w" )
 
   -- se obtienen los registros del detalle
   PREPARE sid_query_exporta FROM p_query
   DECLARE cur_query_exporta CURSOR FOR sid_query_exporta
   
   LET v_indice = 1
   LET v_nss_paso = NULL
   LET v_s_detalle = "NSS del Trabajador|Estado de Solicitud|Descripción del estado de solicitud SSV|",
                     "Origen de la solicitud|vía de pago|grupo|Nombre AFORE|1er apellido AFORE|",
                     "2do apellido AFORE|Beneficiario (Titular o Beneficiario)|Nombre beneficiario|",
                     "Apellido paterno beneficiario|Apellido materno beneficiario|CURP|RFC|",
                     "Entidad federativa|Fecha de solicitud|Fecha de autorización|",
                     "Número de documento cuenta por pagar fico|Ejercicio fiscal cuenta por pagar fico|",
                     "No documento pago fico|Fecha pago fico|Importe de pago fico|Referencia de pago fico|",
                     "Número de caso ADAI|Número de laudo|Número de junta especial|",
                     "Importe de pago realizado anterioridad|Fecha de pago realizado anterioridad|",
                     "Clave de Banco|Cuenta Bancaria|Importe transferido SSV  (tipo B)|",
                     "Fecha transferencia SSV (tipo B)|SSV DIF Carga inic Legacy|Fecha de marca TJ SSV|",
                     "Error FICO (Creación deudor)|SSV Clave AFORE|SSV 97 Importe en $|",
                     "SSV 97  Importe en AIVS|SSV 92 Importe en $|SSV 92  Importe en AIVS|",
                     "Nombre de Usuario|texto cabecera|Acreedor|vía de pago|Bloqueo de pago|",
                     "Anulado con|Ejercicio doc. Anulación|"
   CALL v_ch_arch_ret_generico.write(v_s_detalle)
   LET v_indice = v_indice + 1
   -- se transfieren los datos al arreglo de despliegue agrupador
   FOREACH cur_query_exporta INTO v_registro_archivo[v_indice].*
      IF v_registro_archivo[v_indice].clave_banco IS NOT NULL THEN 
         LET v_cve_banco = v_registro_archivo[v_indice].clave_banco
         LET v_registro_archivo[v_indice].clave_banco = v_cve_banco USING "&&&&&"
      END IF 
      IF v_registro_archivo[v_indice].fch_marca_tj IS NOT NULL AND LENGTH(v_registro_archivo[v_indice].fch_marca_tj CLIPPED) = 10 THEN 
         LET v_registro_archivo[v_indice].fch_marca_tj = v_registro_archivo[v_indice].fch_marca_tj[4,5]||"/"||v_registro_archivo[v_indice].fch_marca_tj[1,2]||"/"||v_registro_archivo[v_indice].fch_marca_tj[7,10]   
      END IF 
      IF v_registro_archivo[v_indice].fch_pago_anterior IS NOT NULL AND LENGTH(v_registro_archivo[v_indice].fch_pago_anterior CLIPPED) = 10 THEN 
         LET v_registro_archivo[v_indice].fch_pago_anterior = v_registro_archivo[v_indice].fch_pago_anterior[4,5]||"/"||v_registro_archivo[v_indice].fch_pago_anterior[1,2]||"/"||v_registro_archivo[v_indice].fch_pago_anterior[7,10]   
      END IF 
      IF v_registro_archivo[v_indice].fch_transf IS NOT NULL AND LENGTH(v_registro_archivo[v_indice].fch_transf CLIPPED) = 10 THEN 
         LET v_registro_archivo[v_indice].fch_transf = v_registro_archivo[v_indice].fch_transf[4,5]||"/"||v_registro_archivo[v_indice].fch_transf[1,2]||"/"||v_registro_archivo[v_indice].fch_transf[7,10]   
      END IF 
      IF v_registro_archivo[v_indice].fecha_autorizacion IS NOT NULL AND LENGTH(v_registro_archivo[v_indice].fecha_autorizacion CLIPPED) = 10  THEN
         LET v_registro_archivo[v_indice].fecha_autorizacion = v_registro_archivo[v_indice].fecha_autorizacion[4,5]||"/"||v_registro_archivo[v_indice].fecha_autorizacion[1,2]||"/"||v_registro_archivo[v_indice].fecha_autorizacion[7,10]   
      END IF 
      IF v_registro_archivo[v_indice].fecha_pago IS NOT NULL AND LENGTH(v_registro_archivo[v_indice].fecha_pago CLIPPED) = 10 THEN 
         LET v_registro_archivo[v_indice].fecha_pago = v_registro_archivo[v_indice].fecha_pago[4,5]||"/"||v_registro_archivo[v_indice].fecha_pago[1,2]||"/"||v_registro_archivo[v_indice].fecha_pago[7,10]   
      END IF 
      IF v_registro_archivo[v_indice].fecha_tramite IS NOT NULL AND LENGTH(v_registro_archivo[v_indice].fecha_tramite CLIPPED) = 10 THEN 
         LET v_registro_archivo[v_indice].fecha_tramite = v_registro_archivo[v_indice].fecha_tramite[4,5]||"/"||v_registro_archivo[v_indice].fecha_tramite[1,2]||"/"||v_registro_archivo[v_indice].fecha_tramite[7,10]   
      END IF
      LET v_s_detalle =  v_registro_archivo[v_indice].nss                     , "|",
           v_registro_archivo[v_indice].estado_solicitud                      , "|",
           v_registro_archivo[v_indice].descripcion                           , "|",
           v_registro_archivo[v_indice].origen                                , "|",
           v_registro_archivo[v_indice].via_pago                              , "|",
           v_registro_archivo[v_indice].grupo                                 , "|",
           v_registro_archivo[v_indice].nombre                                , "|",
           v_registro_archivo[v_indice].ap_paterno                            , "|",
           v_registro_archivo[v_indice].ap_materno                            , "|",
           v_registro_archivo[v_indice].beneficiario                          , "|",
           v_registro_archivo[v_indice].nombre_beneficiario                   , "|",
           v_registro_archivo[v_indice].ap_paterno_beneficiario               , "|",
           v_registro_archivo[v_indice].ap_materno_beneficiario               , "|",
           v_registro_archivo[v_indice].curp                                  , "|",
           v_registro_archivo[v_indice].rfc                                   , "|",
           v_registro_archivo[v_indice].entidad                               , "|",
           v_registro_archivo[v_indice].fecha_tramite                         , "|",
           v_registro_archivo[v_indice].fecha_autorizacion                    , "|",
           v_registro_archivo[v_indice].documento                             , "|",
           v_registro_archivo[v_indice].ejercicio                             , "|",
           v_registro_archivo[v_indice].documento_pago                        , "|",
           v_registro_archivo[v_indice].fecha_pago                            , "|",
           v_registro_archivo[v_indice].importe_pago                          , "|",
           v_registro_archivo[v_indice].referencia_pago                       , "|",
           v_registro_archivo[v_indice].caso_adai                             , "|",
           v_registro_archivo[v_indice].num_laudo                             , "|",
           v_registro_archivo[v_indice].num_junta                             , "|",
           v_registro_archivo[v_indice].importe_pago_anterior                 , "|",
           v_registro_archivo[v_indice].fch_pago_anterior                     , "|",
           v_registro_archivo[v_indice].clave_banco                           , "|",
           v_registro_archivo[v_indice].cuenta                                , "|",
           v_registro_archivo[v_indice].importe_transf                        , "|",
           v_registro_archivo[v_indice].fch_transf                            , "|",
           v_registro_archivo[v_indice].ssv_dif                               , "|",
           v_registro_archivo[v_indice].fch_marca_tj                          , "|",
           v_registro_archivo[v_indice].error_fico                            , "|",
           v_registro_archivo[v_indice].cve_afore                             , "|",
           v_registro_archivo[v_indice].pesos_viv97                           , "|",
           v_registro_archivo[v_indice].aivs_viv97                            , "|",
           v_registro_archivo[v_indice].pesos_viv92                           , "|",
           v_registro_archivo[v_indice].aivs_viv92                            , "|",
           v_registro_archivo[v_indice].usuario                               , "|",
           v_registro_archivo[v_indice].cabecera                              , "|",
           v_registro_archivo[v_indice].acreedor                              , "|",
           v_registro_archivo[v_indice].via_pago_ac                           , "|",
           v_registro_archivo[v_indice].bloqueo_pago                          , "|",
           v_registro_archivo[v_indice].anulado                               , "|",
           v_registro_archivo[v_indice].ejercicio_anulacion                   , "|"
           
           CALL v_ch_arch_ret_generico.write(v_s_detalle)
         LET v_indice = v_indice + 1
   END FOREACH

   -- Se cierra el archivo de Rechazados
   CALL v_ch_arch_ret_generico.close()

   LET v_mensaje_archivo = "El archivo fue generado exitosamente:\n\n\t", v_v_ruta_nomarch

   CALL fn_mensaje("Atención", v_mensaje_archivo, "information")

   RETURN v_regresa
   
END FUNCTION

FUNCTION fn_llena_tabla_nss (p_nss)
DEFINE  p_nss       STRING
DEFINE  v_respuesta SMALLINT 
DEFINE  i           INTEGER 
DEFINE  v_nss       CHAR(11)
DEFINE  v_num_reg   INTEGER 

   DROP TABLE IF EXISTS tmp_nss;
   CREATE TEMP TABLE tmp_nss (nss CHAR(11));
   LET v_nss = ""
   LET v_num_reg = 0

   FOR i = 1 TO p_nss.getLength()
      IF i = 1 THEN
         LET v_nss = v_nss CLIPPED, p_nss.subString(i,i)
      ELSE
         IF p_nss.subString(i,i) = "\n" THEN 
            DISPLAY "EL NSS A INSERTAR >",v_nss, "<"
            INSERT INTO tmp_nss VALUES (v_nss);
            LET v_nss = ""
         ELSE 
            LET v_nss = v_nss CLIPPED, p_nss.subString(i,i)
         END IF 
      END IF 
   END FOR 

   SELECT COUNT(*)
   INTO   v_num_reg
   FROM   tmp_nss;

   DISPLAY "Se insertaron en la temporal de NSS's : ", v_num_reg
      

RETURN v_respuesta
END FUNCTION 