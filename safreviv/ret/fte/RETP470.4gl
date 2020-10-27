--===============================================================
-- Version: 1.0.0
--===============================================================

#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETC470                                                                #
#Objetivo     => Consulta y Extracción de Pagos de la Devolición del SSV                #
#Fecha inicio => Abril 23, 2018                                                         # 
#########################################################################################

DATABASE safre_viv

GLOBALS "RETG01.4gl"  -- se agrega archivo globales y se sustituyen las variables necesarias
PRIVATE DEFINE p_pid                      LIKE glo_pid.pid
PRIVATE DEFINE p_id_peticion              DECIMAL(9,0)
PRIVATE DEFINE p_proceso_cod              LIKE cat_proceso.proceso_cod
PRIVATE DEFINE p_opera_cod                LIKE cat_operacion.opera_cod
PRIVATE DEFINE v_proceso_desc             LIKE cat_proceso.proceso_desc
PRIVATE DEFINE v_opera_desc               LIKE cat_operacion.opera_desc
PRIVATE DEFINE v_folio                    LIKE glo_folio.folio

GLOBALS
	DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod -- clave del usuario firmado
          ,v_ventana        ui.WINDOW
          ,f_forma          ui.Form   
          
END GLOBALS

MAIN
DEFINE p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING -- titulo de la ventana

DEFINE v_query          STRING 

DEFINE r_resultado_opera    INTEGER

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1

    CALL ARG_VAL(1) RETURNING p_usuario_cod
    CALL ARG_VAL(2) RETURNING p_pid
    CALL ARG_VAL(3) RETURNING p_proceso_cod
    CALL ARG_VAL(4) RETURNING p_opera_cod
    CALL ARG_VAL(7) RETURNING p_id_peticion

     --Descripción del proceso
    SELECT proceso_desc
      INTO v_proceso_desc
    FROM   cat_proceso
    WHERE  proceso_cod = p_proceso_cod

    --Descripcion de la operacion
    SELECT opera_desc
      INTO v_opera_desc
    FROM   cat_operacion
    WHERE  proceso_cod = p_proceso_cod
      AND  opera_cod   = p_opera_cod   

    --Encabezado para el archivo de monitoreo
    DISPLAY "*******************************************************************"
    DISPLAY " PROCESO            : ",v_proceso_desc
    DISPLAY " OPERACIÓN          : ",v_opera_desc
    DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
    DISPLAY " HORA               : ",TIME(CURRENT)
    DISPLAY " USUARIO            : ",p_usuario_cod
    DISPLAY " PID                : ",p_pid
    DISPLAY " PROCESO COD        : ",p_proceso_cod
    DISPLAY " OPERA COD          : ",p_opera_cod
    DISPLAY " ID PETICION        : ",p_id_peticion
    DISPLAY "*******************************************************************"
    DISPLAY ""
    DISPLAY ""
    DISPLAY ""

    -- Se solicita el numero de folio asociado a la operacion. Parametros: proceso, operacion, usuario
    CALL fn_genera_folio(p_proceso_cod,p_opera_cod,p_usuario_cod)
    RETURNING v_folio

    --Se actualiza el folio del proceso
    UPDATE bat_ctr_proceso   
    SET    folio = v_folio
    WHERE  pid = p_pid
    UPDATE bat_ctr_operacion
    SET    folio = v_folio
    WHERE  pid = p_pid
      
   -- consulta de informacion recibida 
   CALL fn_consulta_detalle()

    --Finaliza la operacion
    CALL fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
    RETURNING r_resultado_opera

    IF(r_resultado_opera <> 0)THEN         
        # Actualiza a estado erróneo
        DISPLAY "Ocurrio un ERROR al intentar actualizar el estado de la operacion..."
        CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
        RETURNING r_resultado_opera
    END IF

    --Encabezado para el archivo de monitoreo
    DISPLAY ""
    DISPLAY ""
    DISPLAY ""
    DISPLAY "Terminó el Batch de Extracción del detalle del SSV                 "
    DISPLAY "*******************************************************************"
    DISPLAY " PROCESO            : ",v_proceso_desc
    DISPLAY " OPERACIÓN          : ",v_opera_desc
    DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
    DISPLAY " HORA               : ",TIME(CURRENT)
    DISPLAY "*******************************************************************"

   
END MAIN

{ ============================================================================
Clave: RETP470
Nombre: fn_consulta_detalle
Fecha creacion: Abril 23, 2018
Registro de modificaciones:
Descrip: Consulta y exportación del detalle de pagos de la devolución del SSV
==============================================================================
}
FUNCTION fn_consulta_detalle()
DEFINE 
      v_archivo             LIKE glo_ctr_archivo.nombre_archivo,
      v_archivo_consulta    LIKE glo_ctr_archivo.nombre_archivo,
      v_registros           INTEGER, 
      v_condicion           STRING, 
      v_fecha_ini           STRING,
      v_fecha_fin           STRING,

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
      v_curp                   STRING,
      v_rfc                    STRING,
      v_doc_pago               STRING,
      v_caso_adai              STRING,
      v_documento              STRING,
      v_e_fiscal               STRING,
      v_ref_pago               STRING,
      v_num_laudo              STRING,
      v_junta                  STRING,
      v_cve_banco              STRING,
      v_cuenta                 STRING,
      v_cve_afore              STRING,
      v_usuario                CHAR(20),
      v_acreedor               CHAR(10),
      v_anulado                CHAR(10),
      v_chk_gpo_todos          BOOLEAN,
      v_chk_gpo_1              BOOLEAN,
      v_chk_gpo_2              BOOLEAN,
      v_chk_gpo_3              BOOLEAN,
      v_chk_gpo_4              BOOLEAN,
      v_chk_ori_todos          BOOLEAN,
      v_chk_ori_infonavit      BOOLEAN,
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
      v_chk_estatus_8         BOOLEAN,
      v_chk_estatus_10         BOOLEAN,
      v_chk_estatus_15         BOOLEAN,
      v_chk_estatus_50         BOOLEAN,
      v_chk_estatus_60         BOOLEAN,
      v_chk_estatus_69         BOOLEAN,
      v_chk_estatus_70         BOOLEAN,
      v_chk_estatus_71         BOOLEAN,
      v_chk_estatus_72         BOOLEAN,
      v_chk_estatus_73         BOOLEAN,
      v_chk_estatus_90         BOOLEAN,
      v_chk_estatus_100         BOOLEAN,
      v_chk_estatus_200         BOOLEAN,
      v_chk_estatus_209         BOOLEAN,
      v_chk_estatus_210         BOOLEAN,
      v_chk_estatus_214         BOOLEAN,
      v_chk_estatus_700         BOOLEAN,
      v_chk_estatus_710         BOOLEAN,
      v_chk_estatus_720         BOOLEAN,
      v_chk_estatus_790         BOOLEAN,
      v_chk_entidad_todos      BOOLEAN,
      v_chk_entidad_01         BOOLEAN,
      v_chk_entidad_02         BOOLEAN,
      v_chk_entidad_03         BOOLEAN,
      v_chk_entidad_04         BOOLEAN,
      v_chk_entidad_05         BOOLEAN,
      v_chk_entidad_06         BOOLEAN,
      v_chk_entidad_07         BOOLEAN,
      v_chk_entidad_08         BOOLEAN,
      v_chk_entidad_09         BOOLEAN,
      v_chk_entidad_10         BOOLEAN,
      v_chk_entidad_11         BOOLEAN,
      v_chk_entidad_12         BOOLEAN,
      v_chk_entidad_13         BOOLEAN,
      v_chk_entidad_14         BOOLEAN,
      v_chk_entidad_15         BOOLEAN,
      v_chk_entidad_16         BOOLEAN,
      v_chk_entidad_17         BOOLEAN,
      v_chk_entidad_18         BOOLEAN,
      v_chk_entidad_19         BOOLEAN,
      v_chk_entidad_20         BOOLEAN,
      v_chk_entidad_21         BOOLEAN,
      v_chk_entidad_22         BOOLEAN,
      v_chk_entidad_23         BOOLEAN,
      v_chk_entidad_24         BOOLEAN,
      v_chk_entidad_25         BOOLEAN,
      v_chk_entidad_26         BOOLEAN,
      v_chk_entidad_27         BOOLEAN,
      v_chk_entidad_28         BOOLEAN,
      v_chk_entidad_29         BOOLEAN,
      v_chk_entidad_30         BOOLEAN,
      v_chk_entidad_31         BOOLEAN,
      v_chk_entidad_32         BOOLEAN,
      v_chk_benef_todos        BOOLEAN,
      v_chk_benef_beneficiario BOOLEAN,
      v_chk_benef_titular      BOOLEAN,
      v_chk_benef_blanco       BOOLEAN,

      v_arr_detalle  DYNAMIC ARRAY OF RECORD    -- registro del archivo
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
           ejercicio_anulacion       CHAR(04),
           fecha_carga               DATE 
      END RECORD, 
      
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
      v_reg_actual            RECORD 
            nss                       CHAR(11),
            estado_solicitud          SMALLINT,
            descripcion               CHAR(25),
            origen                    CHAR(25),
            via_pago                  CHAR(11),
            grupo                     CHAR(4),
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
            fecha_tramite             DATE,
            fecha_autorizacion        DATE,
            documento                 CHAR(10),
            ejercicio                 CHAR(4),
            documento_pago            CHAR(10),
            fecha_pago                DATE,
            importe_pago              DECIMAL(22,2),
            referencia_pago           CHAR(25),
            caso_adai                 CHAR(10),
            num_laudo                 CHAR(10),
            num_junta                 CHAR(10),
            importe_pago_anterior     DECIMAL(22,2),
            fch_pago_anterior         DATE,
            clave_banco               CHAR(4),
            cuenta                    CHAR(18),
            importe_transf            DECIMAL(22,2),
            fch_transf                DATE,
            ssv_dif                   DECIMAL(22,2),
            fch_marca_tj              DATE,
            error_fico                CHAR(10),
            cve_afore                 CHAR(3),
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
            ejercicio_anulacion       CHAR(4),
            fch_carga                 DATE

       END RECORD,    
       v_id_solicitud_generico       DECIMAL(9,0),
       v_consec_beneficiario         SMALLINT,
       v_tpo_beneficiario            SMALLINT,       
       v_query                       STRING, -- detalle
       v_query_where                 STRING,
       v_query_where_act             STRING,
       v_indice                      INTEGER, -- indice de arreglo
       v_precio_fondo                LIKE ret_cza_transferencia.precio_fondo,
       v_cve_banco_int               INTEGER 
       
   CONSTANT l_nom_tbl_estados = "tbl_estados" -- Tabla de estados
   CONSTANT l_nom_tbl_estados_cons = "tbl_estados_cons" -- Tabla de estados a consultar

   LET v_indice = 1
   LET v_regs_insertados = 0
   LET v_cve_banco_int = 0
   LET v_registros = 0
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
   LET v_tipo_solicitud = "";
   LET v_d_f_sol_fin         = NULL;
   LET v_d_f_sol_ini         = NULL;
   LET v_d_f_pago_fin        = NULL;
   LET v_d_f_pago_ini        = NULL;
   LET v_d_f_autoriza_ini    = NULL;
   LET v_d_f_autoriza_fin    = NULL;
   LET v_tot_registros       = 0;
   LET v_suma_total          = 0;
         
    --INITIALIZE v_query_where  TO NULL
    LET v_query_where = " "
    LET v_query_where_act = " "
    CALL v_arr_detalle.clear()
    LET v_indice = 1

    -- se borran los arreglos de despliegue
    DISPLAY "El nss es ", v_nss
    --INITIALIZE 
    LET v_query_where = " "

    --modificación de validación de  captura de parametros
    --valida que se ingrese al menos un parametro
    -- Se valida que existan registros para la consulta

    SELECT COUNT(*)
    INTO   v_registros 
    FROM   ret_datos_extrac_ssv
    WHERE  id_peticion = p_id_peticion

    IF v_registros > 0 THEN 
        -- se buscan las condiciones para la consulta
        -- Se busca si se capturaron NSS's para consulta
        CALL fn_busca_registros(p_id_peticion, 1) RETURNING v_registros
        IF v_registros > 0 THEN
            CALL fn_obtiene_condicion(p_id_peticion, 1) RETURNING v_condicion
            LET v_query_where = v_query_where CLIPPED, " AND nss IN ", v_condicion CLIPPED, "\n"
            LET v_query_where_act = v_query_where_act CLIPPED , " AND rsg.nss IN ", v_condicion CLIPPED, "\n"
        END IF 
        -- Se busca si se capturaron CURP's para consulta
        CALL fn_busca_registros(p_id_peticion, 8) RETURNING v_registros
        IF v_registros > 0 THEN
            CALL fn_obtiene_condicion(p_id_peticion, 8) RETURNING v_condicion
            LET v_query_where = v_query_where CLIPPED, " AND curp IN ", v_condicion CLIPPED, "\n"  
            LET v_query_where_act = v_query_where_act CLIPPED , " AND ad.curp IN ", v_condicion CLIPPED, "\n"
        END IF 
        -- Se busca si se capturaron RFC's para consulta
        CALL fn_busca_registros(p_id_peticion, 11) RETURNING v_registros
        IF v_registros > 0 THEN
            CALL fn_obtiene_condicion(p_id_peticion, 11) RETURNING v_condicion
            LET v_query_where = v_query_where CLIPPED, " AND rfc IN ", v_condicion CLIPPED, "\n"  
            LET v_query_where_act = v_query_where_act CLIPPED , " AND ad.rfc IN ", v_condicion CLIPPED, "\n"
        END IF 
        -- Se busca si se capturaron Docs de Pago para consulta
        CALL fn_busca_registros(p_id_peticion, 9) RETURNING v_registros
        IF v_registros > 0 THEN
            CALL fn_obtiene_condicion(p_id_peticion, 9) RETURNING v_condicion
            LET v_query_where = v_query_where CLIPPED, " AND documento_pago IN ", v_condicion CLIPPED, "\n"  
            LET v_query_where_act = v_query_where_act CLIPPED , " AND rwcpf.rsp_documento IN ", v_condicion CLIPPED, "\n"
        END IF 
        -- Se busca si se capturaron Casos ADAI/CRM para consulta
        CALL fn_busca_registros(p_id_peticion, 17) RETURNING v_registros
        IF v_registros > 0 THEN
            CALL fn_obtiene_condicion(p_id_peticion, 17) RETURNING v_condicion
            LET v_query_where = v_query_where CLIPPED, " AND caso_adai IN ", v_condicion CLIPPED, "\n"  
            LET v_query_where_act = v_query_where_act CLIPPED , " AND rsg.caso_adai IN ", v_condicion CLIPPED, "\n"
        END IF 
        -- Se busca si se capturaron Documentos para consulta
        CALL fn_busca_registros(p_id_peticion, 14) RETURNING v_registros
        IF v_registros > 0 THEN
            CALL fn_obtiene_condicion(p_id_peticion, 14) RETURNING v_condicion
            LET v_query_where = v_query_where CLIPPED, " AND documento IN ", v_condicion CLIPPED, "\n"  
            LET v_query_where_act = v_query_where_act CLIPPED , " AND rrf.cta_x_pagar IN ", v_condicion CLIPPED, "\n"
        END IF 
        -- Se busca si se capturaron Ejercicios para consulta
        CALL fn_busca_registros(p_id_peticion, 12) RETURNING v_registros
        IF v_registros > 0 THEN
            CALL fn_obtiene_condicion(p_id_peticion, 12) RETURNING v_condicion
            LET v_query_where = v_query_where CLIPPED, " AND ejercicio IN ", v_condicion CLIPPED, "\n"  
            LET v_query_where_act = v_query_where_act CLIPPED , " AND rrf.anho IN ", v_condicion CLIPPED, "\n"
        END IF 
        -- Se busca si se capturaron Referencias de Pago para consulta
        CALL fn_busca_registros(p_id_peticion, 15) RETURNING v_registros
        IF v_registros > 0 THEN
            CALL fn_obtiene_condicion(p_id_peticion, 15) RETURNING v_condicion
            LET v_query_where = v_query_where CLIPPED, " AND referencia_pago IN ", v_condicion CLIPPED, "\n"  
            LET v_query_where_act = v_query_where_act CLIPPED , " AND rwcpf.rsp_referencia IN ", v_condicion CLIPPED, "\n"
        END IF 
        -- Se busca si se capturaron Números de Laudo para consulta
        CALL fn_busca_registros(p_id_peticion, 18) RETURNING v_registros
        IF v_registros > 0 THEN
            CALL fn_obtiene_condicion(p_id_peticion, 18) RETURNING v_condicion
            LET v_query_where = v_query_where CLIPPED, " AND num_laudo IN ", v_condicion CLIPPED, "\n"  
            LET v_query_where_act = v_query_where_act CLIPPED , " AND rbj.num_acuerdo IN ", v_condicion CLIPPED, "\n"
        END IF 
        -- Se busca si se capturaron Números de Junta para consulta
        CALL fn_busca_registros(p_id_peticion, 10) RETURNING v_registros
        IF v_registros > 0 THEN
            CALL fn_obtiene_condicion(p_id_peticion, 10) RETURNING v_condicion
            LET v_query_where = v_query_where CLIPPED, " AND num_junta IN ", v_condicion CLIPPED, "\n"
            --- No se incluye en la consulta de los registros actuales ya que no se cuenta con este dato  
        END IF 
        -- Se busca si se capturaron Claves de Banco para consulta
        CALL fn_busca_registros(p_id_peticion, 13) RETURNING v_registros
        IF v_registros > 0 THEN
            CALL fn_obtiene_condicion(p_id_peticion, 13) RETURNING v_condicion
            LET v_query_where = v_query_where CLIPPED, " AND clave_banco IN ", v_condicion CLIPPED, "\n"  
            LET v_query_where_act = v_query_where_act CLIPPED , " AND SUBSTR(rrf.cta_clabe,1,3) IN ", v_condicion CLIPPED, "\n"
        END IF 
        -- Se busca si se capturaron Cuentas Clabe para consulta
        CALL fn_busca_registros(p_id_peticion, 16) RETURNING v_registros
        IF v_registros > 0 THEN
            CALL fn_obtiene_condicion(p_id_peticion, 16) RETURNING v_condicion
            LET v_query_where = v_query_where CLIPPED, " AND cuenta IN ", v_condicion CLIPPED, "\n"  
            LET v_query_where_act = v_query_where_act CLIPPED , " AND rrf.cta_clabe IN ", v_condicion CLIPPED, "\n"
        END IF 
        -- Se busca si se capturaron Claves de Afore para consulta
        CALL fn_busca_registros(p_id_peticion, 19) RETURNING v_registros
        IF v_registros > 0 THEN
            CALL fn_obtiene_condicion(p_id_peticion, 19) RETURNING v_condicion
            LET v_query_where = v_query_where CLIPPED, " AND cve_afore IN ", v_condicion CLIPPED, "\n"  
            LET v_query_where_act = v_query_where_act CLIPPED , " AND rwadva.claveafore IN ", v_condicion CLIPPED, "\n"
        END IF 
        -- Se busca si se capturó Usuario para consulta
        CALL fn_busca_registros(p_id_peticion, 20) RETURNING v_registros
        IF v_registros = 1 THEN
            CALL fn_obtiene_condicion(p_id_peticion, 20) RETURNING v_condicion
            LET v_query_where = v_query_where CLIPPED, " AND usuario IN ", v_condicion CLIPPED, "\n"  
            LET v_query_where_act = v_query_where_act CLIPPED , " AND rlg.usuario IN ", v_condicion CLIPPED, "\n"
        END IF 
        -- Se busca si se capturó Acreedor para consulta
        CALL fn_busca_registros(p_id_peticion, 21) RETURNING v_registros
        IF v_registros = 1 THEN
            CALL fn_obtiene_condicion(p_id_peticion, 21) RETURNING v_condicion
            LET v_query_where = v_query_where CLIPPED, " AND acreedor IN ", v_condicion CLIPPED, "\n"  
            LET v_query_where_act = v_query_where_act CLIPPED , " AND rrf.acreedor IN ", v_condicion CLIPPED, "\n"
        END IF 
        -- Se busca si se capturó Anulado para consulta
        CALL fn_busca_registros(p_id_peticion, 22) RETURNING v_registros
        IF v_registros = 1 THEN
            CALL fn_obtiene_condicion(p_id_peticion, 22) RETURNING v_condicion
            LET v_query_where = v_query_where CLIPPED, " AND anulado IN ", v_condicion CLIPPED, "\n"  
            --- No se incluye en la consulta de los registros actuales ya que no se cuenta con este dato
        END IF 
        -- Se busca si se eligieron Entidades para consulta
        CALL fn_busca_registros(p_id_peticion, 26) RETURNING v_registros
        IF v_registros > 0 THEN
            CALL fn_obtiene_condicion(p_id_peticion, 26) RETURNING v_condicion
            LET v_query_where = v_query_where CLIPPED, " AND entidad IN ", v_condicion CLIPPED, "\n"  
            LET v_query_where_act = v_query_where_act CLIPPED , " AND rbg.id_entidad_federativa IN ", v_condicion CLIPPED, "\n"
        END IF 
        -- Se busca si se eligieron Grupos para consulta
        CALL fn_busca_registros(p_id_peticion, 23) RETURNING v_registros
        IF v_registros > 0 THEN
            CALL fn_obtiene_condicion(p_id_peticion, 23) RETURNING v_condicion
            LET v_query_where = v_query_where CLIPPED, " AND grupo IN ", v_condicion CLIPPED, "\n"  
            LET v_query_where_act = v_query_where_act CLIPPED , " AND rlg.gpo_ley73 IN ", v_condicion CLIPPED, "\n"
        END IF 
        -- Se busca si se eligieron Origen para consulta
        CALL fn_busca_registros(p_id_peticion, 27) RETURNING v_registros
        IF v_registros > 0 THEN
            CALL fn_obtiene_condicion(p_id_peticion, 27) RETURNING v_condicion
            LET v_query_where = v_query_where CLIPPED, " AND origen IN ", v_condicion CLIPPED, "\n"  
        END IF 
        -- Se busca si se eligieron Código Origen para consulta
        CALL fn_busca_registros(p_id_peticion, 28) RETURNING v_registros
        IF v_registros > 0 THEN
            CALL fn_obtiene_condicion(p_id_peticion, 28) RETURNING v_condicion
            LET v_query_where_act = v_query_where_act CLIPPED , " AND rsme.medio_entrega IN ", v_condicion CLIPPED, "\n"
        END IF 
        -- Se busca si se eligieron Vias de Pago para consulta
        CALL fn_busca_registros(p_id_peticion, 29) RETURNING v_registros
        IF v_registros > 0 THEN
            CALL fn_obtiene_condicion(p_id_peticion, 29) RETURNING v_condicion
            LET v_query_where = v_query_where CLIPPED, " AND via_pago IN ", v_condicion CLIPPED, "\n"  
            LET v_query_where_act = v_query_where_act CLIPPED , " AND rcme.descripcion IN ", v_condicion CLIPPED, "\n"
        END IF 
        -- Se busca si se eligieron Estados de Solicitud para consulta
        CALL fn_busca_registros(p_id_peticion, 25) RETURNING v_registros
        IF v_registros > 0 THEN
            CALL fn_obtiene_condicion(p_id_peticion, 25) RETURNING v_condicion
            LET v_query_where = v_query_where CLIPPED, " AND estado_solicitud IN ", v_condicion CLIPPED, "\n"  
            LET v_query_where_act = v_query_where_act CLIPPED , " AND rsg.estado_solicitud IN ", v_condicion CLIPPED, "\n"
        END IF 
        -- Se busca si se eligieron Beneficiarios/Titular/Blancos para consulta ************ pendiente *************
        CALL fn_busca_registros(p_id_peticion, 24) RETURNING v_registros
        IF v_registros > 0 THEN
            -- Busca si se eligieron BLANCOS
            SELECT COUNT(*) 
            INTO   v_registros 
            FROM   ret_datos_extrac_ssv
            WHERE  id_peticion = p_id_peticion
            AND    ind_dato    = 24
            AND    valor = "BLANCOS"
            IF v_registros = 1 THEN 
                LET v_query_where = v_query_where CLIPPED, " AND beneficiario NOT IN ('TITULAR','BENEFICIARIO') \n"
            ELSE 
                SELECT COUNT(*) 
                INTO   v_registros 
                FROM   ret_datos_extrac_ssv
                WHERE  id_peticion = p_id_peticion
                AND    ind_dato    = 24
                AND    valor IN ('TITULAR','BENEFICIARIO')
                IF v_registros > 0 THEN 
                    CALL fn_obtiene_condicion_benef_ant(p_id_peticion, 25) RETURNING v_condicion
                    LET v_query_where = v_query_where CLIPPED, " AND beneficiario IN ",v_condicion CLIPPED, " \n"
                END IF 
                SELECT COUNT(*) 
                INTO   v_registros 
                FROM   ret_datos_extrac_ssv
                WHERE  id_peticion = p_id_peticion
                AND    ind_dato    = 24
                AND    valor IN ('1','2')
                IF v_registros > 0 THEN 
                    CALL fn_obtiene_condicion_benef_act(p_id_peticion, 25) RETURNING v_condicion
                    LET v_query_where_act = v_query_where_act CLIPPED, " AND rbg.tpo_beneficiario IN ",v_condicion CLIPPED, " \n"
                END IF 
            END IF 
        END IF 
        -- Se busca si se eligieron Fechas de Solicitud para consulta
        CALL fn_busca_registros(p_id_peticion, 4) RETURNING v_registros
        IF v_registros = 1 THEN
            CALL fn_busca_registros(p_id_peticion, 5) RETURNING v_registros
            IF v_registros = 1 THEN 
                LET v_fecha_ini = ""
                LET v_fecha_fin = ""
                CALL fn_obtiene_condicion(p_id_peticion, 4) RETURNING v_fecha_ini
                CALL fn_obtiene_condicion(p_id_peticion, 5) RETURNING v_fecha_fin
                LET v_query_where = v_query_where CLIPPED, " AND fecha_tramite BETWEEN ", v_fecha_ini CLIPPED, " AND ", v_fecha_fin CLIPPED, "\n"  
                LET v_query_where_act = v_query_where_act CLIPPED , " AND rsg.f_solicitud BETWEEN ", v_fecha_ini CLIPPED, " AND ", v_fecha_fin CLIPPED, "\n"
            END IF 
        END IF 
        -- Se busca si se eligieron Fechas de Pago para consulta
        CALL fn_busca_registros(p_id_peticion, 2) RETURNING v_registros
        IF v_registros = 1 THEN
            CALL fn_busca_registros(p_id_peticion, 3) RETURNING v_registros
            IF v_registros = 1 THEN 
                LET v_fecha_ini = ""
                LET v_fecha_fin = ""
                CALL fn_obtiene_condicion(p_id_peticion, 2) RETURNING v_fecha_ini
                CALL fn_obtiene_condicion(p_id_peticion, 3) RETURNING v_fecha_fin
                LET v_query_where = v_query_where CLIPPED, " AND fecha_pago BETWEEN ", v_fecha_ini CLIPPED, " AND ", v_fecha_fin CLIPPED, "\n"  
                LET v_query_where_act = v_query_where_act CLIPPED , " AND MDY(SUBSTR(rwcpf.rsp_f_pago,5,2),SUBSTR(rwcpf.rsp_f_pago,7,2),SUBSTR(rwcpf.rsp_f_pago,1,4)) BETWEEN ", v_fecha_ini CLIPPED, " AND ", v_fecha_fin CLIPPED, "\n"
            END IF 
        END IF 
        -- Se busca si se eligieron Fechas de Autorización para consulta
        CALL fn_busca_registros(p_id_peticion, 6) RETURNING v_registros
        IF v_registros = 1 THEN
            CALL fn_busca_registros(p_id_peticion, 7) RETURNING v_registros
            IF v_registros = 1 THEN 
                LET v_fecha_ini = ""
                LET v_fecha_fin = ""
                CALL fn_obtiene_condicion(p_id_peticion, 6) RETURNING v_fecha_ini
                CALL fn_obtiene_condicion(p_id_peticion, 7) RETURNING v_fecha_fin
                LET v_query_where = v_query_where CLIPPED, " AND fecha_autorizacion BETWEEN ", v_fecha_ini CLIPPED, " AND ", v_fecha_fin CLIPPED, "\n"  
                LET v_query_where_act = v_query_where_act CLIPPED , " AND rwpm.fecha_autorizacion BETWEEN ", v_fecha_ini CLIPPED, " AND ", v_fecha_fin CLIPPED, "\n"
            END IF 
        END IF 
        DISPLAY "La clausula Where >", v_query_where, "<"
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

        LET v_query = "\n SELECT *              ",
                   "\n FROM   ret_ley73_hist ", 
                   "\n WHERE  1 = 0 ",
                   "INTO TEMP tmp_ret_ley73_actual "
        -- consulta INSERT
        DISPLAY "Creacion de la tabla:\n", v_query

        -- se llena el arreglo con los historicos 
        PREPARE sid_query_vacia_act FROM v_query
        EXECUTE sid_query_vacia_act
                     
        LET v_query = "\n INSERT INTO tmp_ret_ley73_hist  ",
                   "\n SELECT *                        ",
                   "\n FROM   ret_ley73_hist           ", 
                   "\n WHERE  1 = 1                    ",
                   v_query_where
        -- consulta INSERT
        DISPLAY "Consulta Insert del historico:\n", v_query

        -- se llena el arreglo con los historicos
        PREPARE sid_query FROM v_query
        EXECUTE sid_query

        --- Se integran las solicitudes actuales
        LET v_query = 
                    "SELECT rsg.nss AS nss,                                                                                                            \n", 
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
                    "       '' AS fecha_autorizacion,                                 \n",  -- rwpm.fecha_autorizacion
                    "       '' AS documento, '' AS ejercicio,                         \n",  -- rrf.cta_x_pagar,   rrf.anho
                    "       '' AS documento_pago,                                     \n",  -- rwcpf.rsp_documento
--                    "       NVL(MDY(SUBSTR(rwcpf.rsp_f_pago,5,2),SUBSTR(rwcpf.rsp_f_pago,7,2),SUBSTR(rwcpf.rsp_f_pago,1,4)),MDY(1,1,1)) AS fecha_pago,           \n",
                    "       '' AS fecha_pago,                                         \n",
--                    "       (ABS(NVL(rpteso.monto_pesos,0)+NVL(rpviv92.monto_pesos,0)+ \n",
--                    "       NVL(rpviv97.monto_pesos,0)))*(rbg.porcentaje/100) AS importe_pago,              \n",
                    --                                   "       rwcpf.rsp_importe AS importe_pago,                        \n",
                    "       0 AS importe_pago,                                        \n",
                    "       '' AS referencia_pago,                                    \n",   -- rwcpf.rsp_referencia
                    "       rsg.caso_adai AS caso_adai, rbj.num_acuerdo AS num_laudo, \n",
                    "       '' AS num_junta, '' AS importe_pago_anterior,             \n",
                    "       '' AS fch_pago_anterior,                                  \n",
--                    "       NVL(SUBSTR(rrf.cta_clabe,1,3),'') AS clave_banco,         \n",
                    "       '' AS clave_banco,                                        \n",
                    "       '' AS cuenta, '' AS importe_transf,                       \n", -- rrf.cta_clabe 
                    "       '' AS fch_transf, '' AS ssv_dif,                          \n",
                    "       NVL(MDY(SUBSTR(rbj.fecha_ejecucion,6,2),SUBSTR(rbj.fecha_ejecucion,9,2),SUBSTR(rbj.fecha_ejecucion,1,4)),MDY(1,1,1)) AS fch_marca_tj, \n",
                    "       '' AS error_fico, '' AS cve_afore,                        \n",
                    "       rlg.importe_viv97*(rbg.porcentaje/100) AS pesos_viv97,                         \n",
                    "       rlg.aivs_viv97*(rbg.porcentaje/100) AS aivs_viv97,                             \n",
                    "       rlg.importe_viv92*(rbg.porcentaje/100) AS pesos_viv92,                         \n",
                    "       rlg.aivs_viv92*(rbg.porcentaje/100) AS aivs_viv92, rlg.usuario AS usuario,     \n",
                    "       '' AS cabecera, '' AS acreedor,                           \n",  -- rrf.acreedor
                    "       '' AS via_pago_ac, '' AS bloqueo_pago, '' AS anulado,     \n",
                    "       '' AS ejercicio_anulacion, '' AS fch_carga,                \n",
                    "       rsg.id_solicitud, NVL(rbg.consec_beneficiario,0) as consec_beneficiario,                \n",
                    "       NVL(rbg.tpo_beneficiario,0) as tpo_beneficiario           \n",
                    "FROM   ret_estado_solicitud      res,                            \n",
                    "       ret_sol_medio_entrega     rsme,                           \n",
                    "       ret_cat_medio_entrega     rcme,                           \n",
                    "       afi_derechohabiente       ad,                             \n",
                    "       ret_solicitud_generico    rsg                             \n",
                    "       LEFT OUTER JOIN ret_ley73_generico        rlg             \n",
                    "                    ON rsg.id_solicitud = rlg.id_solicitud       \n",
                    "       LEFT OUTER JOIN ret_beneficiario_generico rbg             \n",
                    "                    ON rsg.id_solicitud = rbg.id_solicitud       \n",
                    "       LEFT OUTER JOIN ret_beneficiario_juridico rbj             \n",
                    "                    ON rbj.id_solicitud = rbg.id_solicitud       \n",
                    "                   AND rbj.consec_beneficiario = rbg.consec_beneficiario  \n",
                    "WHERE  rsg.estado_solicitud   = res.estado_solicitud             \n",
                    "AND    rsg.id_solicitud       = rsme.id_solicitud                \n",
                    "AND    rsme.medio_entrega     = rcme.medio_entrega               \n",
                    "AND    rsg.modalidad_retiro   = 3                                \n",
                    "AND    rsg.id_derechohabiente = ad.id_derechohabiente            \n"
                     
        -- se llena el arreglo con los actuales 
        DISPLAY "El Query actual", v_query
        
        PREPARE sid_query_tmp_act FROM v_query
        DECLARE cur_id_query CURSOR FOR sid_query_tmp_act 
        FOREACH cur_id_query INTO v_reg_actual.*, v_id_solicitud_generico, v_consec_beneficiario, v_tpo_beneficiario 
           CALL fn_busca_fecha_autorizacion(v_reg_actual.caso_adai, v_tpo_beneficiario) 
                RETURNING v_reg_actual.fecha_autorizacion
           CALL fn_busca_fecha_pago(v_id_solicitud_generico,v_consec_beneficiario,v_tpo_beneficiario) 
                RETURNING v_reg_actual.documento_pago,v_reg_actual.referencia_pago, v_reg_actual.fecha_pago
           CALL fn_busca_inf_respuesta_fico(v_id_solicitud_generico,v_consec_beneficiario,v_tpo_beneficiario) 
                RETURNING v_reg_actual.documento,v_reg_actual.ejercicio, v_reg_actual.clave_banco, v_reg_actual.cuenta, v_reg_actual.acreedor
           CALL fn_busca_montos(v_id_solicitud_generico, v_consec_beneficiario, v_tpo_beneficiario)  
                RETURNING v_reg_actual.importe_pago
           INSERT INTO tmp_ret_ley73_actual VALUES (v_reg_actual.*)
        END FOREACH 

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

        LET v_query = "\n SELECT *                    ", 
                   "\n FROM   tmp_ret_ley73_hist   " 
        -- consulta
        DISPLAY "Consulta Cifras:\n", v_query
        PREPARE prp_detalle FROM v_query
        DECLARE cur_detalle CURSOR FOR prp_detalle
        LET v_indice = 1
        --DISPLAY v_s_qryTxt
        FOREACH cur_detalle INTO v_arr_detalle[v_indice].*
            --                        DISPLAY "Las Fechas"
            IF v_arr_detalle[v_indice].fch_marca_tj IS NOT NULL AND LENGTH(v_arr_detalle[v_indice].fch_marca_tj CLIPPED) = 10 THEN 
                LET v_arr_detalle[v_indice].fch_marca_tj = v_arr_detalle[v_indice].fch_marca_tj[4,5]||"/"||v_arr_detalle[v_indice].fch_marca_tj[1,2]||"/"||v_arr_detalle[v_indice].fch_marca_tj[7,10]   
            END IF 
            IF v_arr_detalle[v_indice].fch_pago_anterior IS NOT NULL AND LENGTH(v_arr_detalle[v_indice].fch_pago_anterior CLIPPED) = 10 THEN 
                LET v_arr_detalle[v_indice].fch_pago_anterior = v_arr_detalle[v_indice].fch_pago_anterior[4,5]||"/"||v_arr_detalle[v_indice].fch_pago_anterior[1,2]||"/"||v_arr_detalle[v_indice].fch_pago_anterior[7,10]   
            END IF 
            IF v_arr_detalle[v_indice].fch_transf IS NOT NULL AND LENGTH(v_arr_detalle[v_indice].fch_transf CLIPPED) = 10 THEN 
                LET v_arr_detalle[v_indice].fch_transf = v_arr_detalle[v_indice].fch_transf[4,5]||"/"||v_arr_detalle[v_indice].fch_transf[1,2]||"/"||v_arr_detalle[v_indice].fch_transf[7,10]   
            END IF 
            IF v_arr_detalle[v_indice].fecha_autorizacion IS NOT NULL AND LENGTH(v_arr_detalle[v_indice].fecha_autorizacion CLIPPED) = 10  THEN
            --                           DISPLAY "La fecha autorización antes del reformateo :>>",v_arr_detalle[v_indice].fecha_autorizacion, "<<"
                LET v_arr_detalle[v_indice].fecha_autorizacion = v_arr_detalle[v_indice].fecha_autorizacion[4,5]||"/"||v_arr_detalle[v_indice].fecha_autorizacion[1,2]||"/"||v_arr_detalle[v_indice].fecha_autorizacion[7,10]   
            END IF 
            IF v_arr_detalle[v_indice].fecha_pago IS NOT NULL AND LENGTH(v_arr_detalle[v_indice].fecha_pago CLIPPED) = 10 THEN 
                LET v_arr_detalle[v_indice].fecha_pago = v_arr_detalle[v_indice].fecha_pago[4,5]||"/"||v_arr_detalle[v_indice].fecha_pago[1,2]||"/"||v_arr_detalle[v_indice].fecha_pago[7,10]   
            END IF 
            IF v_arr_detalle[v_indice].fecha_tramite IS NOT NULL AND LENGTH(v_arr_detalle[v_indice].fecha_tramite CLIPPED) = 10 THEN 
                LET v_arr_detalle[v_indice].fecha_tramite = v_arr_detalle[v_indice].fecha_tramite[4,5]||"/"||v_arr_detalle[v_indice].fecha_tramite[1,2]||"/"||v_arr_detalle[v_indice].fecha_tramite[7,10]   
            END IF
            IF v_arr_detalle[v_indice].clave_banco  IS NOT NULL THEN 
                LET v_cve_banco_int = v_arr_detalle[v_indice].clave_banco
                LET v_arr_detalle[v_indice].clave_banco = v_cve_banco_int USING "&&&&&"
            END IF 

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
        IF v_indice > 2 THEN 
            CALL fn_exporta_archivo(v_query) RETURNING v_resultado;
        ELSE 
            DISPLAY "No existen registros para exportar"
        END IF 
    ELSE 
        DISPLAY "No existen criterios de búsqueda para la ejecución del proceso, no se generará información"
    END IF 

END FUNCTION

FUNCTION fn_busca_registros(param_id_peticion, param_ind_dato)
DEFINE param_id_peticion  DECIMAL(9,0)
DEFINE param_ind_dato     SMALLINT 
DEFINE v_registros        INTEGER

    LET v_registros = 0
    
    SELECT COUNT(*)
    INTO   v_registros
    FROM   ret_datos_extrac_ssv
    WHERE  id_peticion = param_id_peticion
    AND    ind_dato    = param_ind_dato
    
RETURN v_registros
END FUNCTION 

FUNCTION fn_obtiene_condicion (param_id_peticion, param_ind_dato)
DEFINE param_id_peticion  DECIMAL(9,0)
DEFINE param_ind_dato     SMALLINT 
DEFINE v_cadena           STRING

    LET v_cadena = "(SELECT valor FROM ret_datos_extrac_ssv WHERE id_peticion = ", param_id_peticion, " AND ind_dato = ", param_ind_dato, ")"
    
RETURN v_cadena       
END FUNCTION 

FUNCTION fn_obtiene_condicion_benef_ant(param_id_peticion, param_ind_dato)
DEFINE param_id_peticion  DECIMAL(9,0)
DEFINE param_ind_dato     SMALLINT 
DEFINE v_cadena           STRING

    LET v_cadena = "(SELECT valor FROM ret_datos_extrac_ssv WHERE id_peticion = ", param_id_peticion, " AND ind_dato = ", param_ind_dato, " AND valor IN ('TITULAR','BENEFICIARIO'))"
    
RETURN v_cadena       
END FUNCTION 
FUNCTION fn_obtiene_condicion_benef_act(param_id_peticion, param_ind_dato)
DEFINE param_id_peticion  DECIMAL(9,0)
DEFINE param_ind_dato     SMALLINT 
DEFINE v_cadena           STRING

    LET v_cadena = "(SELECT valor FROM ret_datos_extrac_ssv WHERE id_peticion = ", param_id_peticion, " AND ind_dato = ", param_ind_dato, " AND valor IN ('1','2'))"
    
RETURN v_cadena       
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
   LET v_cve_banco = 0
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
   LET v_nom_archivo = TODAY USING "yyyymmdd", "_", p_id_peticion USING "&&&&&&" 
   LET v_archivo_txt = v_nom_archivo CLIPPED, ".cpdssv"

   -- El archivo con ruta destino que contiene el detalle 
   LET v_v_ruta_nomarch = v_c_ruta_env CLIPPED , "/", v_archivo_txt

   -- Se muestra mensaje
   DISPLAY "Se generará el archivo:\n\n\t", v_v_ruta_nomarch
   
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
      
      LET v_s_detalle =  v_registro_archivo[v_indice].nss                            , "|",
           v_registro_archivo[v_indice].estado_solicitud                             , "|",
           v_registro_archivo[v_indice].descripcion                                  , "|",
           v_registro_archivo[v_indice].origen                                       , "|",
           v_registro_archivo[v_indice].via_pago                                     , "|",
           v_registro_archivo[v_indice].grupo                                        , "|",
           v_registro_archivo[v_indice].nombre                                       , "|",
           v_registro_archivo[v_indice].ap_paterno                                   , "|",
           v_registro_archivo[v_indice].ap_materno                                   , "|",
           v_registro_archivo[v_indice].beneficiario                                 , "|",
           v_registro_archivo[v_indice].nombre_beneficiario                          , "|",
           v_registro_archivo[v_indice].ap_paterno_beneficiario                      , "|",
           v_registro_archivo[v_indice].ap_materno_beneficiario                      , "|",
           v_registro_archivo[v_indice].curp                                         , "|",
           v_registro_archivo[v_indice].rfc                                          , "|",
           v_registro_archivo[v_indice].entidad                                      , "|",
           v_registro_archivo[v_indice].fecha_tramite                                , "|",
           v_registro_archivo[v_indice].fecha_autorizacion                           , "|",
           v_registro_archivo[v_indice].documento                                    , "|",
           v_registro_archivo[v_indice].ejercicio                                    , "|",
           v_registro_archivo[v_indice].documento_pago                               , "|",
           v_registro_archivo[v_indice].fecha_pago                                   , "|",
           v_registro_archivo[v_indice].importe_pago                                 , "|",
           v_registro_archivo[v_indice].referencia_pago                              , "|",
           v_registro_archivo[v_indice].caso_adai                                    , "|",
           v_registro_archivo[v_indice].num_laudo                                    , "|",
           v_registro_archivo[v_indice].num_junta                                    , "|",
           v_registro_archivo[v_indice].importe_pago_anterior                        , "|",
           v_registro_archivo[v_indice].fch_pago_anterior                            , "|",
           v_registro_archivo[v_indice].clave_banco                                  , "|",
           v_registro_archivo[v_indice].cuenta                                       , "|",
           v_registro_archivo[v_indice].importe_transf                               , "|",
           v_registro_archivo[v_indice].fch_transf                                   , "|",
           v_registro_archivo[v_indice].ssv_dif                                      , "|",
           v_registro_archivo[v_indice].fch_marca_tj                                 , "|",
           v_registro_archivo[v_indice].error_fico                                   , "|",
           v_registro_archivo[v_indice].cve_afore                                    , "|",
           v_registro_archivo[v_indice].pesos_viv97                                  , "|",
           v_registro_archivo[v_indice].aivs_viv97                                   , "|",
           v_registro_archivo[v_indice].pesos_viv92                                  , "|",
           v_registro_archivo[v_indice].aivs_viv92                                   , "|",
           v_registro_archivo[v_indice].usuario                                      , "|",
           v_registro_archivo[v_indice].cabecera                                     , "|",
           v_registro_archivo[v_indice].acreedor                                     , "|",
           v_registro_archivo[v_indice].via_pago_ac                                  , "|",
           v_registro_archivo[v_indice].bloqueo_pago                                 , "|",
           v_registro_archivo[v_indice].anulado                                      , "|",
           v_registro_archivo[v_indice].ejercicio_anulacion                          , "|"
           CALL v_ch_arch_ret_generico.write(v_s_detalle)
         LET v_indice = v_indice + 1
   END FOREACH

   -- Se cierra el archivo de Rechazados
   CALL v_ch_arch_ret_generico.close()

    DISPLAY "El archivo fue generado exitosamente:\n\n\t", v_v_ruta_nomarch

   RETURN v_regresa
   
END FUNCTION

FUNCTION fn_busca_fecha_autorizacion (p_caso_crm, p_tpo_beneficiario)
   DEFINE p_caso_crm           CHAR(10)
   DEFINE p_tpo_beneficiario   SMALLINT
   DEFINE v_f_autorizacion     DATE 

   IF p_tpo_beneficiario <> 0 THEN 
      IF p_tpo_beneficiario = 1 THEN 
         SELECT MAX(rp.f_peticion) 
         INTO   v_f_autorizacion
         FROM   ret_ws_peticion_marca rp,        
                ret_ws_det_peticion_marca rd     
         WHERE  rp.id_peticion = rd.id_peticion  
         AND    rd.ind_marca = 3                 
         AND    rd.modalidad_retiro = 3
         AND    rp.caso_adai = p_caso_crm
      ELSE 
         SELECT MAX(rp.f_peticion) 
         INTO   v_f_autorizacion
         FROM   ret_ws_peticion_act_benef rp
         WHERE  caso_crm = p_caso_crm
      END IF 
   ELSE
      INITIALIZE v_f_autorizacion TO NULL
   END IF

   RETURN v_f_autorizacion

END FUNCTION 

FUNCTION fn_busca_fecha_pago(p_id_solicitud, p_consec_beneficiario, p_tpo_beneficiario)
   DEFINE p_id_solicitud         DECIMAL(9,0)
   DEFINE p_consec_beneficiario  SMALLINT
   DEFINE p_tpo_beneficiario     SMALLINT
   DEFINE v_documento            CHAR(10)
   DEFINE v_referencia           CHAR(25)
   DEFINE v_f_pago               CHAR(8)
   DEFINE v_fecha_pago           DATE
   DEFINE vc_id_solicitud        CHAR(10)

   INITIALIZE v_fecha_pago TO NULL
   INITIALIZE v_f_pago TO NULL
   INITIALIZE v_documento TO NULL
   INITIALIZE v_referencia TO NULL
   INITIALIZE vc_id_solicitud TO NULL
   
   IF p_tpo_beneficiario <> 0 THEN 
      IF p_tpo_beneficiario = 1 THEN 
         SELECT DISTINCT rsp_documento, rsp_f_pago, rsp_referencia
         INTO   v_documento, v_f_pago, v_referencia
         FROM   ret_ws_consulta_pago_fico
         WHERE  id_solicitud = p_id_solicitud 
         AND    rsp_f_pago IS NOT NULL
         AND    rsp_referencia IS NOT NULL
         AND    rsp_estatus = 2
      ELSE 
         LET vc_id_solicitud = p_id_solicitud, p_consec_beneficiario
         SELECT DISTINCT rsp_documento, rsp_f_pago, rsp_referencia
         INTO   v_documento, v_f_pago, v_referencia
         FROM   ret_ws_consulta_pago_fico
         WHERE  id_solicitud = vc_id_solicitud 
         AND    rsp_f_pago IS NOT NULL
         AND    rsp_referencia IS NOT NULL
         AND    rsp_estatus = 2
      END IF 
      IF v_f_pago IS NOT NULL THEN
         LET v_fecha_pago = MDY(v_f_pago[5,6],v_f_pago[7,8],v_f_pago[1,4])
      ELSE 
         LET v_fecha_pago = MDY(1,1,1)
      END IF 
   ELSE
      LET v_documento = ''
      LET v_referencia = ''
      LET v_fecha_pago = MDY(1,1,1)
   END IF
   IF v_fecha_pago = '07/26/2019' THEN 
      DISPLAY "Fecha de pago a regresar :", v_fecha_pago, " - Solicitud: ",p_id_solicitud
   END IF 
   RETURN v_documento,v_referencia,v_fecha_pago
END FUNCTION 

FUNCTION fn_busca_inf_respuesta_fico(p_id_solicitud, p_consec_beneficiario, p_tpo_beneficiario)
   DEFINE p_id_solicitud         DECIMAL(9,0)
   DEFINE p_consec_beneficiario  SMALLINT
   DEFINE p_tpo_beneficiario     SMALLINT
   DEFINE v_documento            CHAR(10)
   DEFINE v_ejercicio            CHAR(4)
   DEFINE v_clave_banco          CHAR(3)
   DEFINE v_cuenta_clabe         CHAR(18)
   DEFINE v_acreedor             CHAR(10)
   DEFINE vc_id_solicitud        CHAR(10)

   IF p_tpo_beneficiario <> 0 THEN 
      IF p_tpo_beneficiario = 1 THEN 
         SELECT cta_x_pagar, anho, cta_clabe, acreedor
         INTO   v_documento, v_ejercicio, v_cuenta_clabe, v_acreedor
         FROM   ret_respuesta_fico
         WHERE  referencia = p_id_solicitud
         AND    folio      = (SELECT MAX(folio)
                              FROM   ret_respuesta_fico
                              WHERE  referencia = p_id_solicitud)
      ELSE
         LET vc_id_solicitud = p_id_solicitud, p_consec_beneficiario
         SELECT cta_x_pagar, anho, cta_clabe, acreedor
         INTO   v_documento, v_ejercicio, v_cuenta_clabe, v_acreedor
         FROM   ret_respuesta_fico
         WHERE  referencia = vc_id_solicitud 
         AND    folio      = (SELECT MAX(folio)
                              FROM   ret_respuesta_fico
                              WHERE  referencia = vc_id_solicitud )
      END IF 
      IF v_cuenta_clabe IS NOT NULL THEN 
         LET v_clave_banco = v_cuenta_clabe[1,3]
      END IF
   ELSE 
      LET v_documento    = ''
      LET v_ejercicio    = ''
      LET v_clave_banco  = ''
      LET v_cuenta_clabe = ''
      LET v_acreedor     = ''
   END IF 

   RETURN  v_documento, v_ejercicio, v_clave_banco, v_cuenta_clabe, v_acreedor
   
END FUNCTION
FUNCTION fn_busca_montos(p_id_solicitud, p_consec_beneficiario, p_tpo_beneficiario)
   DEFINE p_id_solicitud         DECIMAL(9,0)
   DEFINE p_consec_beneficiario  SMALLINT
   DEFINE p_tpo_beneficiario     SMALLINT
   DEFINE v_imp_tesofe           DECIMAL(22,2)
   DEFINE v_imp_viv92            DECIMAL(22,2)
   DEFINE v_imp_viv97            DECIMAL(22,2)
   DEFINE v_importe_total        DECIMAL(22,2)
   DEFINE v_porcentaje           DECIMAL(10,2)

   IF p_tpo_beneficiario = 0 THEN
      LET v_importe_total = 0
   ELSE 
      SELECT porcentaje
      INTO   v_porcentaje
      FROM   ret_beneficiario_generico
      WHERE  id_solicitud = p_id_solicitud
      AND    consec_beneficiario = p_consec_beneficiario

      SELECT NVL(SUM(ABS(monto_pesos)),0) AS monto
      INTO   v_imp_tesofe
      FROM   ret_preliquida
      WHERE  id_referencia = p_id_solicitud
      AND    monto_pesos < 0
      AND    subcuenta = 47
      
      SELECT NVL(SUM(ABS(monto_pesos)),0) AS monto
      INTO   v_imp_viv92
      FROM   ret_preliquida
      WHERE  id_referencia = p_id_solicitud
      AND    monto_pesos < 0
      AND    subcuenta = 8

      SELECT NVL(SUM(ABS(monto_pesos)),0) AS monto
      INTO   v_imp_viv97
      FROM   ret_preliquida
      WHERE  id_referencia = p_id_solicitud
      AND    monto_pesos < 0
      AND    subcuenta = 4
      
      LET v_importe_total = v_imp_tesofe + v_imp_viv92 + v_imp_viv97
      LET v_importe_total = v_importe_total * (v_porcentaje/100)
   END IF 

   RETURN  v_importe_total
   
END FUNCTION