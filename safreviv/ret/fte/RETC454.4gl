--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => RET                                                     #
#Programa          => RETC442                                                 #
#Objetivo          => AUTORIZACIÓN DE LAS SOLICITUDES DAP                     #
#Fecha Inicio      =>                                                         #
###############################################################################
DATABASE safre_viv

GLOBALS
----DEFINICION DE VARIABLES GLOBALES, PARAMETROS ENVIADOS DESDE EL MENÚ
DEFINE g_usuario      CHAR(20)
DEFINE g_tipo_proceso SMALLINT
DEFINE g_nom_ventana  STRING
DEFINE g_pid          LIKE bat_ctr_proceso.pid --  ID del proceso
DEFINE g_proceso_cod  LIKE cat_proceso.proceso_cod -- codigo del proceso
DEFINE g_opera_cod    LIKE cat_operacion.opera_cod -- codigo de operacion

DEFINE gr_param RECORD
        accion          CHAR(001)                           ,
        nss             LIKE afi_derechohabiente.nss                 ,
        consecutivo     LIKE afi_derechohabiente.id_derechohabiente
    END RECORD
DEFINE gar_precio_acc DYNAMIC ARRAY OF RECORD #Arreglo para los precios_accion
    estado                SMALLINT     ,
    fecha                 DATE         ,
    siefore               SMALLINT     ,
    precio_dia            DECIMAL(16,6)
END RECORD

DEFINE gr_rutas      RECORD LIKE safre_viv:seg_modulo.*
DEFINE gc_usuario              CHAR(020),
       enter                   CHAR(001)

DEFINE gr_prest RECORD
    desempleo       SMALLINT,
    matrimonio      SMALLINT
END RECORD
       
DEFINE gc_rpt_where  STRING

DEFINE p_usuario_cod   LIKE seg_usuario.usuario_cod -- clave del usuario firmado
      ,v_ventana       ui.WINDOW
      ,v_folio         DECIMAL(11,0)

DEFINE gr_edo RECORD
       capturado           LIKE ret_estado_solicitud.estado_solicitud    ,
       confirmado          LIKE ret_estado_solicitud.estado_solicitud    ,
       enviado             LIKE ret_estado_solicitud.estado_solicitud    ,
       autorizado          LIKE ret_estado_solicitud.estado_solicitud    ,
       provisionado        LIKE ret_estado_solicitud.estado_solicitud    ,
       liquidado           LIKE ret_estado_solicitud.estado_solicitud    ,
       preliquidado        LIKE ret_estado_solicitud.estado_solicitud    ,
       rechazado           LIKE ret_estado_solicitud.estado_solicitud    ,
       enviado_op16        LIKE ret_estado_solicitud.estado_solicitud    ,
       recibido_op16       LIKE ret_estado_solicitud.estado_solicitud
END RECORD

DEFINE arr_reg_marcas_activas  DYNAMIC ARRAY OF RECORD 
         v_marca                SMALLINT       ,
         v_desc_marca           CHAR(40)       ,
         v_fecha                DATE           ,
         v_hora                 CHAR(8)        ,
         v_referencia           DECIMAL(10,0)  ,
         v_usuario              CHAR(20)
      END RECORD 
DEFINE arr_reg_marcas_historicas  DYNAMIC ARRAY OF RECORD 
         v_fecha_fin            DATE           ,
         v_disgnostico          CHAR(20)       ,
         v_desc_diag            CHAR(40)       ,
         v_marca                SMALLINT       ,
         v_desc_marca           CHAR(40)       ,
         v_fecha_ini            DATE           ,
         v_hora                 CHAR(8)        ,
         v_estado_marca         SMALLINT       ,
         v_marca_causa          SMALLINT       ,
         v_fecha_causa          DATE           ,
         v_referencia           DECIMAL(10,0)  ,
         v_usuario              CHAR(20)       ,
         v_usuario_demarca      CHAR(20)
      END RECORD 
DEFINE arr_reg_movtos_97  DYNAMIC ARRAY OF RECORD 
         v_fecha_liquida        DATE           ,
         v_acciones             DECIMAL(16,2)  ,
         v_pesos                DECIMAL(16,2)  ,
         v_precio_accion        DECIMAL(10,6)  ,
         v_fondo                SMALLINT       ,
         v_subcuenta            SMALLINT       ,
         v_tipo_movimiento      CHAR(40)       ,
         v_folio                DECIMAL(10,0)  ,
         v_origen               CHAR(30)
      END RECORD 
DEFINE arr_reg_movtos_92  DYNAMIC ARRAY OF RECORD 
         v_fecha_liquida        DATE           ,
         v_acciones             DECIMAL(16,2)  ,
         v_pesos                DECIMAL(16,2)  ,
         v_precio_accion        DECIMAL(10,6)  ,
         v_fondo                SMALLINT       ,
         v_subcuenta            SMALLINT       ,
         v_tipo_movimiento      CHAR(40)       ,
         v_folio                DECIMAL(10,0)  ,
         v_origen               CHAR(30)
      END RECORD 
DEFINE arr_reg_pagos_previos  DYNAMIC ARRAY OF RECORD 
         documento              CHAR(10)     ,
         ejercicio              CHAR(4)      ,
         estatus                CHAR(2)      ,  
         f_pago                 CHAR(10)      ,
         monto                  DECIMAL(22,2),
         referencia             CHAR(16)     ,
         wt_withcd              CHAR(2)
      END RECORD 
DEFINE r_datos_beneficiario DYNAMIC ARRAY OF RECORD 
         v_ben_no_benefciario    SMALLINT,
         v_ben_porcentaje        SMALLINT,
         v_ben_nombre            CHAR(40),
         v_ben_paterno           CHAR(40),
         v_ben_materno           CHAR(40),
         v_ben_num_cta           CHAR(18),
         v_ben_banco             CHAR(40),
         v_ben_estatus_bloqueo   CHAR(2),
         v_ben_monto_bruto       DECIMAL(10,2),
         v_ben_pension           DECIMAL(10,2),
         v_ben_monto_pago        DECIMAL(10,2),
         v_ben_estatus_pago      CHAR(20),
         v_ben_diagnostico_pago  CHAR(40)   
END RECORD 
DEFINE arr_reg_montos_paso   DYNAMIC ARRAY OF RECORD
          v_subcuenta            CHAR(20)      ,
          v_acc_s1               DECIMAL(15,6) ,
          v_acc_s2               DECIMAL(15,6) ,
          v_acc_s3               DECIMAL(15,6) ,
          v_acc_s4               DECIMAL(15,6) ,
          v_acc_s90              DECIMAL(15,6) ,
          v_pesos_s1             DECIMAL(15,2) ,
          v_pesos_s2             DECIMAL(15,2) ,
          v_pesos_s3             DECIMAL(15,2) ,
          v_pesos_s4             DECIMAL(15,2) ,
          v_pesos_s90            DECIMAL(15,2) ,
          v_total_pesos          DECIMAL(15,2) ,
          v_isr                  DECIMAL(15,2) ,
          v_total_neto           DECIMAL(15,2)
       END RECORD 

END GLOBALS

PRIVATE DEFINE ventana     ui.Window
PRIVATE DEFINE forma       ui.Form

MAIN
    DEFINE p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
           p_s_titulo       STRING, -- titulo de la ventana
           lc_where         CHAR(200)

    DEFINE v_ruta_bitacora        CHAR(40)
    DEFINE v_archivo_log          STRING
    DEFINE v_programa             STRING
    DEFINE v_front                STRING
       
   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1me refiero 
    ---SE INCORPORA COMO PARAMETROS ENVIADOS DESDE EL MENU EL PROCESO Y CODIGO DE OPERACION
    LET g_usuario      = ARG_VAL(1)
    LET g_tipo_proceso = ARG_VAL(2)
    LET g_nom_ventana  = ARG_VAL(3)
    LET g_proceso_cod  = ARG_VAL(4)
    LET g_opera_cod    = ARG_VAL(5)

    LET v_programa     = "RETC454" 
    
   IF ( g_nom_ventana IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_nom_ventana)
   END IF
-- pendiente definición de pantallas que reciben parámetros adicionales

    LET gr_param.accion         = ARG_VAL(6)
    LET gr_param.nss            = ARG_VAL(7)
    LET gr_param.consecutivo    = ARG_VAL(8)
    

    SELECT ruta_bitacora
      INTO v_ruta_bitacora
      FROM seg_modulo
     WHERE modulo_cod = "ret"

    LET v_archivo_log = v_ruta_bitacora CLIPPED ,"/",g_usuario CLIPPED, ".",v_programa,".log"
    CALL STARTLOG(v_archivo_log)

    LET gr_edo.capturado     = 0;
    LET gr_edo.confirmado    = 3;
    LET gr_edo.enviado       = 4;
    LET gr_edo.provisionado  = 7;
    LET gr_edo.liquidado     = 8;
    LET gr_edo.preliquidado  = 10;
    LET gr_edo.autorizado    = 15;
    LET gr_edo.rechazado     = 20;
    LET gr_edo.enviado_op16  = 13;
    LET gr_edo.recibido_op16 = 14;
    
    --LET gc_usuario           = f_lib_obten_user()
    -- CATALOGOS PARCIALES --
    LET gr_prest.desempleo  = 6
    LET gr_prest.matrimonio = 7

    --CALL f_lib_abre_log("RETC001")

       
    --Parametros globales y usuario que ejecuta
    SELECT a.* 
    INTO   gr_rutas.*
    FROM   seg_modulo a
    WHERE  a.modulo_cod = "ret"

    DISPLAY "Ruta ", gr_rutas.ruta_listados
            -- Arma la consulta de los registros a verificar
    CALL fn_consulta_datos()

END MAIN

{ ============================================================================
Clave: XXXXXXX
Nombre: fn_consulta_datos
Fecha creacion: 19 abril, 2016
Registro de modificaciones:
Descrip: CONSULTA SOLICITUDES DE RETIRO
==============================================================================
}
PRIVATE FUNCTION fn_consulta_datos()
DEFINE 
       v_folio                  DECIMAL(11,0), -- folio
       v_nss                    CHAR(11),
       v_rfc                    CHAR(13),
       v_f_captura              DATE,
       v_estado_solicitud       SMALLINT,
       v_cod_rechazo            SMALLINT,
       v_entidad_fed            SMALLINT,
       v_caso_crm               CHAR(10),
       v_num_solicitud          DECIMAL(10,0),
       v_folio_restitucion      DECIMAL(10,0),
       v_ref_dap                CHAR(23),
       v_d_fch_sol_ini          DATE,
       v_d_fch_sol_fin          DATE,
       v_d_fch_pago_ini         DATE,
       v_d_fch_pago_fin         DATE,
       v_pos_combo              INTEGER,
       v_i                      INTEGER,
       v_reg_act                INTEGER,
       v_accion_todas           CHAR(15),
       v_edo_sol_todas          SMALLINT,
       v_cod_rechazo_todas      SMALLINT,
       v_des_edo_sol_todas      CHAR(50),
       v_des_cod_rech_todas     CHAR(50),
       i_todas                  INTEGER,
       v_msg                    STRING,
       v_i_estado_marca         SMALLINT,
       v_cero                   SMALLINT,
       v_nulo                   CHAR(5),
       v_ref_dap_exporta        CHAR(23),

       v_cbx_estado_solicitud   ui.ComboBox, -- combo de estado de solicitud
       v_cbx_cod_rechazo        ui.ComboBox, -- combo de códigos de rechazo
       v_cbx_entidad_fed        ui.ComboBox, -- combo de entidad federativa
       
       v_s_cadena               STRING, -- cadena de texto
       v_r_ret_estado           RECORD LIKE ret_estado_solicitud.*,
       v_r_ret_rechazo          RECORD LIKE ret_cat_rechazo_dap.*,
       v_r_ret_entidad_fed      RECORD LIKE cat_entidad_federativa.*,
       v_resultado              SMALLINT,
       v_tipo_diferencia        CHAR(25),
       v_encontrado_oficios_rojos SMALLINT,
       v_pago_credito           SMALLINT 
 
       
DEFINE arr_reg_datos       DYNAMIC ARRAY OF RECORD
         v_id_solicitud         DECIMAL(10,0) ,
         v_nss                  CHAR(11)      ,
         v_f_solicitud          DATE          ,
         v_f_autoriza           DATE          ,
         v_f_pago               DATE          ,
         v_imp_sol              DECIMAL(16,2) ,
         v_edo_sol              SMALLINT      ,
         v_estado_sol           CHAR(50)      ,
         v_cod_rechazo          SMALLINT      ,
         v_codigo_rechazo       CHAR(50)      ,
         v_tipo_producto        CHAR(20)      ,
         v_tipo_solicitud       CHAR(20)      ,
         v_folio                DECIMAL(10,0) ,
         v_id_derechohabiente   DECIMAL(10,0) ,
         v_referencia_dap       CHAR(23)
      END RECORD

DEFINE arr_reg_datos_orig  DYNAMIC ARRAY OF RECORD 
         v_id_solicitud         DECIMAL(10,0) ,
         v_edo_sol              SMALLINT      ,
         v_cod_rechazo          SMALLINT      
      END RECORD 
         
DEFINE arr_reg_datos_comp       DYNAMIC ARRAY OF RECORD
         v_curp                 CHAR(18)      ,
         v_nss                  CHAR(11)      ,
         v_consec               DECIMAL(12,0) ,
         v_nombre               CHAR(100)     , 
         v_ret                  CHAR(25)      ,
         v_fch_captura          DATE          ,
         v_fch_liquida          DATE          ,
         v_folio                DECIMAL(11,0)  ,
         v_edo_sol              SMALLINT      ,
         v_estado_sol           CHAR(50)      ,
         v_cod_rechazo          SMALLINT      ,
         v_codigo_rechazo       CHAR(50)      ,
         v_modalidad            CHAR(20)      
       END RECORD,

       arr_datos_solicitud  RECORD 
            v_ed_entidad               CHAR(30)         ,
            v_ed_juicio                CHAR(10)         ,
            v_ed_num_acuerdo           CHAR(10)         ,
            v_ed_desc_juez             CHAR(40)         ,
            v_ed_facultado             CHAR(50)         ,
            v_ed_puesto                CHAR(50)         ,
            v_ed_fch_ejecucion         CHAR(10)         ,
            v_ed_procede_juicio        CHAR(40)         
   END RECORD,        
      arr_datos_liquidacion  RECORD 
            v_ed_tesofe               DECIMAL (16,2) ,
            v_ed_aivs_97              DECIMAL (16,2) ,
            v_ed_pesos_97             DECIMAL (16,2) ,
            v_ed_ajustes_aivs_97      DECIMAL (16,2) ,
            v_ed_ajustes_pesos_97     DECIMAL (16,2) ,
            v_ed_aivs_92              DECIMAL (16,2) ,
            v_ed_pesos_92             DECIMAL (16,2) ,
            v_ed_ajustes_aivs_92      DECIMAL (16,2) ,
            v_ed_ajustes_pesos_92     DECIMAL (16,2) ,
            v_ed_importe_cuenta       DECIMAL (16,2) ,
            v_ed_importe_sol          DECIMAL (16,2) ,
            v_ed_importe_dif          DECIMAL (16,2) ,
            v_ed_tipo_diferencia      CHAR    (25)
   END RECORD,       
      arr_detalle_pago  RECORD 
            ed_teso_envio                  VARCHAR(80) ,
            ed_teso_respuesta              VARCHAR(80) ,
            ed_teso_fch_envio              CHAR(10)    ,
            ed_teso_fch_respuesta          CHAR(10)    ,
            ed_teso_codigo_respuesta       VARCHAR(80) ,
            ed_pago_fch_consulta           CHAR(10)    ,
            ed_pago_fch_pago               CHAR(10)    ,
            ed_pago_respuesta              VARCHAR(40) ,
            ed_titular_nombre              VARCHAR(60) ,
            ed_titular_folio_liquida       CHAR(10)    ,
            ed_titular_rfc                 CHAR(13)    ,
            ed_titular_folio_restitucion   CHAR(10)    ,
            ed_benef_nombre                VARCHAR(60)
   END RECORD,  
       arr_datos_credito  RECORD 
            v_ed_tipo_originacion     CHAR(20)          ,
            v_ed_num_credito          CHAR(20)          ,
            v_ed_fch_otorgamiento     CHAR(10)          ,
            v_ed_tipo_credito         SMALLINT          ,
            v_ed_estado_credito       CHAR(20)          ,
            v_ed_fch_liquidacion      CHAR(10)          ,
            v_ed_tipo_descuento       SMALLINT          
    END RECORD,        
       arr_datos_oficios_rojos  RECORD 
            ed_imp_pago_of_rojos     DECIMAL(16,2)          ,
            ed_fch_pago_of_rojos     CHAR(10)                  
    END RECORD,        
    
       v_query                  STRING, -- detalle
       v_indice                 DECIMAL(9,0), -- indice de arreglo       
       v_ruta_reporte           STRING ,-- ruta del archivo del reporte       
       v_ruta_listados          STRING ,-- ruta de los listados
       v_ruta_ejecutable        STRING ,-- ruta del ejecutable
       manejador_rpt            om.SaxDocumentHandler ,
       v_indice_reporte         SMALLINT,
       v_id_solicitud           DECIMAL(9,0),
       v_usuario_liquida        CHAR(20),
       v_fecha_pago             DATE,
       v_fecha_valuacion        DATE 

    DEFINE v_posicion            INTEGER
    DEFINE v_marca_excep_dev_ssv SMALLINT 
    
    DEFINE v_string base.StringBuffer
    DEFINE v_where  STRING
       
    OPEN WINDOW w_consulta_datos WITH FORM "RETC4541"
    LET ventana = ui.Window.getCurrent()
    LET forma   = ventana.getForm()
    LET v_reg_act = 0

    -- se le asigna el apuntado del combo a la variable
    LET v_cbx_estado_solicitud = ui.ComboBox.forName("formonly.cmb_estado_solicitud")
    LET v_cbx_cod_rechazo      = ui.ComboBox.forName("formonly.cmb_rechazo")
    LET v_cbx_entidad_fed      = ui.ComboBox.forName("formonly.cmb_entidad_fed")
    -- se ocultan los grupos de la consulta 
    CALL forma.setElementHidden("grp_detalle",1)
    CALL forma.setElementHidden("fol_detalle_sol",1)
--    CALL forma.setFieldHidden("btn_rechazar_todas",1)
--    CALL forma.setFieldHidden("btn_confirmar_todas",1)


    LET v_marca_excep_dev_ssv = 820;
    LET v_nulo                = NULL;

--    CALL forma.setElementHidden("grp_datos_tra",1)
--    CALL forma.setElementHidden("grp_datos_solicitud",1)
--    CALL forma.setElementHidden("grp_datos_captura",1)

    
    -- se inician los combobox en blanco
    
    CALL v_cbx_estado_solicitud.clear()
    CALL v_cbx_cod_rechazo.clear()
    CALL v_cbx_entidad_fed.clear()
    INPUT   v_folio,
            v_nss,
            v_caso_crm,
            v_num_solicitud,
            v_folio_restitucion,
            v_ref_dap,
            v_estado_solicitud,
            v_cod_rechazo,
            v_entidad_fed,
            v_d_fch_sol_ini,
            v_d_fch_sol_fin,
            v_d_fch_pago_ini,
            v_d_fch_pago_fin
    FROM  ed_folio,
          ed_nss,
          ed_caso_crm,
          ed_num_solicitud,
          ed_folio_restitucion,
          ed_ref_dap,
          cmb_estado_solicitud ,
          cmb_rechazo,
          cmb_entidad_fed,
          ed_d_fch_sol_ini,
          ed_d_fch_sol_fin,
          ed_d_fch_pago_ini,
          ed_d_fch_pago_fin
    ATTRIBUTES (UNBUFFERED, WITHOUT DEFAULTS)

    BEFORE INPUT
        -- se limpian las variables
        LET v_folio              = NULL 
        LET v_nss                = NULL
        LET v_caso_crm           = NULL
        LET v_num_solicitud      = NULL
        LET v_folio_restitucion  = NULL
        LET v_ref_dap            = NULL
        LET v_cod_rechazo        = NULL
        LET v_entidad_fed        = NULL
        LET v_d_fch_sol_ini      = NULL
        LET v_d_fch_sol_fin      = NULL
        LET v_d_fch_pago_ini     = NULL
        LET v_d_fch_pago_fin     = NULL

        LET v_f_captura          = NULL
        LET v_estado_solicitud   = NULL
        LET v_accion_todas       = "confirmar_todas"
        LET v_edo_sol_todas      = 0
        LET v_des_edo_sol_todas  = NULL
        LET v_cod_rechazo_todas  = 0
        LET v_des_cod_rech_todas = NULL
        LET i_todas              = 0
        -- se llena el arreglo de los estados de solicitud
        DECLARE cur_estado_solicitud CURSOR FOR
        SELECT A.estado_solicitud ,              
               A.des_corta
        FROM   ret_estado_solicitud A
        WHERE  estado_solicitud IN (10,15,50,60,70,74,77,71,90,100,200,209,210,211,700)
        ORDER BY 2;


        FOREACH cur_estado_solicitud INTO v_r_ret_estado.estado_solicitud, v_r_ret_estado.des_corta
           LET v_s_cadena = v_r_ret_estado.estado_solicitud, " - ", v_r_ret_estado.des_corta
           CALL v_cbx_estado_solicitud.addItem(v_r_ret_estado.estado_solicitud, v_s_cadena)
        END FOREACH

        FREE cur_estado_solicitud

        -- se llena el arreglo de los codigos de rechazo
        DECLARE cur_codigos_rechazo CURSOR FOR
        SELECT A.cod_rechazo ,              
               A.descripcion
        FROM   ret_cat_rechazo_dap A
        WHERE  A.pantalla = 1
        ORDER BY 2;


        FOREACH cur_codigos_rechazo INTO v_r_ret_rechazo.cod_rechazo, v_r_ret_rechazo.descripcion
           LET v_s_cadena = v_r_ret_rechazo.cod_rechazo, " - ", v_r_ret_rechazo.descripcion
           CALL v_cbx_cod_rechazo.addItem(v_r_ret_rechazo.cod_rechazo, v_s_cadena)
        END FOREACH

        FREE cur_codigos_rechazo
        
        -- se llena el arreglo de los codigos de rechazo
        DECLARE cur_entidad_fed CURSOR FOR
        SELECT DISTINCT A.entidad_federativa ,              
               A.entidad_desc_larga
        FROM   cat_entidad_federativa A
        WHERE  entidad_federativa BETWEEN 1 AND 32 
        ORDER BY 2;


        FOREACH cur_entidad_fed INTO v_r_ret_entidad_fed.entidad_federativa, v_r_ret_entidad_fed.entidad_desc_larga
           LET v_s_cadena = v_r_ret_entidad_fed.entidad_federativa, " - ", v_r_ret_entidad_fed.entidad_desc_larga
           CALL v_cbx_entidad_fed.addItem(v_r_ret_entidad_fed.entidad_federativa, v_s_cadena)
        END FOREACH

        FREE cur_entidad_fed

        ON ACTION ACCEPT

            --modificación de validación de  captura de parametros
            --valida que se ingrese al menos un parametro
            CALL arr_reg_datos.clear()
            IF (v_folio             IS NULL OR 
                v_folio             <= 0)   AND 
                v_nss               IS NULL AND
                v_estado_solicitud  IS NULL AND
                v_caso_crm          IS NULL AND
                v_num_solicitud     IS NULL AND
                v_folio_restitucion IS NULL AND
                v_ref_dap           IS NULL AND 
                v_cod_rechazo       IS NULL AND 
                v_entidad_fed       IS NULL AND 
                v_d_fch_sol_ini     IS NULL AND 
                v_d_fch_sol_fin     IS NULL AND 
                v_d_fch_pago_ini    IS NULL AND 
                v_d_fch_pago_fin    IS NULL
                THEN
                --CALL f_lib_error_msg("Debe de ingresar al menos un criterio de búsqueda")
                CALL fn_mensaje("","Debe de ingresar al menos un criterio de búsqueda","")
                --CALL fn_mensaje("Consulta","Debe de ingresar al menos un criterio de búsqueda","about")
                CONTINUE INPUT
            ELSE 
               IF v_d_fch_sol_ini IS  NOT NULL AND v_d_fch_sol_fin IS NOT NULL THEN 
                  IF v_d_fch_sol_ini > v_d_fch_sol_fin THEN
                     CALL fn_mensaje("","La fecha de solicitud inicial no puede ser mayor a la fecha de solicitud final","")
                     LET v_d_fch_sol_ini = NULL
                     NEXT FIELD ed_d_fch_sol_ini
                     CONTINUE INPUT
                  END IF 
               END IF 
               IF v_d_fch_pago_ini IS  NOT NULL AND v_d_fch_pago_fin IS NOT NULL THEN 
                  IF v_d_fch_pago_ini > v_d_fch_pago_fin THEN
                     CALL fn_mensaje("","La fecha de pago inicial no puede ser mayor a la fecha de pago final","")
                     LET v_d_fch_pago_ini = NULL
                     NEXT FIELD ed_d_fch_pago_ini
                     CONTINUE INPUT
                  END IF 
               END IF 
               LET v_query = " EXECUTE FUNCTION fn_cal_habil_siguiente(today) "
               PREPARE prp_fecha_pago FROM v_query
               EXECUTE prp_fecha_pago 
                               INTO v_fecha_pago
               FREE prp_fecha_pago 
               LET v_fecha_valuacion = MDY(MONTH(v_fecha_pago),1,YEAR(v_fecha_pago))
                 -- se limpia el arreglo de despligue del detalle
                 CALL arr_reg_datos.clear()
                 -- se consulta del detalle de este agrupador
                LET gc_rpt_where  = "\n"
                IF (v_folio IS NOT NULL) THEN
                    LET gc_rpt_where = gc_rpt_where CLIPPED || " AND a.folio = " || v_folio
                END IF
                IF (v_nss IS NOT NULL) THEN
                    LET gc_rpt_where = gc_rpt_where CLIPPED || " AND a.nss = '" || v_nss || "' "
                END IF
                IF (v_ref_dap IS NOT NULL) THEN 
                    LET gc_rpt_where = gc_rpt_where CLIPPED || " AND a.nss||TO_CHAR(a.id_solicitud,'&&&&&&&&&&&&') = '" || v_ref_dap || "' "
                END IF 
                IF (v_folio_restitucion IS NOT NULL) THEN 
                    LET gc_rpt_where = gc_rpt_where CLIPPED || " AND a.folio_restitucion = '" || v_folio_restitucion || "' "
                END IF 
                IF (v_estado_solicitud IS NOT NULL) THEN
                    LET gc_rpt_where = gc_rpt_where CLIPPED || " AND a.estado_solicitud = " || v_estado_solicitud
                END IF
                IF (v_num_solicitud IS NOT NULL) THEN
                    LET gc_rpt_where = gc_rpt_where CLIPPED || " AND a.id_solicitud = " || v_num_solicitud
                END IF
                IF (v_cod_rechazo IS NOT NULL) THEN
                    LET gc_rpt_where = gc_rpt_where CLIPPED || " AND a.cod_rechazo = " || v_cod_rechazo
                END IF
                IF (v_entidad_fed IS NOT NULL) THEN
                    LET gc_rpt_where = gc_rpt_where CLIPPED || " AND a.entidad = " || v_entidad_fed
                END IF
                IF (v_d_fch_sol_ini IS NOT NULL AND v_d_fch_sol_fin IS NOT NULL ) THEN
                    LET gc_rpt_where = gc_rpt_where CLIPPED || " AND a.f_solicitud BETWEEN '" || v_d_fch_sol_ini ||
                                                               "' AND '" || v_d_fch_sol_fin || "'" 
                END IF
                IF (v_d_fch_pago_ini IS NOT NULL AND v_d_fch_pago_fin IS NOT NULL ) THEN
                    LET gc_rpt_where = gc_rpt_where CLIPPED || " AND a.f_pago BETWEEN '" || v_d_fch_pago_ini || 
                                                               "' AND '" || v_d_fch_pago_fin || "'"
                END IF

                DISPLAY "La clausula WHERE ", gc_rpt_where 

                LET v_query    =   " SELECT  a.id_solicitud, a.nss, a.f_solicitud,                                \n ",
                                   "         a.f_autoriza, a.f_pago, NVL(a.importe,a.importe_cuenta),             \n ",          
                                   "         a.estado_solicitud, b.des_corta, a.cod_rechazo, c.descripcion,       \n ",
                                   "         e.descripcion, d.descripcion, a.folio, f.id_derechohabiente          \n ",
                                   "   FROM  ret_estado_solicitud b,ret_excep_devol_ssv a                         \n ",
                                   "         LEFT OUTER JOIN ret_cat_rechazo_dap c                                \n ",
                                   "                      ON a.cod_rechazo = c.cod_rechazo                        \n ",
                                   "         LEFT OUTER JOIN ret_cat_tpo_sol_dap d                                \n ",
                                   "                      ON a.tipo_sol = d.tpo_solicitud                         \n ",
                                   "         LEFT OUTER JOIN ret_cat_tpo_prod_dap e                               \n ",
                                   "                      ON a.tipo_prod = e.tpo_producto                         \n ",
                                   "         LEFT OUTER JOIN afi_derechohabiente f                                \n ",
                                   "                      ON a.nss = f.nss                                        \n ",
                                   "  WHERE  a.estado_solicitud = b.estado_solicitud                              \n ",
                                    gc_rpt_where CLIPPED , " \n"

                DISPLAY ">",v_query,"<"
                PREPARE sid_detalle FROM v_query
                DECLARE cur_detalle  CURSOR FOR sid_detalle
                CALL f_genera_datos(gc_rpt_where)


                --llena el arreglo        
                LET v_indice = 1
                CALL arr_reg_datos.clear()
                CALL arr_reg_datos_comp.clear()
                --- Crea la temporal para la exportacion de datos
                DROP TABLE IF EXISTS tmp_exporta_excepciones_ssv;
                CREATE TEMP TABLE tmp_exporta_excepciones_ssv (
                                 id_solicitud         DECIMAL(10,0) ,
                                 nss                  CHAR(11)      ,
                                 f_solicitud          DATE          ,
                                 f_autoriza           DATE          ,
                                 f_pago               DATE          ,
                                 imp_sol              DECIMAL(16,2) ,
                                 edo_sol              SMALLINT      ,
                                 estado_sol           CHAR(50)      ,
                                 cod_rechazo          SMALLINT      ,
                                 codigo_rechazo       CHAR(50)      ,
                                 tipo_producto        CHAR(20)      ,
                                 tipo_solicitud       CHAR(20)      ,
                                 referencia_dap       CHAR(23));
                FOREACH cur_detalle INTO 
                    arr_reg_datos[v_indice].v_id_solicitud   ,
                    arr_reg_datos[v_indice].v_nss            ,
                    arr_reg_datos[v_indice].v_f_solicitud    ,
                    arr_reg_datos[v_indice].v_f_autoriza     ,
                    arr_reg_datos[v_indice].v_f_pago         ,
                    arr_reg_datos[v_indice].v_imp_sol        ,
                    arr_reg_datos[v_indice].v_edo_sol        ,
                    arr_reg_datos[v_indice].v_estado_sol     ,
                    arr_reg_datos[v_indice].v_cod_rechazo    ,
                    arr_reg_datos[v_indice].v_codigo_rechazo ,
                    arr_reg_datos[v_indice].v_tipo_producto  ,
                    arr_reg_datos[v_indice].v_tipo_solicitud ,
                    arr_reg_datos[v_indice].v_folio          ,
                    arr_reg_datos[v_indice].v_id_derechohabiente    
                    LET arr_reg_datos[v_indice].v_referencia_dap = arr_reg_datos[v_indice].v_nss CLIPPED ,arr_reg_datos[v_indice].v_id_solicitud USING "&&&&&&&&&&&&"
                    DISPLAY "El resultado de la consulta :>", arr_reg_datos[v_indice].*, "<"
                    LET arr_reg_datos_orig[v_indice].v_id_solicitud = arr_reg_datos[v_indice].v_id_solicitud
                    LET arr_reg_datos_orig[v_indice].v_edo_sol      = arr_reg_datos[v_indice].v_edo_sol
                    LET arr_reg_datos_orig[v_indice].v_cod_rechazo  = arr_reg_datos[v_indice].v_cod_rechazo
                    IF arr_reg_datos[v_indice].v_imp_sol IS NULL THEN 
                       LET arr_reg_datos[v_indice].v_imp_sol = 0
                    END IF 
--                    IF arr_reg_datos[v_indice].v_imp_sol = 0 OR 
--                       arr_reg_datos[v_indice].v_imp_sol IS NULL THEN 
                       -- Busca el saldo 
--                       CALL fn_busca_saldo_cuenta(arr_reg_datos[v_indice].v_id_derechohabiente, v_fecha_valuacion)
--                                                  RETURNING arr_reg_datos[v_indice].v_imp_devol
--                       DISPLAY "El saldo a devolver es ", arr_reg_datos[v_indice].v_imp_devol, " del nss ", arr_reg_datos[v_indice].v_nss
--                    ELSE 
--                       LET arr_reg_datos[v_indice].v_imp_devol = arr_reg_datos[v_indice].v_imp_sol
--                    END IF 
--                    LET arr_reg_datos[v_indice].v_imp_dif = arr_reg_datos[v_indice].v_imp_devol - arr_reg_datos[v_indice].v_imp_sol
                    INSERT INTO tmp_exporta_excepciones_ssv (
                                 id_solicitud    ,nss            ,f_solicitud     ,f_autoriza     ,
                                 f_pago          ,imp_sol        ,edo_sol         ,estado_sol     ,
                                 cod_rechazo     ,codigo_rechazo ,tipo_producto   ,tipo_solicitud ,
                                 referencia_dap)  
                        VALUES ( arr_reg_datos[v_indice].v_id_solicitud,
                                 arr_reg_datos[v_indice].v_nss,
                                 arr_reg_datos[v_indice].v_f_solicitud,
                                 arr_reg_datos[v_indice].v_f_autoriza,
                                 arr_reg_datos[v_indice].v_f_pago,
                                 arr_reg_datos[v_indice].v_imp_sol,
                                 arr_reg_datos[v_indice].v_edo_sol,
                                 arr_reg_datos[v_indice].v_estado_sol,
                                 arr_reg_datos[v_indice].v_cod_rechazo,
                                 arr_reg_datos[v_indice].v_codigo_rechazo,
                                 arr_reg_datos[v_indice].v_tipo_producto,
                                 arr_reg_datos[v_indice].v_tipo_solicitud,
                                 arr_reg_datos[v_indice].v_referencia_dap)        
                    LET v_indice = v_indice + 1
                END FOREACH
                CALL arr_reg_datos.deleteElement(arr_reg_datos.getLength())
                CALL arr_reg_datos_comp.deleteElement(arr_reg_datos_comp.getLength())

                LET v_indice = v_indice - 1
                IF v_indice = 0 THEN
                    CALL fn_mensaje("","Los parámetros de consulta no arrojaron ningún resultado","")
                ELSE 

                    CALL forma.setElementHidden("grp_detalle",0)
--                    CALL forma.setElementHidden("fol_detalle_sol",0)
                    
                    DIALOG ATTRIBUTES  (UNBUFFERED)
                    
                    DISPLAY  ARRAY arr_reg_marcas_activas TO tbl_m_act.* --ATTRIBUTE (ACCEPT=FALSE, CANCEL=FALSE)
                    END DISPLAY 
                    DISPLAY  ARRAY arr_reg_marcas_historicas TO tbl_m_his.* --ATTRIBUTE (ACCEPT=FALSE, CANCEL=FALSE)
                    END DISPLAY 
                    DISPLAY ARRAY arr_reg_movtos_97 TO tbl_viv_97.* --ATTRIBUTE (ACCEPT=FALSE, CANCEL=FALSE)
                    END DISPLAY 
                    DISPLAY ARRAY arr_reg_movtos_92 TO tbl_viv_92.* --ATTRIBUTE (ACCEPT=FALSE, CANCEL=FALSE)
                    END DISPLAY 
                    DISPLAY ARRAY arr_reg_pagos_previos TO tbl_pagos_previos.* --ATTRIBUTE (ACCEPT=FALSE, CANCEL=FALSE)
                    END DISPLAY 

                    
                    INPUT   ARRAY arr_reg_datos FROM rec_datos.* 
                            ATTRIBUTES ( WITHOUT DEFAULTS ,APPEND ROW = FALSE, 
                            DELETE ROW = FALSE , INSERT ROW = FALSE ,KEEP CURRENT ROW = FALSE)


                        BEFORE INPUT 
                           LET v_posicion = 1
                                INITIALIZE arr_datos_solicitud.* TO NULL 
                                INITIALIZE arr_datos_liquidacion.* TO NULL 
                                INITIALIZE arr_datos_credito.* TO NULL 
                                INITIALIZE arr_datos_oficios_rojos.* TO NULL 
                                CALL fn_consulta_trabajador(arr_reg_datos[v_posicion].v_id_solicitud) RETURNING arr_datos_solicitud.*
                                DISPLAY "El importe de la solicitud > ", arr_reg_datos[v_posicion].v_imp_sol, "<"
                                CALL fn_consulta_detalle_liquida(arr_reg_datos[v_posicion].v_id_derechohabiente, v_fecha_valuacion,
                                                                 arr_reg_datos[v_posicion].v_imp_sol, arr_reg_datos[v_posicion].v_edo_sol) 
                                                                 RETURNING arr_datos_liquidacion.*
                                DISPLAY "El solicitado después de buscar los montos > ", arr_datos_liquidacion.v_ed_importe_sol
                                CALL fn_consulta_credito(arr_reg_datos[v_posicion].v_id_derechohabiente) RETURNING arr_datos_credito.*
                                CALL fn_consulta_oficios_rojos(arr_reg_datos[v_posicion].v_nss) RETURNING arr_datos_oficios_rojos.*
                                CALL arr_reg_marcas_activas.clear()
                                CALL arr_reg_marcas_historicas.clear()
                                CALL arr_reg_movtos_97.clear()
                                CALL arr_reg_movtos_92.clear()
                                CALL arr_reg_pagos_previos.clear()
                                CALL fn_busca_marcas_activas(arr_reg_datos[v_posicion].v_id_derechohabiente) RETURNING v_resultado
                                CALL fn_busca_movtos(arr_reg_datos[v_posicion].v_id_derechohabiente) RETURNING v_resultado
                                CALL fn_busca_pagos_previos(arr_reg_datos[v_posicion].v_id_solicitud) RETURNING v_resultado 
                                LET  v_tipo_diferencia = NULL  
                                LET  v_encontrado_oficios_rojos = 0
                                IF  arr_datos_oficios_rojos.ed_fch_pago_of_rojos IS NOT NULL THEN 
                                   LET  v_encontrado_oficios_rojos = 1
                                END IF 
                                LET  v_pago_credito = 0
                                IF arr_datos_credito.v_ed_num_credito IS NOT NULL AND 
                                   arr_datos_credito.v_ed_estado_credito = "CRÉDITO VIGENTE" THEN 
                                   LET  v_pago_credito = 1
                                END IF 
                                CALL fn_determina_tipo_diferencia(arr_reg_datos[v_posicion].v_nss,
                                                                  arr_reg_datos[v_posicion].v_id_derechohabiente,
                                                                  arr_reg_datos[v_posicion].v_tipo_producto, 
                                                                  v_encontrado_oficios_rojos,
                                                                  v_pago_credito,
                                                                  v_resultado) RETURNING v_tipo_diferencia
                                LET arr_datos_liquidacion.v_ed_tipo_diferencia = v_tipo_diferencia
                                IF arr_reg_datos[v_posicion].v_edo_sol <> 8 AND 
                                   arr_reg_datos[v_posicion].v_edo_sol <> 10 AND 
                                   arr_reg_datos[v_posicion].v_edo_sol <> 15 AND 
                                   arr_reg_datos[v_posicion].v_edo_sol <> 50 AND
                                   arr_reg_datos[v_posicion].v_edo_sol <> 100 THEN
                                   CALL fn_busca_datos_liq_previos(arr_reg_datos[v_posicion].v_id_solicitud,arr_datos_liquidacion.*) RETURNING arr_datos_liquidacion.* 
                                END IF 
                                CALL fn_consulta_pago(arr_reg_datos[v_posicion].v_id_solicitud) RETURNING arr_detalle_pago.*
                                DISPLAY BY NAME arr_datos_solicitud.*
                                DISPLAY BY NAME arr_datos_liquidacion.*
                                DISPLAY BY NAME arr_datos_credito.*
                                DISPLAY BY NAME arr_datos_oficios_rojos.*
                                DISPLAY BY NAME arr_detalle_pago.*
                                CALL forma.setElementHidden("fol_detalle_sol",0)



                        BEFORE ROW
                            LET v_posicion = ARR_CURR()
                            IF v_posicion > 0 THEN --AND arr_curr() <= arr_count() - 1 THEN
                                INITIALIZE arr_datos_solicitud.* TO NULL 
                                INITIALIZE arr_datos_liquidacion.* TO NULL 
                                INITIALIZE arr_datos_credito.* TO NULL 
                                INITIALIZE arr_datos_oficios_rojos.* TO NULL 
                                CALL fn_consulta_trabajador(arr_reg_datos[v_posicion].v_id_solicitud) RETURNING arr_datos_solicitud.*
                                DISPLAY "El importe de la solicitud > ", arr_reg_datos[v_posicion].v_imp_sol, "<"
                                CALL fn_consulta_detalle_liquida(arr_reg_datos[v_posicion].v_id_derechohabiente, v_fecha_valuacion,
                                                                 arr_reg_datos[v_posicion].v_imp_sol,arr_reg_datos[v_posicion].v_edo_sol)
                                                                 RETURNING arr_datos_liquidacion.*
                                DISPLAY "El solicitado después de buscar los montos > ", arr_datos_liquidacion.v_ed_importe_sol
                                CALL fn_consulta_credito(arr_reg_datos[v_posicion].v_id_derechohabiente) RETURNING arr_datos_credito.*
                                CALL fn_consulta_oficios_rojos(arr_reg_datos[v_posicion].v_nss) RETURNING arr_datos_oficios_rojos.*
                                CALL arr_reg_marcas_activas.clear()
                                CALL arr_reg_marcas_historicas.clear()
                                CALL arr_reg_movtos_97.clear()
                                CALL arr_reg_movtos_92.clear()
                                CALL arr_reg_pagos_previos.clear()
                                CALL fn_busca_marcas_activas(arr_reg_datos[v_posicion].v_id_derechohabiente) RETURNING v_resultado
                                CALL fn_busca_movtos(arr_reg_datos[v_posicion].v_id_derechohabiente) RETURNING v_resultado
                                CALL fn_busca_pagos_previos(arr_reg_datos[v_posicion].v_id_solicitud) RETURNING v_resultado 
                                LET  v_tipo_diferencia = NULL  
                                LET  v_encontrado_oficios_rojos = 0
                                IF  arr_datos_oficios_rojos.ed_fch_pago_of_rojos IS NOT NULL THEN 
                                   LET  v_encontrado_oficios_rojos = 1
                                END IF 
                                LET  v_pago_credito = 0
                                IF arr_datos_credito.v_ed_num_credito IS NOT NULL THEN 
                                   LET  v_pago_credito = 1
                                END IF 
                                CALL fn_determina_tipo_diferencia(arr_reg_datos[v_posicion].v_nss,
                                                                  arr_reg_datos[v_posicion].v_id_derechohabiente,
                                                                  arr_reg_datos[v_posicion].v_tipo_producto, 
                                                                  v_encontrado_oficios_rojos,
                                                                  v_pago_credito,
                                                                  v_resultado) RETURNING v_tipo_diferencia
                                LET arr_datos_liquidacion.v_ed_tipo_diferencia = v_tipo_diferencia
                                IF arr_reg_datos[v_posicion].v_edo_sol <> 8 AND 
                                   arr_reg_datos[v_posicion].v_edo_sol <> 10 AND 
                                   arr_reg_datos[v_posicion].v_edo_sol <> 15 AND 
                                   arr_reg_datos[v_posicion].v_edo_sol <> 50 AND
                                   arr_reg_datos[v_posicion].v_edo_sol <> 100 THEN
                                   CALL fn_busca_datos_liq_previos(arr_reg_datos[v_posicion].v_id_solicitud,arr_datos_liquidacion.*) RETURNING arr_datos_liquidacion.* 
                                END IF 
                                CALL fn_consulta_pago(arr_reg_datos[v_posicion].v_id_solicitud) RETURNING arr_detalle_pago.*
                                DISPLAY BY NAME arr_datos_solicitud.*
                                DISPLAY BY NAME arr_datos_liquidacion.*
                                DISPLAY BY NAME arr_datos_credito.*
                                DISPLAY BY NAME arr_datos_oficios_rojos.*
                                DISPLAY BY NAME arr_detalle_pago.*
                            END IF 
                        END INPUT 
                    ON ACTION exportar
                     CALL fn_exporta_archivo();
                    ON ACTION CANCELAR
                        CALL forma.setElementHidden("fol_detalle_sol",1)
                        CALL forma.setElementHidden("grp_detalle",1)
                        CALL forma.setElementHidden("grp_datos_tra",1)
                        CALL forma.setElementHidden("grp_datos_solicitud",1)
                        CALL forma.setElementHidden("grp_datos_captura",1)
                        CALL forma.setFieldHidden("btn_rechazar_todas",1)
                        CALL forma.setFieldHidden("btn_confirmar_todas",1)

                        EXIT DIALOG   
                    END DIALOG       
                END IF 

            END IF 
         
      ON ACTION CANCEL
         EXIT INPUT
   
   END INPUT
   
   CLOSE WINDOW w_consulta_datos

END FUNCTION

#---------------------------------------------------------------------------#
# f_consulta_juridico : Obtiene y muestra los datos de la consulta de datos #
#                       del Archivo (Juridico)                              #
#---------------------------------------------------------------------------#
PRIVATE FUNCTION fn_consulta_trabajador(p_id_solicitud) 

DEFINE 
   p_id_solicitud       DECIMAL(10,0)         ,
   p_folio              INTEGER          
DEFINE  arr_detalle  RECORD 
         ed_entidad               CHAR(40)         ,
         ed_juicio                CHAR(10)         ,
         ed_num_acuerdo           CHAR(10)         ,
         ed_desc_juez             CHAR(40)         ,
         ed_facultado             CHAR(50)         ,
         ed_puesto                CHAR(50)         ,
         ed_fch_ejecucion         CHAR(10)         ,
         ed_procede_juicio        CHAR(40)         
      END RECORD 
DEFINE  v_fecha_paso             DATE 
DEFINE lar_despliega_par RECORD    
         nss                 LIKE afi_derechohabiente.nss            ,
         curp                LIKE afi_derechohabiente.curp           ,
         rfc                 LIKE afi_derechohabiente.rfc            ,
         paterno             LIKE afi_derechohabiente.ap_paterno_af  ,
         materno             LIKE afi_derechohabiente.ap_materno_af  ,
         nombre              LIKE afi_derechohabiente.nombre_af      
      END RECORD
  
DEFINE
   li_elem             SMALLINT
        
   DISPLAY "Parámetros recibidos :"
   DISPLAY "Id_solicitud:" || p_id_solicitud

   SELECT  b.entidad_desc_larga,
           a.juicio            ,
           a.num_acuerdo       ,
           a.desc_juez         ,
           a.facultado         ,
           a.puesto            ,
           a.fch_ejecuta       ,
           a.procede_juicio
   INTO    arr_detalle.ed_entidad,
           arr_detalle.ed_juicio,
           arr_detalle.ed_num_acuerdo      ,
           arr_detalle.ed_desc_juez   ,
           arr_detalle.ed_facultado  ,
           arr_detalle.ed_puesto,
           v_fecha_paso,
           arr_detalle.ed_procede_juicio
   FROM    ret_excep_devol_ssv a 
           LEFT OUTER JOIN cat_entidad_federativa b
                        ON a.entidad = b.entidad_federativa
   WHERE   id_solicitud  = p_id_solicitud

   LET arr_detalle.ed_fch_ejecucion = v_fecha_paso USING "dd/mm/yyyy"
   DISPLAY "Los valores a mostrar en la pantalla son:"
   DISPLAY "ed_entidad             :" || arr_detalle.ed_entidad
   DISPLAY "ed_juicio              :" || arr_detalle.ed_juicio
   DISPLAY "ed_num_acuerdo         :" || arr_detalle.ed_num_acuerdo
   DISPLAY "ed_desc_juez           :" || arr_detalle.ed_desc_juez
   DISPLAY "ed_facultado           :" || arr_detalle.ed_facultado
   DISPLAY "ed_puesto              :" || arr_detalle.ed_puesto
   DISPLAY "ed_fch_ejecucion       :" || arr_detalle.ed_fch_ejecucion
   DISPLAY "ed_procede_juicio      :" || arr_detalle.ed_procede_juicio
        

   -- limpia campos
   DISPLAY "" TO v_ed_entidad
   DISPLAY "" TO v_ed_juicio
   DISPLAY "" TO v_ed_num_acuerdo
   DISPLAY "" TO v_ed_desc_juez
   DISPLAY "" TO v_ed_facultado
   DISPLAY "" TO v_ed_puesto
   DISPLAY "" TO v_ed_fch_ejecucion
   DISPLAY "" TO v_ed_procede_juicio
      
DISPLAY arr_detalle.ed_entidad              TO v_ed_entidad
DISPLAY arr_detalle.ed_juicio               TO v_ed_juicio
DISPLAY arr_detalle.ed_num_acuerdo          TO v_ed_num_acuerdo
DISPLAY arr_detalle.ed_desc_juez            TO v_ed_desc_juez
DISPLAY arr_detalle.ed_facultado            TO v_ed_facultado
DISPLAY arr_detalle.ed_puesto               TO v_ed_puesto
DISPLAY arr_detalle.ed_fch_ejecucion        TO v_ed_fch_ejecucion
DISPLAY arr_detalle.ed_procede_juicio       TO v_ed_procede_juicio

        -- Abre la pantalla para consultar la solicitud
--    CLOSE WINDOW w_parcial
    RETURN arr_detalle.*
END FUNCTION
#---------------------------------------------------------------------------#
# fn_busca_id_derechohabiente: Obtiene el id_derechohabiente del nss        #
#                       en proceso                                          #
#---------------------------------------------------------------------------#
PRIVATE FUNCTION fn_busca_id_derechohabiente(p_nss) 

DEFINE 
   p_nss                   CHAR(11),
   v_id_derechohabiente    DECIMAL(10,0)
   
   DISPLAY "Parámetros recibidos :"
   DISPLAY "Id_solicitud:" || p_nss

   LET v_id_derechohabiente = 0
   
   SELECT  id_derechohabiente 
   INTO    v_id_derechohabiente
   FROM    afi_derechohabiente 
   WHERE   nss   = p_nss


        -- Abre la pantalla para consultar la solicitud
--    CLOSE WINDOW w_parcial
    RETURN v_id_derechohabiente
    
END FUNCTION
#-----------------------------------------------------------------------------#
# f_consulta_oficios_rojos : Obtiene y muestra los datos de los oficios rojos #
#-----------------------------------------------------------------------------#
PRIVATE FUNCTION fn_consulta_oficios_rojos(p_nss) 

DEFINE 
   p_nss    CHAR(11) ,
   v_query  STRING 

 DEFINE  arr_detalle  RECORD 
            ed_imp_pago            DECIMAL(14,2)      ,
            ed_fch_contable        CHAR(10) 
      END RECORD 
DEFINE  v_id_derechohabiente      DECIMAL(10,0)
DEFINE  v_ed_fch_pago             DATE
DEFINE  v_resultado               SMALLINT 


DEFINE
   li_elem             SMALLINT
        
   DISPLAY "Parámetros recibidos para obtener los datos de los oficios rojos:"
   DISPLAY "NSS:" || p_nss
   
   -- limpia campos
   DISPLAY 0 TO ed_imp_pago_of_rojos;
   DISPLAY 0 TO ed_fch_pago_of_rojos;

   LET arr_detalle.ed_imp_pago      = NULL 
   LET arr_detalle.ed_fch_contable  = NULL 
   LET v_ed_fch_pago                = NULL 
  
   LET v_query = " SELECT imp_pago, fch_contable ",
                 " FROM   ret_oficios_rojos      ",
                 " WHERE  nss = '", p_nss, "'"
   PREPARE prp_oficios_rojos FROM v_query
   EXECUTE prp_oficios_rojos INTO arr_detalle.ed_imp_pago, v_ed_fch_pago
   FREE prp_oficios_rojos 

   IF  v_ed_fch_pago IS NOT NULL THEN 
      LET arr_detalle.ed_fch_contable = v_ed_fch_pago USING "dd/mm/yyyy"
   END IF       
   
   DISPLAY "Los valores a mostrar en la pantalla son:"
   DISPLAY "ed_imp_pago          :" || arr_detalle.ed_imp_pago
   DISPLAY "ed_fch_contable      :" || arr_detalle.ed_fch_contable
     
   DISPLAY arr_detalle.ed_imp_pago     TO ed_imp_pago_of_rojos
   DISPLAY arr_detalle.ed_fch_contable TO ed_fch_pago_of_rojos

        -- Abre la pantalla para consultar la solicitud
--    CLOSE WINDOW w_parcial
    RETURN arr_detalle.*
END FUNCTION


#------------------------------------------------------------------------------------#
# fn_consulta_detalle_liquida : Obtiene y muestra los datos de los saldos e importes #
#                               solicitados                                          #
#------------------------------------------------------------------------------------#
PRIVATE FUNCTION fn_consulta_detalle_liquida(p_id_derechohabiente, p_f_pago, p_importe_sol, p_edo_sol) 

DEFINE 
   p_id_derechohabiente DECIMAL(10,0)  ,
   p_f_pago             DATE           ,
   p_importe_sol        DECIMAL (16,2) ,
   p_edo_sol            SMALLINT       ,
   p_folio              INTEGER        ,
   v_query              STRING 
DEFINE  arr_detalle  RECORD 
            ed_tesofe               DECIMAL (16,2) ,
            ed_aivs_97              DECIMAL (16,2) ,
            ed_pesos_97             DECIMAL (16,2) ,
            ed_ajustes_aivs_97      DECIMAL (16,2) ,
            ed_ajustes_pesos_97     DECIMAL (16,2) ,
            ed_aivs_92              DECIMAL (16,2) ,
            ed_pesos_92             DECIMAL (16,2) ,
            ed_ajustes_aivs_92      DECIMAL (16,2) ,
            ed_ajustes_pesos_92     DECIMAL (16,2) ,
            ed_importe_cuenta       DECIMAL (16,2) ,
            ed_importe_sol          DECIMAL (16,2) ,
            ed_importe_dif          DECIMAL (16,2) ,
            ed_tipo_diferencia      CHAR    (25)
      END RECORD 
DEFINE  v_fecha_paso             DATE 
DEFINE  v_f_consulta             DATE 
DEFINE  v_id_derechohabiente     DECIMAL(10,0)
DEFINE v_subcuenta               SMALLINT 
DEFINE v_fecha_consulta          DATE 
DEFINE v_resultado               SMALLINT 
DEFINE v_pesos_tesofe            DECIMAL (16,2)
DEFINE v_precio_fondo            DECIMAL (10,5)
DEFINE v_aivs_721                DECIMAL (16,2)
DEFINE v_aivs_672                DECIMAL (16,2)
DEFINE v_nullo                   CHAR(1)
DEFINE v_movtos_97               SMALLINT 
DEFINE v_movtos_92               SMALLINT 
DEFINE v_nss                     CHAR(11)

DEFINE
   li_elem             SMALLINT
        
   DISPLAY "Parámetros recibidos para obtener los saldos:"
   DISPLAY "Id_derechohabiente:" || p_id_derechohabiente
   LET v_id_derechohabiente = p_id_derechohabiente;
   LET v_fecha_consulta     = TODAY;
   LET v_subcuenta          = 47;
   LET v_pesos_tesofe       = 0;
   LET v_nullo              = NULL 
   LET v_movtos_97          = 0;
   LET v_movtos_92          = 0;
   LET v_nss                = NULL;


   -- limpia campos
   DISPLAY 0 TO v_ed_tesofe
   DISPLAY 0 TO v_ed_aivs_97
   DISPLAY 0 TO v_ed_pesos_97
   DISPLAY 0 TO v_ed_ajustes_aivs_97
   DISPLAY 0 TO v_ed_ajustes_pesos_97
   DISPLAY 0 TO v_ed_aivs_92
   DISPLAY 0 TO v_ed_pesos_92
   DISPLAY 0 TO v_ed_ajustes_aivs_92
   DISPLAY 0 TO v_ed_ajustes_pesos_92
   DISPLAY 0 TO v_ed_importe_cuenta
   DISPLAY 0 TO v_ed_importe_sol
   DISPLAY 0 TO v_ed_importe_dif

   LET arr_detalle.ed_tesofe           = 0
   LET arr_detalle.ed_aivs_97          = 0
   LET arr_detalle.ed_pesos_97         = 0
   LET arr_detalle.ed_ajustes_aivs_97  = 0
   LET arr_detalle.ed_ajustes_pesos_97 = 0
   LET arr_detalle.ed_aivs_92          = 0
   LET arr_detalle.ed_pesos_92         = 0
   LET arr_detalle.ed_ajustes_aivs_92  = 0
   LET arr_detalle.ed_ajustes_pesos_92 = 0
   LET arr_detalle.ed_importe_cuenta   = 0
   LET arr_detalle.ed_importe_sol      = 0
   LET arr_detalle.ed_importe_dif      = 0
   LET arr_detalle.ed_tipo_diferencia  = ""

   CALL forma.setFieldHidden("v_ed_tipo_diferencia",1)
   CALL forma.setElementHidden("lbl_tipo_diferencia",1)
   
   -- Se obtiene el precio de la fecha de pago
   SELECT precio_fondo
   INTO   v_precio_fondo
   FROM   glo_valor_fondo
   WHERE  fondo = 11
   AND    f_valuacion = p_f_pago

   IF p_edo_sol <> 8 AND p_edo_sol <> 10 AND p_edo_sol <> 15 AND p_edo_sol <> 50 AND p_edo_sol <> 100 THEN
      LET v_f_consulta = TODAY 
   ELSE 
      LET v_f_consulta = p_f_pago
   END IF 
   
   IF v_id_derechohabiente IS NOT NULL  AND v_id_derechohabiente <> 0 THEN 
      -- Se buscan los movimientos 672 y 721

      SELECT NVL(SUM(abs(monto_acciones)),0) 
      INTO   v_aivs_721
      FROM   tmp_movimientos
      WHERE  subcuenta = 4
      AND    movimiento = 721
      AND    id_derechohabiente = v_id_derechohabiente
      AND    f_liquida <= v_f_consulta

      SELECT NVL(SUM(abs(monto_acciones)),0) 
      INTO   v_aivs_672
      FROM   tmp_movimientos
      WHERE  subcuenta = 4
      AND    movimiento = 672
      AND    id_derechohabiente = v_id_derechohabiente
      AND    f_liquida <= v_f_consulta

      LET arr_detalle.ed_ajustes_aivs_97  = v_aivs_721 - v_aivs_672
      LET arr_detalle.ed_ajustes_pesos_97 = arr_detalle.ed_ajustes_aivs_97 * v_precio_fondo

      LET v_aivs_721 = 0
      LET v_aivs_672 = 0
      
      SELECT NVL(SUM(abs(monto_acciones)),0) 
      INTO   v_aivs_721
      FROM   tmp_movimientos
      WHERE  subcuenta = 8
      AND    movimiento = 721
      AND    id_derechohabiente = v_id_derechohabiente
      AND    f_liquida <= v_f_consulta

      SELECT NVL(SUM(abs(monto_acciones)),0) 
      INTO   v_aivs_672
      FROM   tmp_movimientos
      WHERE  subcuenta = 8
      AND    movimiento = 672
      AND    id_derechohabiente = v_id_derechohabiente
      AND    f_liquida <= v_f_consulta

      LET arr_detalle.ed_ajustes_aivs_92  = v_aivs_721 - v_aivs_672
      LET arr_detalle.ed_ajustes_pesos_92 = arr_detalle.ed_ajustes_aivs_92 * v_precio_fondo

   ---  Busca Saldo de Tesofe
      -- Se obtienen los folios
      LET v_query = " EXECUTE FUNCTION fn_saldo_dia(?,?,?,?) "
      PREPARE prp_saldos_tesofe FROM v_query
      EXECUTE prp_saldos_tesofe USING v_nss, v_id_derechohabiente, v_subcuenta, v_f_consulta
                      INTO v_resultado, arr_detalle.ed_tesofe, v_pesos_tesofe
      FREE prp_saldos_tesofe 
      -- Se consulta Vivienda 97

      LET v_subcuenta = 4;
      PREPARE prp_saldos_viv_97 FROM v_query
      EXECUTE prp_saldos_viv_97 USING v_nss, v_id_derechohabiente, v_subcuenta, v_f_consulta
                      INTO v_resultado, arr_detalle.ed_aivs_97, arr_detalle.ed_pesos_97
      FREE prp_saldos_viv_97 
      LET arr_detalle.ed_pesos_97 = arr_detalle.ed_aivs_97 * v_precio_fondo
      -- Se consulta Vivienda 92

      LET v_subcuenta = 8;
      PREPARE prp_saldos_viv_92 FROM v_query
      EXECUTE prp_saldos_viv_92 USING v_nss, v_id_derechohabiente, v_subcuenta, v_f_consulta
                      INTO v_resultado, arr_detalle.ed_aivs_92, arr_detalle.ed_pesos_92
      FREE prp_saldos_viv_92 
      LET arr_detalle.ed_pesos_92 = arr_detalle.ed_aivs_92 * v_precio_fondo

      LET arr_detalle.ed_importe_sol = p_importe_sol
      LET arr_detalle.ed_importe_cuenta = (arr_detalle.ed_tesofe + arr_detalle.ed_pesos_97 + arr_detalle.ed_pesos_92) -
                                          (arr_detalle.ed_ajustes_pesos_97) -
                                          (arr_detalle.ed_ajustes_pesos_92)

      LET arr_detalle.ed_importe_dif = arr_detalle.ed_importe_cuenta - arr_detalle.ed_importe_sol
      IF arr_detalle.ed_importe_dif < 0 THEN 
         LET arr_detalle.ed_importe_dif = arr_detalle.ed_importe_dif * (-1)
      ELSE 
         LET arr_detalle.ed_importe_dif = 0
      END IF 
      -- Busca si hay movimientos para clasificar el tipo de diferencia

      IF arr_detalle.ed_importe_dif <> 0 THEN  
         CALL forma.setFieldHidden("v_ed_tipo_diferencia",0)
         CALL forma.setElementHidden("lbl_tipo_diferencia",0)

         LET v_subcuenta = 4;
         LET v_nullo = NULL 
         LET v_query = " EXECUTE FUNCTION fn_cuenta_movimientos(?,?,?) "
         PREPARE prp_num_movtos_97 FROM v_query
         EXECUTE prp_num_movtos_97 USING v_id_derechohabiente, v_nullo, v_subcuenta
                         INTO v_movtos_97
         FREE prp_num_movtos_97 

         LET v_subcuenta = 8;
         PREPARE prp_num_movtos_92 FROM v_query
         EXECUTE prp_num_movtos_92 USING v_id_derechohabiente, v_nullo, v_subcuenta
                         INTO v_movtos_92
         FREE prp_num_movtos_92 
         IF v_movtos_97 + v_movtos_92 = 0 THEN
            LET arr_detalle.ed_tipo_diferencia = "Importe a cuenta de carga inicial"
         ELSE 
            LET arr_detalle.ed_tipo_diferencia = "Importe cargo a resultados"
         END IF 
      END IF 
      
      DISPLAY "Los valores a mostrar en la pantalla son:"
      DISPLAY "ed_tesofe             :" || arr_detalle.ed_tesofe
      DISPLAY "ed_aivs_97            :" || arr_detalle.ed_aivs_97
      DISPLAY "ed_pesos_97           :" || arr_detalle.ed_pesos_97
      DISPLAY "ed_ajuste_aivs_97     :" || arr_detalle.ed_ajustes_aivs_97
      DISPLAY "ed_ajuste_pesos_97    :" || arr_detalle.ed_ajustes_pesos_97
      DISPLAY "ed_aivs_92            :" || arr_detalle.ed_aivs_92
      DISPLAY "ed_pesos_92           :" || arr_detalle.ed_pesos_92
      DISPLAY "ed_ajuste_aivs_92     :" || arr_detalle.ed_ajustes_aivs_92
      DISPLAY "ed_ajuste_pesos_92    :" || arr_detalle.ed_ajustes_pesos_92
      DISPLAY "ed_importe_cuenta     :" || arr_detalle.ed_importe_cuenta
      DISPLAY "ed_importe_sol        :" || arr_detalle.ed_importe_sol
      DISPLAY "ed_importe_dif        :" || arr_detalle.ed_importe_dif
      DISPLAY "ed_tipo_diferencia    :" || arr_detalle.ed_tipo_diferencia  

      DISPLAY arr_detalle.ed_tesofe             TO v_ed_tesofe
      DISPLAY arr_detalle.ed_aivs_97            TO v_ed_aivs_97
      DISPLAY arr_detalle.ed_pesos_97           TO v_ed_pesos_97
      DISPLAY arr_detalle.ed_ajustes_aivs_97    TO v_ed_ajustes_aivs_97
      DISPLAY arr_detalle.ed_ajustes_pesos_97   TO v_ed_ajustes_pesos_97
      DISPLAY arr_detalle.ed_aivs_92            TO v_ed_aivs_92
      DISPLAY arr_detalle.ed_pesos_92           TO v_ed_pesos_92
      DISPLAY arr_detalle.ed_ajustes_aivs_92    TO v_ed_ajustes_aivs_92
      DISPLAY arr_detalle.ed_ajustes_pesos_92   TO v_ed_ajustes_pesos_92
      DISPLAY arr_detalle.ed_importe_cuenta     TO v_ed_importe_cuenta
      DISPLAY arr_detalle.ed_importe_sol        TO v_ed_importe_sol
      DISPLAY arr_detalle.ed_importe_dif        TO v_ed_importe_dif
      DISPLAY arr_detalle.ed_tipo_diferencia    TO v_ed_tipo_diferencia
   END  IF 

        -- Abre la pantalla para consultar la solicitud
--    CLOSE WINDOW w_parcial
    RETURN arr_detalle.*
END FUNCTION

#-----------------------------------------------------------------------------#
# f_consulta_credito : Obtiene y muestra los datos de la consulta de créditos #
#-----------------------------------------------------------------------------#
PRIVATE FUNCTION fn_consulta_credito(p_id_derechohabiente) 

DEFINE 
   p_id_derechohabiente DECIMAL(10,0)       ,
   v_query              STRING 

 DEFINE  arr_detalle  RECORD 
            ed_tipo_originacion     CHAR(20)          ,
            ed_num_credito          CHAR(20)          ,
            ed_fch_otorgamiento     CHAR(10)          ,
            ed_tipo_credito         SMALLINT          ,
            ed_estado_credito       CHAR(20)          ,
            ed_fch_liquidacion      CHAR(10)          ,
            ed_tipo_descuento       SMALLINT          

      END RECORD 
DEFINE  v_id_derechohabiente      DECIMAL(10,0)
DEFINE  v_ed_tipo_originacion     INTEGER           ,
        v_ed_num_credito          DECIMAL(10,0)     ,
        v_ed_fch_otorgamiento     DATE              ,
        v_ed_tipo_credito         INTEGER           ,
        v_ed_estado_credito       CHAR(20)          ,
        v_ed_fch_liquidacion      DATE              ,
        v_ed_tipo_descuento       INTEGER           
DEFINE v_resultado                SMALLINT 


DEFINE
   li_elem             SMALLINT
        
   DISPLAY "Parámetros recibidos para obtener los créditos:"
   DISPLAY "Id_derechohabiente:" || p_id_derechohabiente
   LET v_id_derechohabiente = p_id_derechohabiente;
   LET v_ed_tipo_originacion = 0;
   LET v_ed_num_credito = 0;
   LET v_ed_fch_otorgamiento = NULL;
   LET v_ed_tipo_credito = 0;
   LET v_ed_estado_credito = "";
   LET v_ed_fch_liquidacion = NULL;
   LET v_ed_tipo_descuento = 0;
   
   -- limpia campos
   DISPLAY 0 TO v_ed_tipo_originacion;
   DISPLAY 0 TO v_ed_num_credito;
   DISPLAY 0 TO v_ed_tipo_credito;
   DISPLAY 0 TO v_ed_tipo_descuento;


   LET arr_detalle.ed_tipo_originacion = "SIN INFORMACIÓN"
   LET arr_detalle.ed_num_credito      = NULL 
   LET arr_detalle.ed_fch_otorgamiento = NULL 
   LET arr_detalle.ed_tipo_credito     = NULL 
   LET arr_detalle.ed_estado_credito   = "SIN INFORMACIÓN" 
   LET arr_detalle.ed_fch_liquidacion  = NULL 
   LET arr_detalle.ed_tipo_descuento   = NULL 
   
   IF v_id_derechohabiente IS NOT NULL  AND v_id_derechohabiente <> 0 THEN 

   ---  Busca Saldo de Tesofe
      -- Se obtienen los folios
      LET v_query = " EXECUTE FUNCTION fn_edo_cred_viv(?,?) "
      PREPARE prp_creditos FROM v_query
      EXECUTE prp_creditos USING v_id_derechohabiente, "1"
                      INTO v_resultado, v_ed_tipo_originacion, v_ed_tipo_credito,
                           v_ed_num_credito, v_ed_fch_otorgamiento, v_ed_fch_liquidacion,
                           v_ed_tipo_descuento
      FREE prp_creditos 
      -- Se consulta Vivienda 97

      CASE v_resultado
         WHEN 0 
            LET arr_detalle.ed_estado_credito = "CRÉDITO VIGENTE"
         WHEN 1 
            LET arr_detalle.ed_estado_credito = "SIN CRÉDITO"
         WHEN 2 
            LET arr_detalle.ed_estado_credito = "CRÉDITO LIQUIDADO"
         WHEN 3
            LET arr_detalle.ed_estado_credito = "CRÉDITO EN TRÁMITE"
         OTHERWISE
            LET arr_detalle.ed_estado_credito = "SIN INFORMACIÓN"
      END CASE 
      CASE v_ed_tipo_originacion
         WHEN 1 
            LET arr_detalle.ed_tipo_originacion = "CRÉDITO TRADICIONAL"
         WHEN 2
            LET arr_detalle.ed_tipo_originacion = "CRÉDITO 43 BIS"
         WHEN 3
            LET arr_detalle.ed_tipo_originacion = "CRÉDITO COFINANCIADO"
         OTHERWISE
            LET arr_detalle.ed_tipo_originacion = "SIN INFORMACIÓN"
      END CASE 
      IF v_ed_fch_otorgamiento IS NOT NULL THEN 
         LET arr_detalle.ed_fch_otorgamiento = v_ed_fch_otorgamiento USING "dd/mm/yyyy"
      END IF 
      IF v_ed_fch_liquidacion IS NOT NULL THEN 
         LET arr_detalle.ed_fch_liquidacion = v_ed_fch_liquidacion USING "dd/mm/yyyy"
      END IF 
      LET arr_detalle.ed_num_credito = v_ed_num_credito
      LET arr_detalle.ed_tipo_credito = v_ed_tipo_credito
      LET arr_detalle.ed_tipo_descuento = v_ed_tipo_descuento
      
      
      DISPLAY "Los valores a mostrar en la pantalla son:"
      DISPLAY "ed_tipo_originacion :" || arr_detalle.ed_tipo_originacion
      DISPLAY "ed_num_credito      :" || arr_detalle.ed_num_credito
      DISPLAY "ed_fch_otorgamiento :" || arr_detalle.ed_fch_otorgamiento
      DISPLAY "ed_tipo_credito     :" || arr_detalle.ed_tipo_credito
      DISPLAY "ed_estado_credito   :" || arr_detalle.ed_estado_credito
      DISPLAY "ed_fch_liquidacion  :" || arr_detalle.ed_fch_liquidacion
      DISPLAY "ed_tipo_descuento   :" || arr_detalle.ed_tipo_descuento
        
      DISPLAY arr_detalle.ed_tipo_originacion TO v_ed_tipo_originacion
      DISPLAY arr_detalle.ed_num_credito      TO v_ed_num_credito
      DISPLAY arr_detalle.ed_fch_otorgamiento TO v_ed_fch_otorgamiento
      DISPLAY arr_detalle.ed_tipo_credito     TO v_ed_tipo_credito
      DISPLAY arr_detalle.ed_estado_credito   TO v_ed_estado_credito
      DISPLAY arr_detalle.ed_fch_liquidacion  TO v_ed_fch_liquidacion
      DISPLAY arr_detalle.ed_tipo_descuento   TO v_ed_tipo_descuento
   END  IF 

        -- Abre la pantalla para consultar la solicitud
--    CLOSE WINDOW w_parcial
    RETURN arr_detalle.*
END FUNCTION


PRIVATE FUNCTION fn_busca_saldo_cuenta(p_id_derechohabiente, p_f_pago) 

DEFINE 
   p_id_derechohabiente                CHAR(11)  ,
   p_f_pago             DATE      ,
   p_folio              INTEGER   ,
   v_query              STRING 
DEFINE  arr_detalle  RECORD 
         ed_tesofe               DECIMAL (16,2) ,
         ed_aivs_97              DECIMAL (16,2) ,
         ed_ajustes_aivs_97_721  DECIMAL (16,2) ,
         ed_ajustes_aivs_97_672  DECIMAL (16,2) ,
         ed_aivs_92              DECIMAL (16,2) ,
         ed_ajustes_aivs_92_721  DECIMAL (16,2) ,
         ed_ajustes_aivs_92_672  DECIMAL (16,2) 
      END RECORD 
DEFINE  v_fecha_paso             DATE 
DEFINE  v_id_derechohabiente     DECIMAL(10,0)
DEFINE v_subcuenta               SMALLINT 
DEFINE v_fecha_consulta          DATE 
DEFINE v_resultado               SMALLINT 
DEFINE v_pesos_tesofe            DECIMAL(16,2)
DEFINE v_pesos_paso              DECIMAL(16,2)
DEFINE v_precio_fondo            DECIMAL(10,5)
DEFINE v_saldo_cuenta            DECIMAL(16,2)
DEFINE v_nss                     CHAR(11)
DEFINE
   li_elem             SMALLINT
        
   DISPLAY "Se busca el saldo:"
   DISPLAY "ID derechohabiente: " || p_id_derechohabiente
   LET v_id_derechohabiente = p_id_derechohabiente;
   LET v_fecha_consulta = TODAY;
   LET v_subcuenta = 47;
   LET v_pesos_tesofe = 0;

   -- limpia campos

   LET arr_detalle.ed_tesofe               = 0
   LET arr_detalle.ed_aivs_97              = 0
   LET arr_detalle.ed_ajustes_aivs_97_721  = 0
   LET arr_detalle.ed_ajustes_aivs_97_672  = 0
   LET arr_detalle.ed_aivs_92              = 0
   LET arr_detalle.ed_ajustes_aivs_92_721  = 0
   LET arr_detalle.ed_ajustes_aivs_92_672  = 0
   LET v_saldo_cuenta                      = 0
   LET v_pesos_paso                        = 0
   LET v_nss                               = NULL 
   -- Se obtiene el precio de la fecha de pago
   SELECT precio_fondo
   INTO   v_precio_fondo
   FROM   glo_valor_fondo
   WHERE  fondo = 11
   AND    f_valuacion = p_f_pago
   
   IF v_id_derechohabiente IS NOT NULL  AND v_id_derechohabiente <> 0 THEN 
      -- Se buscan los movimientos 672 y 721

      SELECT SUM(ABS(monto_acciones)) 
      INTO   arr_detalle.ed_ajustes_aivs_97_721
      FROM   tmp_movimientos
      WHERE  subcuenta = 4
      AND    movimiento = 721
      AND    id_derechohabiente = v_id_derechohabiente

      SELECT SUM(ABS(monto_acciones)) 
      INTO   arr_detalle.ed_ajustes_aivs_97_672
      FROM   tmp_movimientos
      WHERE  subcuenta = 4
      AND    movimiento = 672
      AND    id_derechohabiente = v_id_derechohabiente

      SELECT SUM(ABS(monto_acciones)) 
      INTO   arr_detalle.ed_ajustes_aivs_92_721
      FROM   tmp_movimientos
      WHERE  subcuenta = 8
      AND    movimiento = 721
      AND    id_derechohabiente = v_id_derechohabiente

      SELECT SUM(ABS(monto_acciones)) 
      INTO   arr_detalle.ed_ajustes_aivs_92_672
      FROM   tmp_movimientos
      WHERE  subcuenta = 8
      AND    movimiento = 672
      AND    id_derechohabiente = v_id_derechohabiente

   ---  Busca Saldo de Tesofe
      -- Se obtienen los folios
      LET v_query = " EXECUTE FUNCTION fn_saldo_dia(?,?,?,?) "
      PREPARE prp_saldos_tesofe_sal FROM v_query
      EXECUTE prp_saldos_tesofe_sal USING v_nss, v_id_derechohabiente, v_subcuenta, v_fecha_consulta
                      INTO v_resultado, arr_detalle.ed_tesofe, v_pesos_tesofe
      FREE prp_saldos_tesofe_sal 
      -- Se consulta Vivienda 97

      LET v_subcuenta = 4;
      PREPARE prp_saldos_viv_97_sal FROM v_query
      EXECUTE prp_saldos_viv_97_sal USING v_nss, v_id_derechohabiente, v_subcuenta, v_fecha_consulta
                      INTO v_resultado, arr_detalle.ed_aivs_97, v_pesos_paso
      FREE prp_saldos_viv_97_sal 

      -- Se consulta Vivienda 92
      LET v_subcuenta = 8;
      PREPARE prp_saldos_viv_92_sal FROM v_query
      EXECUTE prp_saldos_viv_92_sal USING v_nss, v_id_derechohabiente, v_subcuenta, v_fecha_consulta
                      INTO v_resultado, arr_detalle.ed_aivs_92, v_pesos_paso
      FREE prp_saldos_viv_92_sal 
      LET v_saldo_cuenta = v_pesos_tesofe + ((arr_detalle.ed_aivs_97 + 
                                                    arr_detalle.ed_aivs_92) - 
                                                   (arr_detalle.ed_ajustes_aivs_97_721 - 
                                                    arr_detalle.ed_ajustes_aivs_97_672) -
                                                   (arr_detalle.ed_ajustes_aivs_92_721 - 
                                                    arr_detalle.ed_ajustes_aivs_92_672)) * v_precio_fondo
                                             
   END  IF 

        -- Abre la pantalla para consultar la solicitud
--    CLOSE WINDOW w_parcial
    RETURN v_saldo_cuenta
END FUNCTION


#---------------------------------------------------------------------------#
# f_tablas_tmp : Genera las tablas temporales donde se almacenan los        #
#                calculos y cambios                                         #
#---------------------------------------------------------------------------#
FUNCTION f_tablas_tmp()

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_movimientos
    WHENEVER ERROR STOP

    #-- --------------------------------------------------------------------

    CREATE TEMP TABLE tmp_movimientos
    (
         f_liquida          DATE         ,
         id_derechohabiente DECIMAL(9,0) ,
         subcuenta          SMALLINT     ,
         fondo_inversion    SMALLINT     ,
         movimiento         SMALLINT     ,
         folio_liquida      DECIMAL(9,0) ,
         id_referencia      DECIMAL(9,0) ,
         monto_acciones     DECIMAL(16,6),
         monto_pesos        DECIMAL(12,2),
         origen             CHAR(20)
    )
     DISPLAY "se creo la tabla tmp_movimientos"
    #-- --------------------------------------------------------------------

END FUNCTION


FUNCTION fn_busca_marcas_activas(p_id_derechohabiente)

   DEFINE p_id_derechohabiente DECIMAL(10,0)
   DEFINE v_consulta           STRING 
   DEFINE i                    INTEGER 
   DEFINE v_id_derechohabiente DECIMAL(10,0)


   DEFINE arr_reg_marcas_activas_paso  RECORD 
      v_marca                SMALLINT       ,
      v_desc_marca           CHAR(40)       ,
      v_fecha                DATE           ,
      v_hora                 CHAR(8)        ,
      v_referencia           DECIMAL(10,0)  ,
      v_usuario              CHAR(20)
   END RECORD 
DEFINE arr_reg_marcas_historicas_paso  RECORD 
         v_fecha_fin            DATE           ,
         v_disgnostico          CHAR(20)       ,
         v_desc_diag            CHAR(40)       ,
         v_marca                SMALLINT       ,
         v_desc_marca           CHAR(40)       ,
         v_fecha_ini            DATE           ,
         v_hora                 CHAR(8)        ,
         v_estado_marca         SMALLINT       ,
         v_marca_causa          SMALLINT       ,
         v_fecha_causa          DATE           ,
         v_referencia           DECIMAL(10,0)  ,
         v_usuario              CHAR(20)       ,
         v_usuario_demarca      CHAR(20)
      END RECORD 

   -- Limpia el arreglo
   CALL arr_reg_marcas_activas.clear()
   CALL arr_reg_marcas_historicas.clear()
   LET v_id_derechohabiente = 0
   LET i = 0
   LET v_id_derechohabiente = p_id_derechohabiente

   IF v_id_derechohabiente IS NOT NULL AND v_id_derechohabiente <> 0 THEN 

      LET v_consulta = "SELECT                            " ,
                       "       a.marca                 ,  " ,
                       "       b.descripcion_marca     ,  " ,
                       "       a.f_inicio              ,  " ,
                       "       a.h_inicio              ,  " ,
                       "       a.n_referencia          ,  " ,
                       "       a.usuario_marca            " ,
                       " FROM                             " ,
                       "       sfr_marca_activa a,        " ,
                       "       sfr_marca b                " ,
                       " WHERE a.marca = b.marca          " , 
                       " AND   a.id_derechohabiente =     " , v_id_derechohabiente 
      DISPLAY "La consulta de marcas activas,", v_consulta
      PREPARE prp_marca_activa FROM v_consulta
      DECLARE cur_marca_activa CURSOR FOR prp_marca_activa

      LET i = 1

      FOREACH cur_marca_activa INTO arr_reg_marcas_activas_paso.*
         LET arr_reg_marcas_activas[i].* = arr_reg_marcas_activas_paso.*
         LET i = i + 1
      END FOREACH

      LET v_consulta = " SELECT a.f_fin,                  ",
                       "       a.estado_marca,            ", 
                       "       CASE WHEN a.estado_marca = 0 THEN 'PROCEDENTE' ELSE 'IMPROCEDENTE' END, ",
                       "       a.marca,                   ",
                       "       c.descripcion_marca,       ",
                       "       a.f_inicio,                ",
                       "       a.h_inicio,                ",
                       "       a.estado_marca,            ",
                       "       a.marca_causa,             ",
                       "       a.f_marca_causa,           ",
                       "       a.n_referencia,            ",
                       "       a.usuario_marca,           ",
                       "       a.usuario_desmarca         ",
                       "FROM   sfr_marca_historica a,     ",
                       "       cat_rch_marca b,           ",
                       "       sfr_marca c                ",
                       "WHERE  a.rch_cod = b.rch_cod      ",
                       "AND    a.marca = c.marca          ",
                       "AND    a.id_derechohabiente = ", v_id_derechohabiente 

      DISPLAY "La consulta de marcas historicas,", v_consulta
      PREPARE prp_marca_historica FROM v_consulta
      DECLARE cur_marca_historica CURSOR FOR prp_marca_historica

      LET i = 1

      FOREACH cur_marca_historica INTO arr_reg_marcas_historicas_paso.*
         LET arr_reg_marcas_historicas[i].* = arr_reg_marcas_historicas_paso.*
         LET i = i + 1
      END FOREACH


   END IF 
   RETURN i
   
END FUNCTION

FUNCTION fn_busca_movtos(p_id_derechohabiente)

   DEFINE p_id_derechohabiente DECIMAL(10,0)
   DEFINE v_consulta           STRING 
   DEFINE i                    INTEGER 
   DEFINE v_id_derechohabiente DECIMAL(10,0)

DEFINE arr_reg_movtos_97_paso  RECORD 
         v_fecha_liquida        DATE           ,
         v_acciones             DECIMAL(16,2)  ,
         v_pesos                DECIMAL(16,2)  ,
         v_precio_accion        DECIMAL(10,6)  ,
         v_fondo                SMALLINT       ,
         v_subcuenta            SMALLINT       ,
         v_tipo_movimiento      CHAR(40)       ,
         v_folio                DECIMAL(10,0)  ,
         v_origen               CHAR(30)
      END RECORD 
DEFINE arr_reg_movtos_92_paso  RECORD 
         v_fecha_liquida        DATE           ,
         v_acciones             DECIMAL(16,2)  ,
         v_pesos                DECIMAL(16,2)  ,
         v_precio_accion        DECIMAL(10,6)  ,
         v_fondo                SMALLINT       ,
         v_subcuenta            SMALLINT       ,
         v_tipo_movimiento      CHAR(40)       ,
         v_folio                DECIMAL(10,0)  ,
         v_origen               CHAR(30)
      END RECORD 

   -- Limpia el arreglo
   CALL arr_reg_movtos_97.clear()
   CALL arr_reg_movtos_92.clear()
   LET v_id_derechohabiente = p_id_derechohabiente
   LET i = 0

   IF v_id_derechohabiente IS NOT NULL AND v_id_derechohabiente <> 0 THEN 
      LET v_consulta = " SELECT a.f_liquida,          \n ",
                       "        a.monto_acciones,     \n ", 
                       "        a.monto_pesos,        \n ",
                       "        b.precio_fondo,       \n ",
                       "        a.fondo_inversion,    \n ",
                       "        a.subcuenta,          \n ",
                       "        c.movimiento_desc,    \n ",
                       "        a.folio_liquida,      \n ",
                       "        a.origen              \n ",
                       " FROM   tmp_movimientos a,     \n ",
                       "        glo_valor_fondo b,     \n ",
                       "        cat_movimiento c       \n ",
                       " WHERE  a.id_derechohabiente = ", v_id_derechohabiente, " \n ",
                       " AND    a.fondo_inversion = b.fondo \n ",
                       " AND    a.f_liquida = b.f_valuacion \n ",
                       " AND    a.movimiento = c.movimiento \n ",
                       " AND    a.subcuenta = 4 "

      DISPLAY "La consulta de movimientos vivienda 97 ,", v_consulta
      PREPARE prp_movtos_97 FROM v_consulta
      DECLARE cur_movtos_97 CURSOR FOR prp_movtos_97

      LET i = 1

      FOREACH cur_movtos_97 INTO arr_reg_movtos_97_paso.*
         LET arr_reg_movtos_97[i].* = arr_reg_movtos_97_paso.*
         LET i = i + 1
      END FOREACH

      LET v_consulta = " SELECT a.f_liquida,          \n ",
                       "        a.monto_acciones,     \n ", 
                       "        a.monto_pesos,        \n ",
                       "        b.precio_fondo,       \n ",
                       "        a.fondo_inversion,    \n ",
                       "        a.subcuenta,          \n ",
                       "        c.movimiento_desc,    \n ",
                       "        a.folio_liquida,      \n ",
                       "        a.origen              \n ",
                       " FROM   tmp_movimientos a,     \n ",
                       "        glo_valor_fondo b,     \n ",
                       "        cat_movimiento c       \n ",
                       " WHERE  a.id_derechohabiente = ", v_id_derechohabiente, " \n ",
                       " AND    a.fondo_inversion = b.fondo \n ",
                       " AND    a.f_liquida = b.f_valuacion \n ",
                       " AND    a.movimiento = c.movimiento \n ",
                       " AND    a.subcuenta = 8 "

      DISPLAY "La consulta de movimientos vivienda 92 ,", v_consulta
      PREPARE prp_movtos_92 FROM v_consulta
      DECLARE cur_movtos_92 CURSOR FOR prp_movtos_92

      LET i = 1

      FOREACH cur_movtos_92 INTO arr_reg_movtos_92_paso.*
         LET arr_reg_movtos_92[i].* = arr_reg_movtos_92_paso.*
         LET i = i + 1
      END FOREACH
      
      END IF 
   RETURN i
   
END FUNCTION



#---------------------------------------------------------------------------#
# f_genera_datos : Obtiene los datos de acuerdo a los criterios de busqueda #
#                  insertados por el usuario y los almacena en la tabla     #
#                  temporal desde donde se recuperan para desplegarse       #
#---------------------------------------------------------------------------#
FUNCTION f_genera_datos(pc_where)

    DEFINE pc_where          STRING

    DEFINE lc_query          STRING
    DEFINE v_query_insert    STRING   
    DEFINE v_contador_movtos INTEGER 

    DEFINE lr_datos_tmp RECORD
         f_liquida          DATE         ,
         id_derechohabiente DECIMAL(9,0) ,
         subcuenta          SMALLINT     ,
         fondo_inversion    SMALLINT     ,
         movimiento         SMALLINT     ,
         folio_liquida      DECIMAL(9,0) ,
         id_referencia      DECIMAL(9,0) ,
         monto_acciones     DECIMAL(16,6),
         monto_pesos        DECIMAL(12,2),
         origen             CHAR(20)
    END RECORD
    DEFINE v_tabla          CHAR(20)

    --DEFINE v_string base.StringBuffer
    --DEFINE v_where  STRING

    -- -----------------------------------------------------------------------------

   CALL f_tablas_tmp()
   CREATE INDEX tmp_movimientos_01
   ON tmp_movimientos(id_derechohabiente)
   LET v_contador_movtos = 0;
   LET lc_query = " SELECT tabla \n ",
                " FROM   cat_tab_movimiento \n ",
                " UNION \n ",
                " SELECT 'cta_movimiento' \n ",
                " FROM   systables        \n ",
                " WHERE  tabid = 1 "

   PREPARE prp_tablas FROM lc_query
   DECLARE cur_tablas CURSOR FOR prp_tablas

   FOREACH cur_tablas INTO v_tabla

      LET v_query_insert = " SELECT   \n ",
                            "        b.f_liquida          ,    \n ",
                            "        b.id_derechohabiente ,    \n ",
                            "        b.subcuenta          ,    \n ",
                            "        b.fondo_inversion    ,    \n ",
                            "        b.movimiento         ,    \n ",
                            "        b.folio_liquida      ,    \n ",
                            "        b.id_referencia      ,    \n ",
                            "        b.monto_acciones     ,    \n ",
                            "        b.monto_pesos        ,    \n ",
                            "        b.origen                  \n ",
                            " FROM   ", v_tabla CLIPPED , " b, \n ",
                            "        afi_derechohabiente c,    \n ",
                            "        ret_excep_devol_ssv a     \n ",
                            " WHERE  b.id_derechohabiente = c.id_derechohabiente \n ",
                            " AND    b.subcuenta IN (4, 8)     \n ",
                            " AND    b.movimiento IN (672,721) \n ",
                            " AND    a.nss = c.nss             \n "
      LET v_query_insert = v_query_insert CLIPPED, pc_where CLIPPED 

      DISPLAY "El query de los movimientos ", v_query_insert
      PREPARE prp_movimientos FROM v_query_insert
      DECLARE cur_movimientos CURSOR FOR prp_movimientos

      FOREACH cur_movimientos INTO lr_datos_tmp.*

         INSERT INTO tmp_movimientos
         VALUES (lr_datos_tmp.*)
         LET v_contador_movtos = v_contador_movtos + 1
      END FOREACH
      FREE prp_movimientos
      FREE cur_movimientos


    END FOREACH

    DISPLAY "se insertaron los registros en la tabla tmp_movimientos >", v_contador_movtos, "<"
    
END FUNCTION

FUNCTION fn_busca_pagos_previos(p_id_solicitud)

   DEFINE p_id_solicitud   DECIMAL(10,0)
   DEFINE v_consulta           STRING 
   DEFINE i                    INTEGER 

DEFINE arr_reg_pagos_previos_paso  RECORD 
         documento              CHAR(10)     ,
         ejercicio              CHAR(4)      ,
         estatus                CHAR(2)      ,  
         f_pago                 CHAR(10)      ,
         monto                  DECIMAL(22,2),
         referencia             CHAR(16)     ,
         wt_withcd              CHAR(2)
      END RECORD 
   
   -- Limpia el arreglo
   CALL arr_reg_pagos_previos.clear()
   LET i = 0

   IF p_id_solicitud IS NOT NULL AND p_id_solicitud <> 0 THEN 
      LET v_consulta = " SELECT documento,               \n ",
                       "        ejercicio,               \n ", 
                       "        estatus,                 \n ",
                       "        substr(f_pago,9,2)||'/'||substr(f_pago,6,2)||'/'||substr(f_pago,1,4),\n ",
                       "        monto,                   \n ",
                       "        referencia,              \n ",
                       "        wt_withcd                \n ",
                       " FROM   ret_excep_pagos_previos  \n ",
                       " WHERE  id_solicitud = ", p_id_solicitud, " \n "

      DISPLAY "La consulta de pagos previos ,", v_consulta
      PREPARE prp_pagos_previos FROM v_consulta
      DECLARE cur_pagos_previos CURSOR FOR prp_pagos_previos

      LET i = 1

      FOREACH cur_pagos_previos INTO arr_reg_pagos_previos_paso.*
         LET arr_reg_pagos_previos[i].* = arr_reg_pagos_previos_paso.*
         LET i = i + 1
      END FOREACH
      
   END IF 
   RETURN i
   
END FUNCTION

FUNCTION fn_determina_tipo_diferencia(p_nss,p_id_derechohabiente,p_tipo_producto,p_oficio, p_credito,p_movtos)

   DEFINE p_tipo_producto      CHAR(30) 
   DEFINE p_id_derechohabiente DECIMAL(9,0)
   DEFINE p_oficio             SMALLINT 
   DEFINE p_credito            SMALLINT 
   DEFINE p_movtos             SMALLINT 
   DEFINE p_estado_sol         SMALLINT 
   DEFINE p_id_solicitud       DECIMAL(10,0)
   DEFINE p_nss                CHAR(11) 
   

   DEFINE v_consulta           STRING 
   DEFINE i                    INTEGER 
   DEFINE v_id_derechohabiente DECIMAL(10,0)
   DEFINE v_tipo_diferencia    CHAR(25)
   DEFINE v_con_retiro_previo  SMALLINT 
   DEFINE v_subcuenta          SMALLINT 
   DEFINE v_query              STRING
   DEFINE v_nullo              CHAR(1)
   DEFINE v_movtos_97          SMALLINT
   DEFINE v_movtos_92          SMALLINT 
   
   LET v_movtos_97          = 0;
   LET v_movtos_92          = 0;

   DISPLAY "Se determina el tipo de diferencia para el nss: ", p_nss
   

   IF (p_tipo_producto CLIPPED = '03' OR
       p_tipo_producto CLIPPED = '06' OR
       p_tipo_producto CLIPPED = '07' OR
       p_tipo_producto CLIPPED = '09' ) OR 
      (p_oficio = 1) OR  
      (p_credito = 1) THEN 
      LET v_tipo_diferencia = "CARGO A RESULTADOS"
      DISPLAY "Cargo a Resultados por tipo producto o por oficio rojo o por credito"
   ELSE
      LET v_tipo_diferencia = "CARGO A CARGA INICIAL"
      --- busca si tuvo algun retiro previo
      SELECT COUNT(*) 
      INTO   v_con_retiro_previo
      FROM   ret_disposicion
      WHERE  id_derechohabiente = p_id_derechohabiente
      AND    estado_solicitud IN (60,70)
      IF v_con_retiro_previo > 0 THEN 
         LET v_tipo_diferencia = "CARGO A RESULTADOS"
         DISPLAY "Cargo a Resultados por retiro por disposición"
      END IF 
      SELECT COUNT(*) 
      INTO   v_con_retiro_previo
      FROM   ret_transferencia
      WHERE  id_derechohabiente = p_id_derechohabiente
      AND    estado_solicitud IN (60,70)
      IF v_con_retiro_previo > 0 THEN 
         LET v_tipo_diferencia = "CARGO A RESULTADOS"
         DISPLAY "Cargo a Resultados por retiro por transferenca"
      END IF 
      
      SELECT COUNT(*) 
      INTO   v_con_retiro_previo
      FROM   ret_cargos_ssv_siaff
      WHERE  nss = p_nss
      AND    cod_rechazo <> 0
      IF v_con_retiro_previo > 0 THEN 
         LET v_tipo_diferencia = "CARGO A RESULTADOS"
         DISPLAY "Cargo a Resultados por retiro por cargo al SSV SIAFF"
      END IF 

      SELECT COUNT(*) 
      INTO   v_con_retiro_previo
      FROM   ret_ley73
      WHERE  id_derechohabiente = p_id_derechohabiente
      AND    estado_solicitud IN (60,70)
      IF v_con_retiro_previo > 0 THEN 
         LET v_tipo_diferencia = "CARGO A RESULTADOS"
         DISPLAY "Cargo a Resultados por retiro por Ley 73"
      END IF 

      SELECT COUNT(*) 
      INTO   v_con_retiro_previo
      FROM   ret_his_anexo1
      WHERE  id_derechohabiente = p_id_derechohabiente
      AND    estado_solicitud IN (50)
      IF v_con_retiro_previo > 0 THEN 
         LET v_tipo_diferencia = "CARGO A RESULTADOS"
         DISPLAY "Cargo a Resultados por retiro por anexo 1"
      END IF 
      IF v_tipo_diferencia = "CARGO A CARGA INICIAL" THEN 
         IF p_id_derechohabiente IS NOT NULL AND p_id_derechohabiente <> 0 THEN
            LET v_subcuenta = 4;
            LET v_nullo = NULL 
            LET v_query = " EXECUTE FUNCTION fn_cuenta_movimientos(?,?,?) "
            PREPARE prp_num_movtos_97_dif FROM v_query
            EXECUTE prp_num_movtos_97_dif USING p_id_derechohabiente, v_nullo, v_subcuenta
                            INTO v_movtos_97
            FREE prp_num_movtos_97_dif 

            LET v_subcuenta = 8;
            PREPARE prp_num_movtos_92_dif FROM v_query
            EXECUTE prp_num_movtos_92_dif USING p_id_derechohabiente, v_nullo, v_subcuenta
                            INTO v_movtos_92
            FREE prp_num_movtos_92_dif 
            IF v_movtos_97 + v_movtos_92 > 0 THEN
               LET v_tipo_diferencia = "CARGO A RESULTADOS"
               DISPLAY "Cargo a Resultados por retiro por movimientos 97 y/o 92"
            END IF
         END IF 
      END IF 
   END IF 
   
   RETURN v_tipo_diferencia
   
END FUNCTION

FUNCTION fn_busca_datos_liq_previos(p_id_solicitud, p_arr_datos_liquidacion)
DEFINE p_id_solicitud    DECIMAL(10,0)
DEFINE p_arr_datos_liquidacion RECORD 
            v_ed_tesofe               DECIMAL (16,2) ,
            v_ed_aivs_97              DECIMAL (16,2) ,
            v_ed_pesos_97             DECIMAL (16,2) ,
            v_ed_ajustes_aivs_97      DECIMAL (16,2) ,
            v_ed_ajustes_pesos_97     DECIMAL (16,2) ,
            v_ed_aivs_92              DECIMAL (16,2) ,
            v_ed_pesos_92             DECIMAL (16,2) ,
            v_ed_ajustes_aivs_92      DECIMAL (16,2) ,
            v_ed_ajustes_pesos_92     DECIMAL (16,2) ,
            v_ed_importe_cuenta       DECIMAL (16,2) ,
            v_ed_importe_sol          DECIMAL (16,2) ,
            v_ed_importe_dif          DECIMAL (16,2) ,
            v_ed_tipo_diferencia      CHAR    (25)
END RECORD  

DEFINE v_contador    SMALLINT 
--- busca la solicitud en la tabla de ret_excep_inf_liq si no se encuentra devuelve el record tal cual llego

   SELECT COUNT(*) 
   INTO   v_contador
   FROM   ret_excep_inf_liq
   WHERE  id_solicitud = p_id_solicitud

   IF v_contador = 1 THEN 
      SELECT *
      INTO   p_id_solicitud, 
             p_arr_datos_liquidacion.v_ed_tesofe,
             p_arr_datos_liquidacion.v_ed_aivs_97,
             p_arr_datos_liquidacion.v_ed_aivs_92,
             p_arr_datos_liquidacion.v_ed_pesos_97,
             p_arr_datos_liquidacion.v_ed_pesos_92,
             p_arr_datos_liquidacion.v_ed_ajustes_aivs_97,
             p_arr_datos_liquidacion.v_ed_ajustes_aivs_92,
             p_arr_datos_liquidacion.v_ed_ajustes_pesos_97,
             p_arr_datos_liquidacion.v_ed_ajustes_pesos_92,
             p_arr_datos_liquidacion.v_ed_importe_cuenta,
             p_arr_datos_liquidacion.v_ed_importe_sol,
             p_arr_datos_liquidacion.v_ed_importe_dif,
             p_arr_datos_liquidacion.v_ed_tipo_diferencia
      FROM   ret_excep_inf_liq
      WHERE  id_solicitud = p_id_solicitud
   END IF 


RETURN p_arr_datos_liquidacion.*
 
END FUNCTION 

FUNCTION fn_exporta_archivo()

DEFINE  v_registro_archivo  DYNAMIC ARRAY OF RECORD    -- registro del archivo

      id_solicitud         DECIMAL(10,0) ,
      nss                  CHAR(11)      ,
      f_solicitud          DATE          ,
      f_autoriza           DATE          ,
      f_pago               DATE          ,
      imp_sol              DECIMAL(16,2) ,
      edo_sol              SMALLINT      ,
      estado_sol           CHAR(50)      ,
      cod_rechazo          SMALLINT      ,
      codigo_rechazo       CHAR(50)      ,
      tipo_producto        CHAR(20)      ,
      tipo_solicitud       CHAR(20)      ,
      referencia_dap       CHAR(23)
END RECORD 

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
   LET v_nom_archivo = "EXPORT_EXCEPCIONES_SSV", TODAY USING "yyyymmdd" 
   LET v_archivo_txt = v_nom_archivo CLIPPED, ".expexcssv"

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
   LET v_query = "SELECT * FROM tmp_exporta_excepciones_ssv"
   PREPARE sid_query_exporta FROM v_query
   DECLARE cur_query_exporta CURSOR FOR sid_query_exporta
   
   LET v_indice = 1
   LET v_nss_paso = NULL
   LET v_s_detalle = "NSS|Fecha de Solicitud|Fecha de Autorización|",
                     "Fecha de Pago|Cve. Estado de la Solicitud|Desc Estado de la Solicitud|",
                     "Importe del Pago|Código de Rechazo|Descripción Rechazo|",
                     "Tipo de Producto|Id Solicitud|Tipo de Solicitud|Número de Referencia DAP|"
   CALL v_ch_arch_ret_generico.write(v_s_detalle)
   LET v_indice = v_indice + 1
   -- se transfieren los datos al arreglo de despliegue agrupador
   FOREACH cur_query_exporta INTO v_registro_archivo[v_indice].*
      DISPLAY "la exportacion", v_registro_archivo[v_indice].*
      LET v_s_detalle =  v_registro_archivo[v_indice].nss                     , "|",
           v_registro_archivo[v_indice].f_solicitud USING "dd/mm/yyyy"        , "|",
           v_registro_archivo[v_indice].f_autoriza  USING "dd/mm/yyyy"        , "|",
           v_registro_archivo[v_indice].f_pago      USING "dd/mm/yyyy"        , "|",
           v_registro_archivo[v_indice].edo_sol                               , "|",
           v_registro_archivo[v_indice].estado_sol                            , "|",
           v_registro_archivo[v_indice].imp_sol     USING "&&&&&&&&&&&&&&.&&" , "|",
           v_registro_archivo[v_indice].cod_rechazo                           , "|",
           v_registro_archivo[v_indice].codigo_rechazo                        , "|",
           v_registro_archivo[v_indice].tipo_producto                         , "|",
           v_registro_archivo[v_indice].id_solicitud                          , "|",
           v_registro_archivo[v_indice].tipo_solicitud                        , "|",
           v_registro_archivo[v_indice].referencia_dap                        , "|"
           
           CALL v_ch_arch_ret_generico.write(v_s_detalle)
         LET v_indice = v_indice + 1
   END FOREACH

   -- Se cierra el archivo de Rechazados
   CALL v_ch_arch_ret_generico.close()

   LET v_mensaje_archivo = "El archivo fue generado exitosamente:\n\n\t", v_v_ruta_nomarch

   CALL fn_mensaje("Atención", v_mensaje_archivo, "information")

END FUNCTION


#-----------------------------------------------------------------------------#
# f_consulta_pago : Obtiene y muestra los datos de la consulta del pago       #
#-----------------------------------------------------------------------------#
PRIVATE FUNCTION fn_consulta_pago(p_id_solicitud) 

   DEFINE 
      p_id_solicitud       DECIMAL(10,0)       ,
      v_query              STRING 

   DEFINE  arr_detalle  RECORD 
      ed_teso_envio                  VARCHAR(80) ,
      ed_teso_respuesta              VARCHAR(80) ,
      ed_teso_fch_envio              CHAR(10)    ,
      ed_teso_fch_respuesta          CHAR(10)    ,
      ed_teso_codigo_respuesta       VARCHAR(80) ,
      ed_pago_fch_consulta           CHAR(10)    ,
      ed_pago_fch_pago               CHAR(10)    ,
      ed_pago_respuesta              VARCHAR(40) ,
      ed_titular_nombre              VARCHAR(60) ,
      ed_titular_folio_liquida       CHAR(10)    ,
      ed_titular_rfc                 CHAR(13)    ,
      ed_titular_folio_restitucion   CHAR(10)    ,
      ed_benef_nombre                VARCHAR(60)
   END RECORD 
   DEFINE  v_id_solicitud            DECIMAL(10,0)
   DEFINE  v_id_archivo_envio        DECIMAL (9,0)
   DEFINE  v_id_archivo_respuesta    DECIMAL (9,0)

DEFINE v_resultado                SMALLINT 


DEFINE
   li_elem             SMALLINT
        
   DISPLAY "Parámetros recibidos para obtener la información del pago:"
   DISPLAY "Id_solicitud:" || p_id_solicitud
   LET v_id_archivo_envio                        = 0
   LET v_id_archivo_respuesta                    = 0
   LET v_id_solicitud = p_id_solicitud;
   LET arr_detalle.ed_teso_envio                  = ""
   LET arr_detalle.ed_teso_respuesta              = ""
   LET arr_detalle.ed_teso_fch_envio              = ""
   LET arr_detalle.ed_teso_fch_respuesta          = ""
   LET arr_detalle.ed_teso_codigo_respuesta       = ""
   LET arr_detalle.ed_pago_fch_consulta           = ""
   LET arr_detalle.ed_pago_fch_pago               = ""
   LET arr_detalle.ed_pago_respuesta              = ""
   LET arr_detalle.ed_titular_nombre              = ""
   LET arr_detalle.ed_titular_folio_liquida       = ""
   LET arr_detalle.ed_titular_rfc                 = ""
   LET arr_detalle.ed_titular_folio_restitucion   = ""
   LET arr_detalle.ed_benef_nombre                = ""
  
   -- limpia campos
   DISPLAY 0 TO ed_teso_envio
   DISPLAY 0 TO ed_teso_respuesta
   DISPLAY 0 TO ed_teso_fch_envio
   DISPLAY 0 TO ed_teso_fch_respuesta
   DISPLAY 0 TO ed_teso_codigo_respuesta
   DISPLAY 0 TO ed_pago_fch_consulta
   DISPLAY 0 TO ed_pago_fch_pago
   DISPLAY 0 TO ed_pago_respuesta
   DISPLAY 0 TO ed_titular_nombre
   DISPLAY 0 TO ed_titular_folio_liquida
   DISPLAY 0 TO ed_titular_rfc
   DISPLAY 0 TO ed_titular_folio_restitucion
   DISPLAY 0 TO ed_benef_nombre

  
   IF v_id_solicitud IS NOT NULL  AND v_id_solicitud <> 0 THEN 

      ---  Se obtienen los id´s de los archivos de envio y respuesta

      SELECT id_archivo_envio, id_archivo_respuesta
      INTO   v_id_archivo_envio, v_id_archivo_respuesta
      FROM   ret_excep_devol_ssv
      WHERE  id_solicitud = v_id_solicitud

      ---  Busca la información del archivo de envio

      IF v_id_archivo_envio IS NOT NULL AND v_id_archivo_envio <> 0 THEN 
         SELECT nombre_archivo, TO_CHAR(f_actualiza,"%d/%m/%Y")
         INTO   arr_detalle.ed_teso_envio, arr_detalle.ed_teso_fch_envio
         FROM   ret_ctr_archivo_fico
         WHERE  id_archivo = v_id_archivo_envio
         DISPLAY arr_detalle.ed_teso_envio TO ed_teso_envio
         DISPLAY arr_detalle.ed_teso_fch_envio TO ed_teso_fch_envio
      END IF 

      ---  Busca la información del archivo de respuesta

      IF v_id_archivo_respuesta IS NOT NULL AND v_id_archivo_respuesta <> 0 THEN 
         SELECT nombre_archivo, TO_CHAR(f_actualiza,"%d/%m/%Y")
         INTO   arr_detalle.ed_teso_respuesta, arr_detalle.ed_teso_fch_respuesta
         FROM   ret_ctr_archivo_fico
         WHERE  id_archivo = v_id_archivo_respuesta
         DISPLAY arr_detalle.ed_teso_respuesta TO ed_teso_respuesta
         DISPLAY arr_detalle.ed_teso_fch_respuesta TO ed_teso_fch_respuesta
      END IF 
      
      ---  Obtiene los datos de pago o rechazo
      SELECT TO_CHAR(f_consulta,"%d/%m/%Y"), SUBSTR(rsp_f_pago,7,2) || "/" || SUBSTR(rsp_f_pago,5,2) || "/" || SUBSTR(rsp_f_pago,1,4), rsp_desc_estatus
      INTO   arr_detalle.ed_pago_fch_consulta, arr_detalle.ed_pago_fch_pago, arr_detalle.ed_pago_respuesta
      FROM   ret_ws_consulta_pago_fico
      WHERE  id_solicitud = v_id_solicitud
      AND    f_consulta   = (SELECT MAX(f_consulta)
                             FROM   ret_ws_consulta_pago_fico
                             WHERE  id_solicitud = v_id_solicitud
                             AND    rsp_estatus IN (2, 3, 4, 23, 24))
      AND    rsp_estatus IN (2, 3, 4, 23, 24)
      DISPLAY arr_detalle.ed_pago_fch_consulta TO ed_pago_fch_consulta
      DISPLAY arr_detalle.ed_pago_fch_pago TO ed_pago_fch_pago
      DISPLAY arr_detalle.ed_pago_respuesta TO ed_pago_respuesta

      --- Obtenemos los datos del trabajador
      SELECT TRIM(nombre_af) || " " || TRIM(ap_paterno_af) || " " || TRIM(ap_materno_af),
             rfc
      INTO   arr_detalle.ed_titular_nombre,
             arr_detalle.ed_titular_rfc
      FROM   afi_derechohabiente
      WHERE  nss = (SELECT nss
                    FROM   ret_excep_devol_ssv
                    WHERE  id_solicitud = v_id_solicitud)

      -- Se obtienen los folios y el beneficiario
      SELECT folio, folio_restitucion, beneficiario
      INTO   arr_detalle.ed_titular_folio_liquida,
             arr_detalle.ed_titular_folio_restitucion,
             arr_detalle.ed_benef_nombre
      FROM   ret_excep_devol_ssv
      WHERE  id_solicitud = v_id_solicitud

      
      DISPLAY "Los valores a mostrar en la pantalla son:"
      DISPLAY "ed_benef_nombre             :" || arr_detalle.ed_benef_nombre
      DISPLAY "ed_pago_fch_consulta        :" || arr_detalle.ed_pago_fch_consulta
      DISPLAY "ed_pago_fch_pago            :" || arr_detalle.ed_pago_fch_pago
      DISPLAY "ed_pago_respuesta           :" || arr_detalle.ed_pago_respuesta
      DISPLAY "ed_teso_codigo_respuesta    :" || arr_detalle.ed_teso_codigo_respuesta
      DISPLAY "ed_teso_envio               :" || arr_detalle.ed_teso_envio
      DISPLAY "ed_teso_fch_envio           :" || arr_detalle.ed_teso_fch_envio
      DISPLAY "ed_teso_fch_respuesta       :" || arr_detalle.ed_teso_fch_respuesta
      DISPLAY "ed_teso_respuesta           :" || arr_detalle.ed_teso_respuesta
      DISPLAY "ed_titular_folio_liquida    :" || arr_detalle.ed_titular_folio_liquida
      DISPLAY "ed_titular_folio_restitucion:" || arr_detalle.ed_titular_folio_restitucion
      DISPLAY "ed_titular_nombre           :" || arr_detalle.ed_titular_nombre
      DISPLAY "ed_titular_rfc              :" || arr_detalle.ed_titular_rfc

      DISPLAY arr_detalle.ed_benef_nombre              TO ed_benef_nombre
      DISPLAY arr_detalle.ed_pago_fch_consulta         TO ed_pago_fch_consulta
      DISPLAY arr_detalle.ed_pago_fch_pago             TO ed_pago_fch_pago
      DISPLAY arr_detalle.ed_pago_respuesta            TO ed_pago_respuesta
      DISPLAY arr_detalle.ed_teso_codigo_respuesta     TO ed_teso_codigo_respuesta
      DISPLAY arr_detalle.ed_teso_envio                TO ed_teso_envio
      DISPLAY arr_detalle.ed_teso_fch_envio            TO ed_teso_fch_envio
      DISPLAY arr_detalle.ed_teso_fch_respuesta        TO ed_teso_fch_respuesta
      DISPLAY arr_detalle.ed_teso_respuesta            TO ed_teso_respuesta
      DISPLAY arr_detalle.ed_titular_folio_liquida     TO ed_titular_folio_liquida
      DISPLAY arr_detalle.ed_titular_folio_restitucion TO ed_titular_folio_restitucion
      DISPLAY arr_detalle.ed_titular_nombre            TO ed_titular_nombre
      DISPLAY arr_detalle.ed_titular_rfc               TO ed_titular_rfc
   END  IF 

        -- Abre la pantalla para consultar la solicitud
--    CLOSE WINDOW w_parcial
    RETURN arr_detalle.*
END FUNCTION