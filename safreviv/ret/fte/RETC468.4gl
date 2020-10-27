--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => RET                                                     #
#Programa          => RETC468                                                 #
#Objetivo          => Reenvio Notificaciones de Pago Grupos 2, 3 y 4          #
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

DEFINE gr_rutas      RECORD LIKE safre_viv:seg_modulo.*
DEFINE gc_usuario              CHAR(020),
       enter                   CHAR(001)

       
DEFINE p_usuario_cod   LIKE seg_usuario.usuario_cod -- clave del usuario firmado
      ,v_ventana       ui.WINDOW
      ,v_folio         DECIMAL(11,0)

END GLOBALS

PRIVATE DEFINE ventana     ui.Window
PRIVATE DEFINE forma       ui.Form

MAIN
    DEFINE p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
           p_s_titulo       STRING -- titulo de la ventana

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

    LET v_programa     = "RETC468" 
    
   IF ( g_nom_ventana IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_nom_ventana)
   END IF
   
-- pendiente definición de pantallas que reciben parámetros adicionales

    SELECT ruta_bitacora
      INTO v_ruta_bitacora
      FROM seg_modulo
     WHERE modulo_cod = "ret"

    LET v_archivo_log = v_ruta_bitacora CLIPPED ,"/",g_usuario CLIPPED, ".",v_programa,".log"
    CALL STARTLOG(v_archivo_log)

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
       v_f_pago_ini             DATE,
       v_f_pago_fin             DATE,
       v_arr_consulta       DYNAMIC ARRAY OF RECORD 
          v_id_solicitud_envio  DECIMAL(9,0),
          v_id_solicitud_retiro DECIMAL(9,0),
          v_nss                 CHAR(11),
          v_accion              CHAR(15),
          v_diag_procesar       CHAR(3),
          v_desc_diagnostico    CHAR(100),
          v_f_envio             DATE,
          v_f_respuesta         DATE,
          v_indica_cargo        CHAR(1),
          v_importe_total       DECIMAL(11,2),
          v_f_pago              DATE,
          v_grupo               CHAR(4),
          v_valor_aiv           DECIMAL(15,6),
          v_imp_gob_fed         DECIMAL(11,2),
          v_monto_pagado        DECIMAL(11,2),
          v_aivs_pagadas        DECIMAL(15,6)
      END RECORD, 
       v_estado_solicitud       SMALLINT,
       v_cod_rechazo            SMALLINT,
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
       v_c_folio                CHAR(10),
       v_c_archivo_folio        CHAR(40),

       v_s_cadena               STRING, -- cadena de texto
       v_r_ret_estado           RECORD LIKE ret_estado_solicitud.*,
       v_r_ret_rechazo          RECORD LIKE ret_cat_rechazo_dap.*,
       v_resultado              SMALLINT,
       v_tipo_diferencia        CHAR(25),
       v_encontrado_oficios_rojos SMALLINT,
       v_pago_credito           SMALLINT, 
       
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

   DEFINE v_string base.StringBuffer
   DEFINE v_where  STRING
    
   OPEN WINDOW w_consulta_datos WITH FORM "RETC4681"
   LET ventana = ui.Window.getCurrent()
   LET forma   = ventana.getForm()
   LET v_reg_act = 0

   -- se le asigna el apuntado del combo a la variable

   -- se ocultan los grupos de la consulta 
   CALL forma.setElementHidden("grp_detalle",1)
   CALL forma.setFieldHidden("btn_desconfirmar_todas",1)
   CALL forma.setFieldHidden("btn_confirmar_todas",1)


   LET v_nulo                = NULL;

   --   DIALOG 
   INPUT v_f_pago_ini,
       v_f_pago_fin
       --WITHOUT DEFAULTS 
   FROM  d_f_pago_ini,
       d_f_pago_fin
   ATTRIBUTES (UNBUFFERED, WITHOUT DEFAULTS)

      BEFORE INPUT
         -- se limpian las variables
         CALL v_arr_consulta.clear()
         LET v_accion_todas       = "confirmar_todas"
         LET v_cod_rechazo_todas  = 0
         LET v_des_cod_rech_todas = NULL
         LET i_todas              = 0
         LET v_f_pago_ini = TODAY
         LET v_f_pago_fin = TODAY 
        

      ON ACTION ACCEPT

         --modificación de validación de  captura de parametros
         --valida que se ingrese al menos un parametro
         CALL v_arr_consulta.clear()

         IF v_f_pago_ini IS NULL THEN 
            LET v_f_pago_ini = TODAY 
         END IF 
         IF v_f_pago_fin IS NULL THEN 
            LET v_f_pago_fin = TODAY 
         END IF 

         -- se consulta del detalle de este agrupador

         LET v_query    =   " SELECT  MAX(a.id_solicitud_envio), a.id_solicitud_retiro, a.nss,                   \n ",
                       "         0 AS accion, a.diag_procesar, b.descripcion, a.fch_envio, a.fch_respuesta, \n",
                       "         a.indicador_cargo, a.importe_total, a.fch_pago, a.grupo,                   \n ",
                       "         a.valor_aiv, a.imp_gob_fed, a.monto_pagado, a.aivs_pagadas                 \n ",
                       "   FROM  ret_cat_rch_notifica b,ret_notifica_gpo a                                  \n ",
                       "  WHERE  a.diag_procesar = b.cod_rechazo                                            \n ",
                       "  AND    b.reenvio = 1                                                              \n ",
                       "  AND    a.fch_pago BETWEEN '", v_f_pago_ini, "' AND '", v_f_pago_fin, "'           \n",
                       "  AND    a.estado_solicitud = 82                                                    \n ",
                       "  GROUP BY a.id_solicitud_retiro, a.nss,                                            \n ",
                       "         accion, a.diag_procesar, b.descripcion, a.fch_envio, a.fch_respuesta,      \n",
                       "         a.indicador_cargo, a.importe_total, a.fch_pago, a.grupo,                   \n ",
                       "         a.valor_aiv, a.imp_gob_fed, a.monto_pagado, a.aivs_pagadas                 \n "

         DISPLAY ">",v_query,"<"
         PREPARE sid_detalle FROM v_query
         DECLARE cur_detalle  CURSOR FOR sid_detalle

         --llena el arreglo        
         LET v_indice = 1

         FOREACH cur_detalle INTO v_arr_consulta[v_indice].*
            DISPLAY "El resultado de la consulta :>", v_arr_consulta[v_indice].*, "<"
            LET v_indice = v_indice + 1
         END FOREACH
         CALL v_arr_consulta.deleteElement(v_arr_consulta.getLength())

         LET v_indice = v_indice - 1
         IF v_indice = 0 THEN
            CALL fn_mensaje("","Los parámetros de consulta no arrojaron ningún resultado","")
         ELSE 


            CALL forma.setElementHidden("grp_detalle",0)
            CALL forma.setFieldHidden("btn_desconfirmar_todas",0)
            CALL forma.setFieldHidden("btn_confirmar_todas",0)

            DIALOG ATTRIBUTES  (UNBUFFERED)

            INPUT   ARRAY v_arr_consulta FROM rec_datos.* 
                   ATTRIBUTES ( WITHOUT DEFAULTS ,APPEND ROW = FALSE, 
                   DELETE ROW = FALSE , INSERT ROW = FALSE ,KEEP CURRENT ROW = FALSE)

               BEFORE ROW
                  LET v_posicion = ARR_CURR()
               ON ACTION ACCEPT 
                  DISPLAY "se valida si existen cambios y si los hay los actualiza"
                  FOR v_i = 1 TO v_arr_consulta.getLength()
                     DELETE 
                     FROM   ret_notifica_gpo_reenvio
                     WHERE  id_solicitud_retiro = v_arr_consulta[v_i].v_id_solicitud_retiro;
                     IF v_arr_consulta[v_i].v_accion = "confirmar" THEN  
                        INSERT INTO ret_notifica_gpo_reenvio 
                             VALUES (v_arr_consulta[v_i].v_id_solicitud_retiro,
                                     v_arr_consulta[v_i].v_nss,
                                     v_arr_consulta[v_i].v_indica_cargo,
                                     v_arr_consulta[v_i].v_importe_total,
                                     v_arr_consulta[v_i].v_f_pago,
                                     v_arr_consulta[v_i].v_grupo,
                                     v_arr_consulta[v_i].v_valor_aiv,
                                     v_arr_consulta[v_i].v_imp_gob_fed,
                                     v_arr_consulta[v_i].v_monto_pagado,
                                     v_arr_consulta[v_i].v_aivs_pagadas);
                        LET v_reg_act = v_reg_act + 1
                     END IF 
                  END FOR 
                  DISPLAY "Registros dentro del for :", v_reg_act
                  IF v_reg_act > 0 THEN
                     LET v_msg = "Se han actualizado ",v_reg_act, " registros" 
                     CALL fn_mensaje("Atención", v_msg CLIPPED , "information")
                     DISPLAY "Registros actualizados <",v_reg_act, ">"
                  END IF 
                  LET v_reg_act = 0
               ON ACTION btn_desconfirmar_todas 
                  FOR i_todas = 1 TO v_arr_consulta.getLength()
                     LET v_arr_consulta[i_todas].v_accion = "desconfirmar"
                  END FOR    

               ON ACTION btn_confirmar_todas
                  FOR i_todas = 1 TO v_arr_consulta.getLength()
                     LET v_arr_consulta[i_todas].v_accion = "confirmar"
                  END FOR    

            END INPUT  

            ON ACTION CANCELAR
               CALL forma.setElementHidden("grp_detalle",1)
               CALL forma.setFieldHidden("btn_desconfirmar_todas",1)
               CALL forma.setFieldHidden("btn_confirmar_todas",1)
               LET v_folio = NULL 
               LET v_estado_solicitud = NULL 

               EXIT DIALOG   
            END DIALOG       
         END IF 
      ON ACTION CANCEL
         EXIT INPUT
   
   END INPUT
   
   CLOSE WINDOW w_consulta_datos

END FUNCTION

