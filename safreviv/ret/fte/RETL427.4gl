--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETL427                                                                #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la liquidacion    #
#                e la carga de los cargos de SSV aplicados via SIAFF                    #
#Fecha inicio => Octubre 7, 2016                                                        #
#########################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_usuario_cod        LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion     SMALLINT, -- forma como ejecutara el programa
       p_s_titulo           STRING, -- titulo de la ventana
       p_operacion          SMALLINT,
       v_id_derechohabiente DECIMAL(9,0),
       v_id_solicitud       DECIMAL(9,0),
       v_query              STRING,
       v_marca_cargo_ssv    SMALLINT,
       v_estado_marca       SMALLINT,
       v_marca_causa        SMALLINT,
       v_i_estado_marca     SMALLINT,
       v_id_solicitud_revol DECIMAL(9,0)
       
   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".RETL421.log")

   LET g_proceso_cod = g_proceso_ley73_cargo_ssv_siaff -- Anexo 1 L73
   LET g_opera_cod   = g_opera_ley73_cargo_ssv_siaff_liquida -- liquidacion
   LET p_operacion   = 2 -- ejecutar liquidacion
   LET v_estado_marca = 0
   LET v_marca_causa = 0
   LET v_marca_cargo_ssv = 819

   -- se invoca la funcion para enviar la liquidacion
   CALL fn_liquida(p_usuario_cod, g_proceso_cod, g_opera_cod, p_operacion)

   DISPLAY "regresa de la liquidacion"
   
   -- Desmarca las cuentas liquidadas
   LET v_query = " SELECT a.id_derechohabiente, b.id_solicitud ",
                 " FROM   afi_derechohabiente a, ret_cargos_ssv_siaff b ",
                 " WHERE  a.nss = b.nss ",
                 " AND    b.estado_solicitud = 50 "

   PREPARE prp_solicitudes FROM v_query
   DECLARE cur_solicitudes CURSOR FOR prp_solicitudes

   FOREACH cur_solicitudes INTO v_id_derechohabiente, v_id_solicitud

      LET v_query = "EXECUTE FUNCTION fn_desmarca_cuenta(?,?,?,?,?,?,?)"
      PREPARE prp_desmarca FROM v_query
      EXECUTE prp_desmarca USING 
                    v_id_derechohabiente
                   ,v_marca_cargo_ssv
                   ,v_id_solicitud
                   ,v_estado_marca
                   ,v_marca_causa
                   ,p_usuario_cod
                   ,g_proceso_cod
               INTO v_i_estado_marca;
               
      -- Actualiza las solicitudes liquidadas

      UPDATE ret_cargos_ssv_siaff
      SET    estado_solicitud = 60
      WHERE  id_solicitud     = v_id_solicitud
      AND    estado_solicitud = 50


      --- Busca si tienen recuperación en ret_revolvente_siaff
      LET v_id_solicitud_revol = 0;
      DISPLAY "busca",  v_id_solicitud
      SELECT a.id_solicitud
      INTO   v_id_solicitud_revol
      FROM   ret_revolvente_siaff a, ret_cargos_ssv_siaff b
      WHERE  a.nss = b.nss
      AND    b.id_solicitud = v_id_solicitud;
      DISPLAY "el valor de revolvente",v_id_solicitud_revol
      IF v_id_solicitud_revol <> 0 THEN 
         DISPLAY "Actualiza ambas"
         UPDATE ret_revolvente_siaff 
         SET    estado_solicitud = 301
         WHERE  id_solicitud = v_id_solicitud_revol
         UPDATE ret_cargos_ssv_siaff
         SET    estado_solicitud = 301
         WHERE  id_solicitud     = v_id_solicitud
         AND    estado_solicitud = 60
      ELSE 
         DISPLAY "Actualiza solo cargos"
         UPDATE ret_cargos_ssv_siaff
         SET    estado_solicitud = 300
         WHERE  id_solicitud     = v_id_solicitud
         AND    estado_solicitud = 60
      END IF 
      
   END FOREACH
   
   -- Deja la huella contable
--    CALL fn_registro_contable(p_folio,p_fecha_liquida,v_cod_proceso_cnt,
--                                      p_proceso_cod,v_cod_transaccion_cnt)
--                            RETURNING v_si_resultado

   
   


 
END MAIN