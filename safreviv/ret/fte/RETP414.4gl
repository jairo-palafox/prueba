################################################################################
#Modulo        => RET                                                          #
#Programa      => RETP414                                                      #
#Ojetivo       => Realizar la consulta para las solicitudes con                #
#                 indicador de pendientte de consulta a PROCESAR               #
#Elaboro       => Luis Felipe Prieto Cano                                      #
#Fecha inicio  => Febrero 17, 2016.                                            #
#Requerimiento => PRODINFXVI-36                                                #
#------------------------------------------------------------------------------#
#Fecha de modificacion =>   Modificación                                       #
#                      =>                                                      #
#                      =>                                                      #
################################################################################
GLOBALS "RETG01.4gl"

DATABASE safre_viv

#Parametros generales del proceso
PRIVATE DEFINE p_pid                      DECIMAL(9,0)                           -- PID del proceso
PRIVATE DEFINE p_proceso_cod              SMALLINT                               -- codigo del proceso
PRIVATE DEFINE p_opera_cod                SMALLINT                               -- codigo de la operacion
PRIVATE DEFINE p_usuario_cod              CHAR(20)                               -- clave del usuario firmado
PRIVATE DEFINE v_folio                    DECIMAL(9,0)

PRIVATE DEFINE v_proceso_desc             CHAR(40)
PRIVATE DEFINE v_opera_desc               CHAR(40)

MAIN
   
   -- Datos del proceso
   DEFINE v_estado               SMALLINT
   DEFINE r_resultado_opera      INTEGER

   CALL ARG_VAL(1) RETURNING p_usuario_cod
   CALL ARG_VAL(2) RETURNING p_pid
   CALL ARG_VAL(3) RETURNING p_proceso_cod
   CALL ARG_VAL(4) RETURNING p_opera_cod
   CALL ARG_VAL(5) RETURNING v_folio

   WHENEVER ERROR CONTINUE

   -- Log del proceso
   CALL STARTLOG(p_usuario_cod CLIPPED||".RETP414.log")

   --Recupero informacion necesaria del proceso y rutas de trabajo
   --Descripción del proceso
   SELECT proceso_desc
     INTO v_proceso_desc
     FROM cat_proceso
    WHERE proceso_cod = p_proceso_cod

   --Descripcion de la operacion
   SELECT opera_desc
     INTO v_opera_desc
     FROM cat_operacion
    WHERE proceso_cod = p_proceso_cod
      AND opera_cod   = p_opera_cod

   --Encabezado para el archivo de monitoreo
   DISPLAY "*******************************************************************"
   DISPLAY " PROCESO            : ",v_proceso_desc
   DISPLAY " OPERACIÓN          : ",v_opera_desc
   DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY "*******************************************************************"

   -- Se solicita el numero de folio asociado a la operacion. Parametros: proceso, operacion, usuario
   CALL fn_genera_folio(p_proceso_cod,p_opera_cod,p_usuario_cod)
   RETURNING v_folio

   --Se actualiza el folio del proceso
   UPDATE bat_ctr_proceso SET folio = v_folio WHERE pid = p_pid

   UPDATE bat_ctr_operacion SET folio = v_folio WHERE pid = p_pid

   --Realizo el proceso de Desmarcas
   CALL fn_notificar_procesar() RETURNING v_estado

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
   DISPLAY "Terminó el Batch de Consultas a Procesar"
   DISPLAY "*******************************************************************"
   DISPLAY " PROCESO            : ",v_proceso_desc
   DISPLAY " OPERACIÓN          : ",v_opera_desc
   DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY "*******************************************************************"

   WHENEVER ERROR STOP

END MAIN


FUNCTION fn_notificar_procesar()

   -- Variables de control
   DEFINE
      v_solicitudes_procesadas   INTEGER,
      v_estado                   SMALLINT,
      reg_marca_procesar_maestra RECORD
         id_solicitud            DECIMAL(9,0),
         nss                     CHAR(11),
         estado_indicador        SMALLINT,
         origen                  SMALLINT
      END RECORD,
      v_diagnostico              SMALLINT,
      v_estatus                  SMALLINT,
      v_aivs_viv92               DECIMAL(24,6),
      v_pesos_viv92              DECIMAL(22,2),
      v_aivs_viv97               DECIMAL(24,6),
      v_pesos_viv97              DECIMAL(22,2),
      --
      v_caso_crm                 CHAR(10),
      v_saldo_total              DECIMAL(24,6),
      v_estatus_marca            SMALLINT,
      v_id_derechohabiente       DECIMAL(9,0),
      v_rfc                      CHAR(13),
      v_modalidad                SMALLINT,
      v_cod_rechazo              SMALLINT,
      v_res_actualizacion        SMALLINT,
      v_cod_inconsistencia       SMALLINT

   -- Variables auxiliares
   DEFINE v_sql                      STRING

    
    -- Se declara el cursor sobre las solicitudes.
   LET v_sql =   " SELECT r.id_solicitud, r.nss, r.estado_indicador, r.origen ",
                 " FROM ret_ctr_marca_procesar_maestra r,ret_ley73_generico s ",
                 " WHERE r.id_solicitud = s.id_solicitud ",
                 " AND r.estado_indicador = 7 ",
                 " AND s.estado_solicitud = 12 "
   
   LET v_solicitudes_procesadas = 0

   PREPARE prp_solicitudes_consulta_pendientes FROM v_sql
   DECLARE cur_solicitudes_consulta_pendientes CURSOR FOR prp_solicitudes_consulta_pendientes

   FOREACH cur_solicitudes_consulta_pendientes INTO reg_marca_procesar_maestra.id_solicitud,
   reg_marca_procesar_maestra.nss,reg_marca_procesar_maestra.estado_indicador,
   reg_marca_procesar_maestra.origen

      SELECT id_derechohabiente, rfc, modalidad_retiro
      INTO v_id_derechohabiente, v_rfc, v_modalidad
      FROM ret_solicitud_generico
      WHERE nss = reg_marca_procesar_maestra.nss
      AND id_solicitud = reg_marca_procesar_maestra.id_solicitud

      SELECT UNIQUE caso_crm
      INTO v_caso_crm
      FROM ret_ctr_marca_procesar			
      WHERE nss = reg_marca_procesar_maestra.nss
      AND id_solicitud = reg_marca_procesar_maestra.id_solicitud

      --Mando llamar funcionalidad de Desmarca a Procesar
      CALL fn_consulta_saldo_vivienda_afore(reg_marca_procesar_maestra.nss, 30)
           RETURNING v_diagnostico, v_estatus, v_aivs_viv92, v_pesos_viv92, v_aivs_viv97, v_pesos_viv97, v_cod_rechazo
      IF (v_diagnostico = 127) THEN
         --Existio un error desde Procesar
         CALL fn_guarda_consulta_ws_vent_afore(reg_marca_procesar_maestra.nss, 3, 7, TODAY, CURRENT HOUR TO SECOND, v_diagnostico, v_estatus,
                                               v_aivs_viv92, v_aivs_viv97, 'OPSISSACI', reg_marca_procesar_maestra.id_solicitud, v_caso_crm, 2) -- Se deberá reenviar la solicitud de marca 
      ELSE
         CALL fn_guarda_consulta_ws_vent_afore(reg_marca_procesar_maestra.nss, 3, 3, TODAY, CURRENT HOUR TO SECOND, v_diagnostico, v_estatus,
                                               v_aivs_viv92, v_aivs_viv97, 'OPSISSACI', reg_marca_procesar_maestra.id_solicitud, v_caso_crm, 2) -- La solicitud no se pudo marcar porque esta marcada en otro proceso
      END IF

      LET v_saldo_total = v_aivs_viv92 + v_aivs_viv97
      --En caso de que haya sido satisfactoria la comunicación actualizare a estado marca 15
      IF v_diagnostico = 127 THEN 
          LET v_estatus_marca = 12    -- Se reenviará la solicitud de saldo
          LET v_cod_rechazo   = 0
      ELSE 
          IF v_diagnostico = 101 AND v_estatus = 101 THEN 
              IF v_saldo_total = 0 THEN  --- Se debe desmarcar en SACI y en Procesar
                  -- rechazo de solicitud
                  LET v_estatus_marca = 100
                  
                  -- Reviso que no exista una solicitud de marca pendiente
                  SELECT *
                  FROM ret_ctr_marca_procesar_maestra
                  WHERE nss = p_nss
                  AND id_solicitud = reg_marca_procesar_maestra.id_solicitud
                  AND estado_indicador = 1         --No marcado
                  --
                  IF SQLCA.sqlcode = NOTFOUND THEN
                     CALL fn_consulta_saldo_vivienda_afore(reg_marca_procesar_maestra.nss, 60)  -- Desmarca en Procesar
                          RETURNING v_diagnostico, v_estatus, v_aivs_viv92, v_pesos_viv92, v_aivs_viv97, v_pesos_viv97, v_cod_rechazo
                     IF v_diagnostico = 127 THEN 
                         CALL fn_guarda_consulta_ws_vent_afore(reg_marca_procesar_maestra.nss, 2, 4, TODAY, CURRENT HOUR TO SECOND, v_diagnostico, v_estatus,
                                                               v_aivs_viv92, v_aivs_viv97, 'OPSISSACI', reg_marca_procesar_maestra.id_solicitud, v_caso_crm, 2) -- Se deberá reenviar la solicitud de marca 
                         LET v_cod_inconsistencia = v_cod_rechazo
                     ELSE 
                         CALL fn_guarda_consulta_ws_vent_afore(reg_marca_procesar_maestra.nss, 2, 5, TODAY, CURRENT HOUR TO SECOND, v_diagnostico, v_estatus,
                                                               v_aivs_viv92, v_aivs_viv97, 'OPSISSACI', reg_marca_procesar_maestra.id_solicitud, v_caso_crm, 2) -- Se deberá reenviar la solicitud de marca 
                     END IF
                  ELSE
                     --Ya no envío Desmarca a Procesar
                     CALL fn_guarda_consulta_ws_vent_afore(reg_marca_procesar_maestra.nss, 2, 6, TODAY, CURRENT HOUR TO SECOND, v_diagnostico, v_estatus,
                                                           v_aivs_viv92, v_aivs_viv97, 'OPSISSACI', reg_marca_procesar_maestra.id_solicitud, v_caso_crm, 2) -- Se deberá reenviar la solicitud de marca 
                  END IF
              ELSE 
                  -- Actualiza los saldos con los devueltos por el WS de Consulta de Procesar
                  UPDATE ret_ley73_generico
                  SET    aivs_viv92 = v_aivs_viv92,
                         aivs_viv97 = v_aivs_viv97
                  WHERE  id_referencia = reg_marca_procesar_maestra.id_solicitud
                  
                  LET v_estatus_marca = 15
                  LET v_cod_rechazo   = 0
                  
              END IF 
          ELSE 
              ---  Aun esta pendiente la definicion, ya que el WS devolverá saldo cuando ya se encuentre marcada la cuenta
              LET v_estatus_marca = 100 --(Se rechazara)    -- Se dejará fluir la solicitud
              --LET v_cod_rechazo   = 0
          END IF 
      END IF
      
      -- Actualiza tabla de retiro genérico   
      CALL fn_actualiza_retiro_generico(v_id_derechohabiente, reg_marca_procesar_maestra.nss,
                                        v_rfc, v_modalidad, reg_marca_procesar_maestra.id_solicitud, 
                                        v_folio, v_estatus_marca, v_cod_rechazo, 2)
                              RETURNING v_res_actualizacion

      LET v_solicitudes_procesadas = v_solicitudes_procesadas + 1
    
   END FOREACH

   DISPLAY "Se mandaron consultar a Procesar ", v_solicitudes_procesadas, " solicitudes correctamente."

   RETURN v_estado

END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_actualiza_retiro_generico
Fecha creacion: Septiembre 25, 2013
Autor: Esteban Sánchez Zepeda, EFP
Narrativa del proceso que realiza:
Ejecuta la actualización de la respuesta del marcaje en la tabla de retiro genérico


Registro de modificaciones:
Autor         Fecha       Descrip. cambio
Ivan Vega     26 Nov 2013  - La solicitud se crea con grupo de ventanilla
                             infonavit
Eneas Armas   20140122   Se cambia la tabla ret_fondo_ahorro por ret_fondo_ahorro_generico
              20140122   Se cambia la tabla ret_ley73 por ret_ley73_generico
======================================================================
}
FUNCTION fn_actualiza_retiro_generico(p_id_derechohabiente, p_nss, p_rfc, p_modalidad_retiro, p_id_solicitud,
                                      p_folio, p_estado_solicitud, p_cod_rechazo, p_tipo_actualizacion)
DEFINE p_id_derechohabiente       DECIMAL(9,0),
       p_nss                      CHAR(11),
       p_rfc                      CHAR(13),
       p_modalidad_retiro         SMALLINT,
       p_id_solicitud             DECIMAL(9,0),
       p_folio                    DECIMAL(9,0),
       p_estado_solicitud         SMALLINT,
       p_cod_rechazo              SMALLINT,
       p_tipo_actualizacion       SMALLINT, --Variable que recibe el tipo de actualización(INSERT,UPDATE,DELETE)
       v_sql                      STRING,
       v_respuesta                SMALLINT,
       v_r_ret_solicitud_generico RECORD LIKE ret_solicitud_generico.* -- registro de solicitud
   
   -- Valida el tipo de actualización 1-Inserción, 2-Actualización
   CASE p_tipo_actualizacion
   	
      {WHEN 1
   		   -- Arma el query INSERCIÓN
         LET v_r_ret_solicitud_generico.id_solicitud           = p_id_solicitud
         LET v_r_ret_solicitud_generico.id_derechohabiente     = p_id_derechohabiente
         LET v_r_ret_solicitud_generico.nss                    = p_nss
         LET v_r_ret_solicitud_generico.rfc                    = p_rfc
         LET v_r_ret_solicitud_generico.modalidad_retiro       = p_modalidad_retiro
         LET v_r_ret_solicitud_generico.folio                  = p_folio
         LET v_r_ret_solicitud_generico.caso_adai              = ret_marcaje.caso_adai
         LET v_r_ret_solicitud_generico.id_archivo_envio       = 0
         LET v_r_ret_solicitud_generico.id_archivo_respuesta   = 0
         LET v_r_ret_solicitud_generico.folio_restitucion      = 0
         LET v_r_ret_solicitud_generico.id_archivo_cancela_cxp = 0
         LET v_r_ret_solicitud_generico.id_archivo_resp_cxp    = 0
         LET v_r_ret_solicitud_generico.folio_afore            = NULL -- se usa en ventanilla afore
         LET v_r_ret_solicitud_generico.grupo_ventanilla       = gi_ventanilla_infonavit -- solicitud iniciada en infonavit
         LET v_r_ret_solicitud_generico.f_solicitud            = TODAY
         LET v_r_ret_solicitud_generico.h_solicitud            = CURRENT HOUR TO SECOND
         LET v_r_ret_solicitud_generico.estado_solicitud       = p_estado_solicitud
         LET v_r_ret_solicitud_generico.cod_rechazo            = p_cod_rechazo
   		   
         -- se inserta el registro
         INSERT INTO ret_solicitud_generico VALUES ( v_r_ret_solicitud_generico.* )
                                            
         -- Valida que la ejecución del insert haya sido correcta
         IF ( SQLCA.SQLCODE < 0 ) THEN
            -- Asigna respuesta negativa
            LET v_respuesta = FALSE
            CALL ERRORLOG("Se ha producido un error al insertar el registro en retiro genérico")
         ELSE
            -- Asigna respuesta afirmativa
            LET v_respuesta = TRUE
            CALL ERRORLOG("Se ha insertado con éxito el registro en retiro genérico")
         END IF}
                                            
                
      WHEN 2
      
         -- si no llego el codigo de rechazo, se define el generico
         IF ( p_cod_rechazo IS NULL ) THEN
            LET p_cod_rechazo = 54 -- rechazo generico. CAMBIAR POR CONSTANTE GLOBAL
         END IF
      
         -- Arma el query ACTUALIZACION
         LET v_sql = "\nUPDATE ret_solicitud_generico",
                     "\nSET    estado_solicitud   =  ", p_estado_solicitud, ",",
                     "\n       cod_rechazo        =  ", p_cod_rechazo,
                     "\nWHERE  id_derechohabiente =  ", p_id_derechohabiente,
                     "\nAND    modalidad_retiro   =  ", p_modalidad_retiro,
                     "\nAND    id_solicitud       =  ", p_id_solicitud
                      
         PREPARE stm_update_retiro FROM v_sql
         EXECUTE stm_update_retiro 
                                           
         -- Valida que la ejecución del insert haya sido correcta
         IF ( SQLCA.SQLCODE < 0 ) THEN
         	  -- Asigna respuesta negativa
         	  LET v_respuesta = FALSE
         	  CALL ERRORLOG("Se ha producido un error al actualizar el registro en retiro genérico")
         ELSE
            -- Asigna respuesta afirmativa
            LET v_respuesta = TRUE
            CALL ERRORLOG("Se ha actualizado con éxito el registro en retiro genérico")
            
            -- se actualiza la tabla de historicos con el estado de solicitud y cogido de rechazo segun la modalidad
            -- 20140122   Se cambia la tabla ret_fondo_ahorro por ret_fondo_ahorro_generico
            -- 20140122   Se cambia la tabla ret_ley73 por ret_ley73_generico
            CASE p_modalidad_retiro
               WHEN 2 -- fondo de ahorro               
                  UPDATE ret_fondo_ahorro_generico
                  SET    estado_solicitud = p_estado_solicitud,
                         cod_rechazo      = p_cod_rechazo
                  WHERE  id_solicitud     = p_id_solicitud
                  
                  -- si se trata de un rechazo, se desmarca
                  IF ( p_estado_solicitud = 100 ) THEN
                     CALL fn_ret_generico_desmarca_cuenta(p_id_derechohabiente, 802, 
                                                          p_id_solicitud, 802,
                                                          "safreviv", g_proceso_cod_ret_fondo_ahorro)
                  END IF
               
               WHEN 3 -- ley 73
                  UPDATE ret_ley73_generico
                  SET    estado_solicitud = p_estado_solicitud,
                         cod_rechazo      = p_cod_rechazo
                  WHERE  id_solicitud     = p_id_solicitud
                  
                  -- si se trata de un rechazo, se desmarca
                  IF ( p_estado_solicitud = 100 ) THEN
                     CALL fn_ret_generico_desmarca_cuenta(p_id_derechohabiente, 803,
                                                          p_id_solicitud, 803,
                                                          "safreviv", g_proceso_cod_ret_ley73_ws)
                  END IF
               
               WHEN 9 -- amortizaciones excedentes
                  UPDATE ret_amort_excedente
                  SET    estado_solicitud = p_estado_solicitud,
                         cod_rechazo      = p_cod_rechazo
                  WHERE  id_solicitud     = p_id_solicitud
                  
                  -- si se trata de un rechazo, se desmarca
                  IF ( p_estado_solicitud = 100 ) THEN
                     CALL fn_ret_generico_desmarca_cuenta(p_id_derechohabiente, 810, 
                                                          p_id_solicitud, 810,
                                                          "safreviv", g_proceso_cod_ret_amort_excedentes)
                  END IF

               
               WHEN 10 -- aportaciones voluntarias
                  UPDATE ret_voluntaria
                  SET    estado_solicitud = p_estado_solicitud,
                         cod_rechazo      = p_cod_rechazo
                  WHERE  id_solicitud     = p_id_solicitud
                  
                  -- si se trata de un rechazo, se desmarca
                  IF ( p_estado_solicitud = 100 ) THEN
                     CALL fn_ret_generico_desmarca_cuenta(p_id_derechohabiente, 809, 
                                                          p_id_solicitud, 809,
                                                          "safreviv", g_proceso_cod_ret_aport_voluntarias)
                  END IF
            END CASE
         END IF
      {WHEN 3
      
      	 -- Arma el query ELIMINACIÓN
         LET v_sql = "\n DELETE FROM ret_solicitud_generico",
                     "\n WHERE  id_derechohabiente = ", p_id_derechohabiente,
                     "\n AND    modalidad_retiro   = ", p_modalidad_retiro,
                     "\n AND    id_solicitud       = ", p_id_solicitud
                      
         PREPARE stm_delete_retiro FROM v_sql
         EXECUTE stm_delete_retiro 
               
         --Valida que la ejecución del insert haya sido correcta
         IF ( SQLCA.SQLCODE < 0 ) THEN
            -- Asigna respuesta negativa
            LET v_respuesta=FALSE
            CALL ERRORLOG("Se ha producido un error al eliminar el registro en retiro genérico")
         ELSE
         	
            -- Asigna respuesta afirmativa
            LET v_respuesta = TRUE
            CALL ERRORLOG("Se ha eliminado con éxito el registro en retiro genérico")
         END IF}              
   END CASE
   
   -- Regresa respuesta
   RETURN v_respuesta
END FUNCTION                  
