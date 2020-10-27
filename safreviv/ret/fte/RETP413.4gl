################################################################################
#Modulo        => RET                                                          #
#Programa      => RETP413                                                      #
#Ojetivo       => Realizar la consulta de marca para las solicitudes con       #
#                 indicador de pendientte de desmarca a PROCESAR               #
#Elaboro       => Luis Felipe Prieto Cano                                      #
#Fecha inicio  => Enero 28, 2016.                                              #
#Requerimiento => PRODINFXVI-30                                                #
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
   CALL STARTLOG(p_usuario_cod CLIPPED||".RETP413.log")

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
   DISPLAY "Terminó el Batch de Desmarcas a Procesar"
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
      v_saldo_total              DECIMAL(24,6),
      v_caso_crm                 CHAR(10),
      v_cod_rechazo              SMALLINT

   -- Variables auxiliares
   DEFINE v_sql                      STRING

    
    -- Se declara el cursor sobre las solicitudes.
--   LET v_sql =   " SELECT a.id_solicitud, a.nss, a.estado_indicador, a.origen, b.caso_adai ",
--                 " FROM ret_ctr_marca_procesar_maestra a, ",
--                 "      ret_solicitud_generico b ",
--                 " WHERE a.id_solicitud = b.id_solicitud ",
--                 " AND   a.estado_indicador = 4 "

   LET v_sql =   " SELECT DISTINCT a.id_solicitud, a.nss, a.estado_indicador, a.origen, b.caso_adai ",
                 " FROM ret_ctr_marca_procesar_maestra a, ",
                 "      ret_solicitud_generico b, ",
                 "      ret_ctr_marca_procesar c ",
                 " WHERE a.id_solicitud     = b.id_solicitud ",
                 " AND   a.estado_indicador = 4 ",
                 " AND   a.id_solicitud     = c.id_solicitud ",
                 " AND   a.nss              = c.nss ",
                 " AND   b.caso_adai        = c.caso_crm ",
                 " AND   c.diagnostico      = 127 ",
                 " AND   c.estado_indicador = 4 "
                 
   LET v_solicitudes_procesadas = 0

   PREPARE prp_solicitudes_desmarca_pendientes FROM v_sql
   DECLARE cur_solicitudes_desmarca_pendientes CURSOR FOR prp_solicitudes_desmarca_pendientes

   FOREACH cur_solicitudes_desmarca_pendientes INTO reg_marca_procesar_maestra.id_solicitud,
                                                    reg_marca_procesar_maestra.nss,
                                                    reg_marca_procesar_maestra.estado_indicador,
                                                    reg_marca_procesar_maestra.origen,
                                                    v_caso_crm

--      SELECT UNIQUE caso_crm
--      INTO v_caso_crm
--      FROM ret_ctr_marca_procesar			
--      WHERE nss = reg_marca_procesar_maestra.nss
--      AND id_solicitud = reg_marca_procesar_maestra.id_solicitud

      --Mando llamar funcionalidad de Desmarca a Procesar
      CALL fn_consulta_saldo_vivienda_afore(reg_marca_procesar_maestra.nss, 60)
           RETURNING v_diagnostico, v_estatus, v_aivs_viv92, v_pesos_viv92, v_aivs_viv97, v_pesos_viv97, v_cod_rechazo

      LET v_saldo_total = v_aivs_viv92 + v_aivs_viv97
      IF (v_diagnostico = 127) THEN
         --Existio un error desde Procesar
         CALL fn_guarda_consulta_ws_vent_afore(reg_marca_procesar_maestra.nss, 2, 4, TODAY, CURRENT HOUR TO SECOND, v_diagnostico, v_estatus,
                                               v_aivs_viv92, v_aivs_viv97, 'OPSISSACI', reg_marca_procesar_maestra.id_solicitud, v_caso_crm, 2) -- Se deberá reenviar la solicitud de marca 
      ELSE
         CALL fn_guarda_consulta_ws_vent_afore(reg_marca_procesar_maestra.nss, 2, 5, TODAY, CURRENT HOUR TO SECOND, v_diagnostico, v_estatus,
                                               v_aivs_viv92, v_aivs_viv97, 'OPSISSACI', reg_marca_procesar_maestra.id_solicitud, v_caso_crm, 2) -- La solicitud no se pudo marcar porque esta marcada en otro proceso
      END IF

      LET v_solicitudes_procesadas = v_solicitudes_procesadas + 1
    
   END FOREACH

   DISPLAY "Se mandaron desmarcar a Procesar ", v_solicitudes_procesadas, " solicitudes correctamente."

   RETURN v_estado

END FUNCTION