################################################################################
#Modulo        => RET                                                          #
#Programa      => RETP415                                                      #
#Ojetivo       => Realizar la desmarca de solicitudes que tengan mas de 30 dias#
#                 con estado de precapturada.                                  #
#Fecha inicio  => Enero 28, 2016.                                              #
#Requerimiento =>                                                              #
#------------------------------------------------------------------------------#
#Fecha de modificacion =>   Modificación                                       #
#                      =>                                                      #
#                      =>                                                      #
################################################################################
GLOBALS "RETG01.4gl"

DATABASE safre_viv

#Parametros generales del proceso
PRIVATE DEFINE p_pid                      LIKE glo_pid.pid
PRIVATE DEFINE p_proceso_cod              LIKE cat_proceso.proceso_cod
PRIVATE DEFINE p_opera_cod                LIKE cat_operacion.opera_cod
PRIVATE DEFINE p_usuario_cod              LIKE seg_usuario.usuario_cod
PRIVATE DEFINE v_folio                    LIKE glo_folio.folio

PRIVATE DEFINE v_proceso_desc             LIKE cat_proceso.proceso_desc
PRIVATE DEFINE v_opera_desc               LIKE cat_operacion.opera_desc

MAIN
   
   -- Datos del proceso
   DEFINE v_estado              SMALLINT
   DEFINE r_resultado_opera     INTEGER

   CALL ARG_VAL(1) RETURNING p_usuario_cod
   CALL ARG_VAL(2) RETURNING p_pid

   CALL ARG_VAL(3) RETURNING p_proceso_cod
   CALL ARG_VAL(4) RETURNING p_opera_cod

   --Descripción del proceso
   SELECT proceso_desc
   INTO   v_proceso_desc
   FROM   cat_proceso
   WHERE  proceso_cod = p_proceso_cod

   --Descripcion de la operacion
   SELECT opera_desc
   INTO   v_opera_desc
   FROM   cat_operacion
   WHERE  proceso_cod = p_proceso_cod
     AND  opera_cod   = p_opera_cod

   --Encabezado para el archivo de monitoreo
   DISPLAY "*******************************************************************"
   DISPLAY " PROCESO            : ",v_proceso_desc
   DISPLAY " OPERACIÓN          : ",v_opera_desc
   DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY "*******************************************************************"
   DISPLAY ""
   DISPLAY ""
   DISPLAY ""

   -- Se solicita el numero de folio asociado a la operacion. Parametros: proceso, operacion, usuario
   CALL fn_genera_folio(p_proceso_cod,p_opera_cod,p_usuario_cod)
   RETURNING v_folio

   --Se actualiza el folio del proceso
   UPDATE bat_ctr_proceso   SET folio = v_folio WHERE pid = p_pid
   UPDATE bat_ctr_operacion SET folio = v_folio WHERE pid = p_pid

   --Proceso de Desmarcas
   CALL fn_solicitudes_vencidas() RETURNING v_estado

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
   DISPLAY "Terminó el Batch de Desmarcas a Procesar de solicitudes vencidas"
   DISPLAY "*******************************************************************"
   DISPLAY " PROCESO            : ",v_proceso_desc
   DISPLAY " OPERACIÓN          : ",v_opera_desc
   DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY "*******************************************************************"

END MAIN

#Funcion que filtra las solicitudes de ley 73 grupo 1 con mas de 30 dias
#con estado de capturada
FUNCTION fn_solicitudes_vencidas()

    -- Valores devueltos
    DEFINE r_estado     SMALLINT

    -- Variables auxiliares
    DEFINE v_estado                 SMALLINT
    DEFINE v_estado_indicador       SMALLINT
    DEFINE v_nss                    LIKE afi_derechohabiente.nss
    DEFINE v_sql                    STRING
    DEFINE v_cont_solicitudes       INTEGER
    DEFINE v_cont_desmarcadas       INTEGER
    DEFINE v_cont_no_desmarcadas    INTEGER
    DEFINE v_cont_sin_marca         INTEGER
    DEFINE v_cont_sin_proceso       INTEGER
    DEFINE v_id_solicitud           LIKE ret_solicitud_generico.id_solicitud

    -- Se asume que no hay error
    LET r_estado = 0

    -- Se inicializan
    LET v_cont_solicitudes    = 0
    LET v_cont_desmarcadas    = 0
    LET v_cont_no_desmarcadas = 0
    LET v_cont_sin_marca      = 0
    LET v_cont_sin_proceso    = 0
    
    LET v_sql = "SELECT a.nss, a.id_solicitud                             ",
                "FROM   ret_solicitud_generico a,                         ",
                "       ret_sol_medio_entrega b                           ",
                "WHERE  a.id_solicitud = b.id_solicitud                   ",
                "AND    a.estado_solicitud = 10                           ",
                "AND    b.medio_entrega = 5                               ",
                "AND    a.modalidad_retiro = 3                            ",
                "AND    (TODAY - a.f_solicitud) > 30                      "
                
    PREPARE prp_solicitudes FROM v_sql
    DECLARE csr_solicitudes CURSOR FOR prp_solicitudes

    FOREACH csr_solicitudes INTO v_nss,v_id_solicitud

        LET v_cont_solicitudes = v_cont_solicitudes + 1

        -- Se actualiza la solicitud
        CALL fn_actualiza_solicitud(v_nss,v_id_solicitud)

    END FOREACH

    DISPLAY ""
    DISPLAY ""
    DISPLAY ""
    DISPLAY "                           SOLCITUDES"
    DISPLAY "-------------------------------------------------------------------"
    DISPLAY "Total de solicitudes vencidas:     ",v_cont_solicitudes
    DISPLAY "Desmarcadas:                       ",v_cont_desmarcadas
    DISPLAY "No desmarcadas (pendientes):       ",v_cont_no_desmarcadas
    DISPLAY "Aun no habian sido marcadas:       ",v_cont_sin_marca
    DISPLAY "Sin proceso de marca o desmarca:   ",v_cont_sin_proceso
    DISPLAY "-------------------------------------------------------------------"
    DISPLAY ""
    DISPLAY ""
    DISPLAY ""

    RETURN r_estado

END FUNCTION

#Funcion que desmarca la cuenta y actualiza el estado de la solicitud
FUNCTION fn_actualiza_solicitud(p_nss,p_id_solicitud)

    -- Parametros recibidos
    DEFINE p_nss            LIKE afi_derechohabiente.nss
    DEFINE p_id_solicitud   LIKE ret_solicitud_generico.id_solicitud

    -- Variables para la desmarca
    DEFINE v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente
    DEFINE v_marca              LIKE sfr_marca.marca
    DEFINE v_marca_causa        LIKE sfr_marca_historica.marca_causa

    -- Variables auxiliares
    DEFINE v_sql        STRING
    --Variable para realizar el desmarcado de la solicitud
    DEFINE rec_marca    RECORD LIKE sfr_marca_activa.*
    DEFINE v_resultado  SMALLINT

    -- Se inicializa las variables para la desmarca
    LET v_marca       = 815
    LET rec_marca.marca_causa = 0

    -- Se actualiza ret_solicitud_generico
    UPDATE ret_solicitud_generico
    SET    estado_solicitud = 77
    WHERE  id_solicitud    = p_id_solicitud

    -- Se actualiza ret_ley73_generico
    UPDATE ret_ley73_generico
    SET    estado_solicitud = 77
    WHERE  id_solicitud    = p_id_solicitud

    SELECT FIRST 1 id_derechohabiente
      INTO v_id_derechohabiente
    FROM   afi_derechohabiente
    WHERE  nss = p_nss

    SELECT marca_causa
      INTO v_marca_causa
    FROM   sfr_marca_activa
    WHERE  n_referencia = p_id_solicitud
      AND  marca        = v_marca

    -- Se desmarca
    LET v_sql = "EXECUTE FUNCTION fn_desmarca_cuenta(",v_id_derechohabiente,","
                                                      ,v_marca,","
                                                      ,p_id_solicitud,","
                                                      ,"0,"
                                                      ,rec_marca.marca_causa,","
                                                      ,'"OPSISSACI",'
                                                      ,p_proceso_cod,")"

    PREPARE prp_desmarca FROM v_sql
    EXECUTE prp_desmarca INTO v_resultado

END FUNCTION


