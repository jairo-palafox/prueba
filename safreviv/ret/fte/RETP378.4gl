#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
#-----------------------------------------------------------------------------#
#Modulo            => RET                                                     #
#Programa          => RETP378                                                 #
#Objetivo          => PROGRAMA PARA ENVIO DE CORREOS AUTOMATICOS DE ACTAS DE  #
#                     FINIQUITOS                                              #
#Fecha Inicio      => 06-AGOSTO-2015                                          #
###############################################################################
DATABASE safre_viv

#Parametros generales del proceso
PRIVATE DEFINE p_pid                      DECIMAL(9,0)                           -- PID del proceso
PRIVATE DEFINE p_proceso_cod              SMALLINT                               -- codigo del proceso
PRIVATE DEFINE p_opera_cod                SMALLINT                               -- codigo de la operacion
PRIVATE DEFINE p_usuario_cod              CHAR(20)                               -- clave del usuario firmado
PRIVATE DEFINE p_nombre_archivo           CHAR(40)                               -- nombre dle archivo
PRIVATE DEFINE v_folio                    DECIMAL(9,0)

PRIVATE DEFINE v_proceso_desc             CHAR(40)
PRIVATE DEFINE v_opera_desc               CHAR(40)

MAIN

   DEFINE v_bnd_entre                     SMALLINT
   DEFINE v_email                         CHAR(50)
   DEFINE v_cuenta                        SMALLINT
   DEFINE r_resultado_opera               INTEGER

   -- se recuperan los parametros la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET v_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)

   WHENEVER ERROR CONTINUE

   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED || ".RETP378.log")

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
   
   --Primero reviso que existan documentos no marcados para enviar
   LET v_bnd_entre = FALSE
   
   --Se buscan los correos a enviar en base a estado de envio
   DECLARE cur_correo_busca CURSOR FOR
      SELECT bc.email,COUNT(bc.email)
        FROM ret_solicitud_generico ab,ret_sol_gen_excepcion bc
       WHERE ab.id_solicitud = bc.id_solicitud
         AND bc.estado_envio = 0
         AND bc.email IS NOT NULL
         AND ab.estado_solicitud = 700       -- Ya fueron pagadas
    GROUP BY bc.email
    ORDER BY bc.email

   FOREACH cur_correo_busca INTO v_email, v_cuenta
      LET v_bnd_entre = TRUE
      DISPLAY "Encontre correo ...  [", v_email CLIPPED, ", ", v_cuenta USING "<<<<<", " documentos]"
      --Empiezo con la generacion del correo
      CALL fn_genera_correo(v_email,p_pid,p_proceso_cod,p_opera_cod)
   END FOREACH

   FREE cur_correo_busca

   IF NOT v_bnd_entre THEN
      DISPLAY "No existen documentos por enviar."
   END IF

   --Finaliza la operacion
   CALL fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
   RETURNING r_resultado_opera
   
   IF(r_resultado_opera <> 0)THEN         
      # Actualiza a estado erróneo
      DISPLAY "Ocurrio un ERROR al intentar actualizar el estado de la operacion..."
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
      RETURNING r_resultado_opera
   END IF

   DISPLAY "Termino el envio automatico de correos"
   DISPLAY "*******************************************************************"
   DISPLAY ""
   DISPLAY " PROCESO            : ",v_proceso_desc
   DISPLAY " OPERACIÓN          : ",v_opera_desc
   DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY ""
   DISPLAY "*******************************************************************"

END MAIN

--Funcion para generar correo por la misma cuenta
FUNCTION fn_genera_correo(p_email,p_pid0,p_proceso_cod0,p_opera_cod0)
   DEFINE p_email          CHAR(50)
   DEFINE p_pid0           DECIMAL(9,0)                           -- PID del proceso
   DEFINE p_proceso_cod0   SMALLINT                               -- codigo del proceso
   DEFINE p_opera_cod0     SMALLINT                               -- codigo de la operacion
   DEFINE v_titulo         STRING
   DEFINE v_mensaje        STRING
   DEFINE v_fecha          DATE
   DEFINE v_id_solicitud   DECIMAL(9,0)
   DEFINE v_id_derechohabiente DECIMAL(9,0)
   DEFINE v_causal_excepcion   SMALLINT
   DEFINE v_rfc            CHAR(13)
   DEFINE v_nss            CHAR(11)
   DEFINE v_nombre         VARCHAR(60)
   DEFINE v_beneficiario   CHAR(50)
   DEFINE v_monto_causal   DECIMAL(10,2)
   DEFINE v_monto_tanto_adicional DECIMAL(10,2)
   DEFINE v_imp_pagar      DECIMAL(10,2)
   DEFINE v_clabe_bancaria CHAR(18)
   DEFINE v_dap            CHAR(15)
   DEFINE v_archivo        STRING
   DEFINE v_arr_adjuntos   DYNAMIC ARRAY OF STRING
   DEFINE v_cta_arc        SMALLINT
   DEFINE r_resultado_opera INTEGER

   CALL v_arr_adjuntos.clear()
   LET v_cta_arc = 0       --Inicializo
   
   --Obtengo Id_solicitud por correo, fecha y estado de envio
   DECLARE cur_email_deter CURSOR FOR
      SELECT bc.id_solicitud, ab.id_derechohabiente, ab.f_solicitud
        FROM ret_solicitud_generico ab,ret_sol_gen_excepcion bc
       WHERE ab.id_solicitud = bc.id_solicitud
         AND bc.estado_envio = 0
         AND bc.email = p_email
    ORDER BY bc.id_solicitud
    
   FOREACH cur_email_deter INTO v_id_solicitud, v_id_derechohabiente, v_fecha

      LET v_cta_arc = v_cta_arc + 1
   
      --Obtengo Causal
      SELECT bc.causal_excepcion
        INTO v_causal_excepcion
        FROM ret_sol_gen_excepcion ac,ret_causal_excepcion bc
       WHERE ac.causal_excepcion = bc.causal_excepcion
         AND ac.id_solicitud = v_id_solicitud

      --Obtengo NSS, RFC y Nombre completo 
      SELECT nss, rfc, nombre
        INTO v_nss, v_rfc, v_nombre
        FROM afi_fondo72
       WHERE id_derechohabiente = v_id_derechohabiente

      --Obtengo el nombre del beneficiario
      SELECT nombre
        INTO v_beneficiario
        FROM ret_beneficiario_generico
       WHERE id_solicitud = v_id_solicitud

      --Obtengo Monto normal y Monto adicional
      SELECT saldo_viv72, tanto_adicional
        INTO v_monto_causal, v_monto_tanto_adicional
        FROM ret_fondo_ahorro_generico
       WHERE id_solicitud = v_id_solicitud

      --Generare Total a pagar
      LET v_imp_pagar = v_monto_causal + v_monto_tanto_adicional

      --Obtengo CLABE
      INITIALIZE v_clabe_bancaria TO NULL
      SELECT cuenta_clabe
        INTO v_clabe_bancaria
        FROM ret_pago_spei
       WHERE id_solicitud = v_id_solicitud

      --Obtengo Referencia DAP
      SELECT cve_referencia
        INTO v_dap
        FROM ret_pago_dap
       WHERE id_solicitud = v_id_solicitud

      --Genero el archivo correspondiente
      CALL fn_genera_reporte_acta_finiquito(v_id_solicitud,v_fecha,v_causal_excepcion,v_nombre,v_beneficiario,
      v_rfc,v_nss,v_monto_causal,v_monto_tanto_adicional,v_imp_pagar,v_clabe_bancaria,v_dap,0) RETURNING v_archivo

      CALL v_arr_adjuntos.appendElement()
      LET v_arr_adjuntos[v_arr_adjuntos.getLength()] = v_archivo

      --Actualizo bandera de envio de correo
      UPDATE ret_sol_gen_excepcion
         SET estado_envio = 1
       WHERE id_solicitud = v_id_solicitud

      IF SQLCA.SQLCODE <> 0 THEN
         DISPLAY "Ocurrio un ERROR al intentar cambiar el estado del correo para la solicitud ", v_id_solicitud USING "<<<<<<<<<"
         DISPLAY SQLERRMESSAGE
         CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
         RETURNING r_resultado_opera
         RETURN
      END IF

   END FOREACH
   FREE cur_email_deter

   LET v_titulo  = "[SACI] Envio automático de actas de finiquito"
   LET v_mensaje = "Se enviaron satisfactoriamente ", v_cta_arc USING "<<<<<", " documentos."

   --Envio rutina para mandar correo con los datos extraidos
   CALL fn_correo_envio_carta_finiquito(p_email,v_titulo,v_mensaje,v_arr_adjuntos,p_pid0,p_proceso_cod0,p_opera_cod0)
END FUNCTION

--Función para envio de correos
FUNCTION fn_correo_envio_carta_finiquito(p_email,p_titulo,p_mensaje,p_arr_adjuntos,p_pid0,p_proceso_cod0,p_opera_cod0)
    DEFINE p_email          CHAR(50)
    DEFINE p_pid0           DECIMAL(9,0)
    DEFINE p_proceso_cod0   SMALLINT                               -- codigo del proceso
    DEFINE p_opera_cod0     SMALLINT                               -- codigo de la operacion
    DEFINE p_mensaje        STRING
    DEFINE p_titulo         STRING
    DEFINE p_arr_adjuntos   DYNAMIC ARRAY OF STRING
    DEFINE v_ruta_listado   CHAR(40)
    DEFINE v_comando        STRING
    DEFINE v_archivo        STRING
    DEFINE ch               base.Channel
    DEFINE i                SMALLINT

    SELECT ruta_listados
      INTO v_ruta_listado
      FROM seg_modulo
     WHERE modulo_cod = 'ret'

    LET v_archivo = v_ruta_listado CLIPPED,"/",p_pid0 USING "&&&&&&&&",
                    p_proceso_cod0 USING "&&&&&", p_opera_cod0 USING "&&&", ".txt"      --Cuerpo del correo
    
    LET ch = base.Channel.create()
    CALL ch.openFile(v_archivo,"w")
    CALL ch.write(p_mensaje)
    CALL ch.close()

   --Empezamos el comando
   LET v_comando = "mailx -s '",p_titulo.trim(),"'"
   
   --En caso de tener adjuntos anexo
   IF p_arr_adjuntos.getLength() > 0 THEN
      FOR i = 1 TO p_arr_adjuntos.getLength()
         LET v_comando = v_comando, " -a ",p_arr_adjuntos[i]
      END FOR
   END IF
   
   LET v_comando = v_comando ," ", p_email CLIPPED
   LET v_comando = v_comando,  " < ", v_archivo

   RUN v_comando
    
END FUNCTION
