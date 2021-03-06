#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
#-----------------------------------------------------------------------------#
#Modulo            => CBD                                                     #
#Programa          => CBDP17                                                  #
#Objetivo          => PROGRAMA PARA CALCULAR LA DIFERENCIA DE SALDOS          #
#Fecha Inicio      => 23-JULIO-2014                                           #
###############################################################################
DATABASE safre_viv

#Parametros generales del proceso
PRIVATE DEFINE p_pid                      DECIMAL(9,0)                           -- PID del proceso
PRIVATE DEFINE p_proceso_cod              SMALLINT                               -- codigo del proceso
PRIVATE DEFINE p_opera_cod                SMALLINT                               -- codigo de la operacion
PRIVATE DEFINE p_usuario_cod              CHAR(20)                            -- clave del usuario firmado
PRIVATE DEFINE p_nombre_archivo           CHAR(40)                            -- nombre dle archivo
PRIVATE DEFINE v_folio                    DECIMAL(9,0)
PRIVATE DEFINE v_fcorte                   DATE

PRIVATE DEFINE v_proceso_desc             CHAR(40)
PRIVATE DEFINE v_extension                CHAR(10)
PRIVATE DEFINE v_opera_desc               CHAR(40)
PRIVATE DEFINE v_layout                   SMALLINT
PRIVATE DEFINE v_usuario_proceso          CHAR(20)
PRIVATE DEFINE v_ruta_rescate             STRING
PRIVATE DEFINE v_ruta_listados            CHAR(40)

MAIN
   DEFINE r_resultado_opera               INTEGER
   DEFINE v_fn_genera_saldo               STRING
   DEFINE v_resultado                     SMALLINT
   DEFINE v_mensaje_respuesta             VARCHAR(100)

   -- se recuperan los parametros la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET v_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)

   WHENEVER ERROR CONTINUE
   
   CALL fn_recupera_inf_proceso(p_proceso_cod, p_opera_cod) 
                               RETURNING v_proceso_desc,v_extension, 
                                         v_opera_desc,v_layout, 
                                         v_ruta_rescate,v_ruta_listados,
                                         v_usuario_proceso 

   #Encabezado para el archivo de monitoreo
   DISPLAY "*******************************************************************"
   DISPLAY " PROCESO            : ",v_proceso_desc
   DISPLAY " OPERACIÓN          : ",v_opera_desc
   DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY "*******************************************************************"

   -- se solicita el numero de folio asociado a la operacion
   CALL fn_genera_folio(p_proceso_cod,p_opera_cod,p_usuario_cod)
   RETURNING v_folio

   #Se actualiza el folio del proceso               
   UPDATE bat_ctr_proceso SET folio = v_folio WHERE pid = p_pid
   UPDATE bat_ctr_operacion SET folio = v_folio WHERE pid = p_pid

   #Se actualiza el estado del archivo BDNSVIV
   LET v_fcorte = MDY(MONTH(TODAY),1,YEAR(TODAY)) - 1
   UPDATE cbd_cza_bdnsviv SET estado = 3 WHERE f_corte = v_fcorte
   
   #Se ejecuta la funcion que genera los saldos
   DISPLAY ""
   DISPLAY "lanzando la funcion que calcula la diferencia de saldos"
   WHENEVER ERROR CONTINUE
      LET v_fn_genera_saldo = "EXECUTE FUNCTION fn_concilia_cuentas()"
      PREPARE exe_fn_genera_saldo FROM v_fn_genera_saldo
      EXECUTE exe_fn_genera_saldo INTO v_resultado,v_mensaje_respuesta
      IF SQLCA.SQLCODE <> 0 THEN
         DISPLAY "Ocurrio un ERROR en la conciliacion de saldos: "
         DISPLAY SQLCA.SQLCODE
         DISPLAY SQLERRMESSAGE
         CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
         RETURNING r_resultado_opera
         RETURN
      END IF
   WHENEVER ERROR STOP

   DISPLAY ""
   DISPLAY "Termina la ejecucion de la funcion que genera los saldos"
   DISPLAY "Mensaje de respuesta: ", v_mensaje_respuesta
   IF v_resultado = 0 THEN
      # Finaliza la operacion
      CALL  fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
      RETURNING r_resultado_opera
      
      #Se actualiza el tipo de ejecucion de la siguinete operacion para que sea automatico
      UPDATE bat_ctr_operacion SET ind_tipo_ejecucion = 1 WHERE pid = p_pid
         
      DISPLAY ""
      DISPLAY "*******************************************************************"
      DISPLAY ""
      DISPLAY "Termino el calculo de diferencia entre saldos SACI-BDNSVIV: "
      DISPLAY ""
      DISPLAY " PROCESO            : ",v_proceso_desc
      DISPLAY " OPERACIÓN          : ",v_opera_desc
      DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
      DISPLAY " HORA               : ",TIME(CURRENT)
      DISPLAY ""
      DISPLAY "*******************************************************************"
   ELSE
      DISPLAY "Ocurrio un ERROR en la conciliacion de saldos"
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
      RETURNING r_resultado_opera
      
   END IF
END MAIN