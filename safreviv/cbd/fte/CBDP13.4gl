#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
#-----------------------------------------------------------------------------#
#Modulo            => CBD                                                     #
#Programa          => CBDP13                                                  #
#Objetivo          => PROGRAMA PARA GENERAR SALDOS MENSUALES                  #
#Fecha Inicio      => 27-JUNIO-2014                                           #
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
PRIVATE DEFINE v_f_inicio                 DATE

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
   DEFINE v_fn_calcula_periodo            STRING
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

   #Primero se valida si se puede ejecutar la generacion de saldos
    --Se establece la fecha de corte como el ultimo dia natural del mes inmediato anterior
   LET v_fcorte = MDY(MONTH(TODAY),1,YEAR(TODAY)) - 1
   LET v_f_inicio = MDY(MONTH(v_fcorte),1,YEAR(v_fcorte))
   DISPLAY v_fcorte
   
   -- se solicita el numero de folio asociado a la operacion
   -- parametros: proceso, operacion, usuario
   CALL fn_genera_folio(p_proceso_cod,p_opera_cod,p_usuario_cod)
   RETURNING v_folio

   #Se actualiza el folio del proceso               
   UPDATE bat_ctr_proceso SET folio = v_folio WHERE pid = p_pid

   UPDATE bat_ctr_operacion SET folio = v_folio WHERE pid = p_pid

   #Se ejecuta la funcion que genera los saldos
   DISPLAY "lanzando la funcion que genera los saldos mensuales"
   WHENEVER ERROR CONTINUE
      LET v_fn_genera_saldo = "EXECUTE FUNCTION fn_cbd_saldo_safre(?)"
      PREPARE exe_fn_genera_saldo FROM v_fn_genera_saldo
      EXECUTE exe_fn_genera_saldo USING v_fcorte INTO v_resultado,v_mensaje_respuesta
      IF SQLCA.SQLCODE <> 0 THEN
         DISPLAY "Ocurrio un ERROR al intentar crear el saldo mensual: "
         DISPLAY SQLCA.SQLCODE
         DISPLAY SQLERRMESSAGE
         CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
         RETURNING r_resultado_opera
         RETURN
      END IF
   WHENEVER ERROR STOP
            
   DISPLAY " termina la ejecucion de la funcion que genera los saldos"
   DISPLAY "Mensaje de respuesta: ", v_mensaje_respuesta
   IF v_resultado = 0 THEN
      DISPLAY ""
      DISPLAY "Lanzando la funcion que calcula los movimientos del periodo"
      WHENEVER ERROR CONTINUE
         LET v_fn_calcula_periodo = "EXECUTE FUNCTION fn_cbd_movimientos_periodo(?,?)"
         PREPARE exe_fn_calcula_periodo FROM v_fn_calcula_periodo
         EXECUTE exe_fn_calcula_periodo USING v_f_inicio, v_fcorte INTO v_resultado,v_mensaje_respuesta
         IF SQLCA.SQLCODE <> 0 THEN
            DISPLAY "Ocurrio un ERROR al intentar calcular los movimientos del periodo: "
            DISPLAY SQLCA.SQLCODE
            DISPLAY SQLERRMESSAGE
            CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
            RETURNING r_resultado_opera
            RETURN
         END IF
      WHENEVER ERROR STOP
      DISPLAY ""
      DISPLAY "termina la ejecucion de la funcion que calcula los movimientos del periodo"
      DISPLAY "Mensaje de respuesta: ", v_mensaje_respuesta
      IF v_resultado = 0 THEN
         # Finaliza la operacion de generacion del archivo
         CALL  fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
         RETURNING r_resultado_opera
         DISPLAY "*******************************************************************"
         DISPLAY ""
         DISPLAY "Termino la generacion del saldo mensual: "
         DISPLAY ""
         DISPLAY " PROCESO            : ",v_proceso_desc
         DISPLAY " OPERACIÓN          : ",v_opera_desc
         DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
         DISPLAY " HORA               : ",TIME(CURRENT)
         DISPLAY ""
         DISPLAY "*******************************************************************"

         #Se actualiza el tipo de ejecucion de la siguinete operacion para que sea automatico
         UPDATE bat_ctr_operacion SET ind_tipo_ejecucion = 1 WHERE pid = p_pid
      ELSE
         DISPLAY "Ocurrio un ERROR al calcular los movimientos del periodo"
         CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
         RETURNING r_resultado_opera
      END IF
   ELSE
      DISPLAY "Ocurrio un ERROR al generar el saldo mensual"
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
      RETURNING r_resultado_opera
      
   END IF
END MAIN