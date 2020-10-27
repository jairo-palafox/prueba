#Proyecto          => SAFRE VIVIENDA 2A ETAPA                                 #
#Propietario       => E.F.P.                                                  #
#-----------------------------------------------------------------------------#
#Modulo            => NOT                                                     #
#Programa          => NOTP05                                                  #
#Objetivo          => PROGRAMA PARA PREPARAR EL SALDO A ENVIAR		            #
#Fecha Inicio      => 25-JUNIO-2015                                           #
###############################################################################

DATABASE safre_viv

#Parametros generales del proceso
PRIVATE DEFINE p_pid                      DECIMAL(9,0)                           -- PID del proceso
PRIVATE DEFINE p_proceso_cod              SMALLINT                               -- codigo del proceso
PRIVATE DEFINE p_opera_cod                SMALLINT                               -- codigo de la operacion
PRIVATE DEFINE p_usuario_cod              CHAR(20)                            -- clave del usuario firmado
PRIVATE DEFINE p_nombre_archivo           CHAR(40)                            -- nombre dle archivo
PRIVATE DEFINE v_folio                    DECIMAL(9,0)

PRIVATE DEFINE v_proceso_desc             LIKE cat_proceso.proceso_desc
PRIVATE DEFINE v_extension                LIKE cat_operacion.extension
PRIVATE DEFINE v_opera_desc               LIKE cat_operacion.opera_desc
PRIVATE DEFINE v_layout                   LIKE cat_operacion.layout_cod
PRIVATE DEFINE v_usuario_proceso          LIKE seg_modulo.usuario
PRIVATE DEFINE v_ruta_rescate             STRING
PRIVATE DEFINE v_ruta_listados            LIKE seg_modulo.ruta_listados

MAIN
   DEFINE v_result_opera                  INTEGER
   DEFINE v_query                         STRING

   DEFINE v_resultado                     SMALLINT

   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET v_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)

   CALL fn_recupera_inf_proceso(p_proceso_cod, p_opera_cod)
                         RETURNING v_proceso_desc,v_extension,
                                   v_opera_desc,v_layout,
                                   v_ruta_rescate,v_ruta_listados,
                                   v_usuario_proceso

   DISPLAY "*******************************************************************"
   DISPLAY " PROCESO            : ",v_proceso_desc
   DISPLAY " OPERACIÓN          : ",v_opera_desc
   DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY "*******************************************************************"

   #Se valida si el mes de ejecucion en non
   IF (MONTH(TODAY) MOD 2 = 0) THEN
      #Si el resultado es cero significa que el mes es par
      DISPLAY ""
      DISPLAY "El proceso no se puede ejecutar porque el mes actual no es 'non'"
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) RETURNING v_result_opera

      DISPLAY "*******************************************************************"
      DISPLAY ""
      DISPLAY "No es posible ejecutar el proceso para crear la tabla de saldos"
      DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
      DISPLAY " HORA               : ",TIME(CURRENT)
      DISPLAY ""
      DISPLAY "*******************************************************************"
   ELSE
      #Significa que el mes es non
      
      #se solicita el numero de folio asociado a la operacion
      CALL fn_genera_folio(p_proceso_cod,p_opera_cod,p_usuario_cod)
      RETURNING v_folio

      #Se actualiza el folio del proceso
      UPDATE bat_ctr_proceso SET folio = v_folio WHERE pid = p_pid

      UPDATE bat_ctr_operacion SET folio = v_folio WHERE pid = p_pid

      WHENEVER ERROR CONTINUE
         LET v_query = "EXECUTE FUNCTION fn_genera_saldo_not()"
         PREPARE exe_fn_genera_saldo_not FROM v_query
         EXECUTE exe_fn_genera_saldo_not INTO v_resultado
         IF SQLCA.SQLCODE <> 0 THEN
            DISPLAY "Ocurrio un ERROR al intentar obtener el saldo para notificar:"
            DISPLAY SQLCA.SQLCODE
            DISPLAY SQLERRMESSAGE
            CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod)
            RETURNING v_result_opera
            RETURN
         END IF
      WHENEVER ERROR STOP

      IF v_resultado = 1 THEN
         # Finaliza la operacion de generacion del archivo
         CALL  fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
         RETURNING v_result_opera
         IF(v_result_opera <> 0)THEN
            # Actualiza a estado erróneo
            CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod)
            RETURNING v_result_opera
        END IF

        CALL fn_notifica_proceso(v_folio, p_proceso_cod, p_usuario_cod)

        DISPLAY "*******************************************************************"
        DISPLAY ""
        DISPLAY "Termino la preparacion del saldo para Notificar"
        DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
        DISPLAY " HORA               : ",TIME(CURRENT)
        DISPLAY ""
        DISPLAY "*******************************************************************"
      ELSE
         DISPLAY "*******************************************************************"
         DISPLAY ""
         DISPLAY "Ocurrio un ERROR al intentar crear la tabla de saldos"
         DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
         DISPLAY " HORA               : ",TIME(CURRENT)
         DISPLAY ""
         DISPLAY "*******************************************************************"
         CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) RETURNING v_result_opera
      END IF
   END IF
END MAIN