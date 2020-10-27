#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
#-----------------------------------------------------------------------------#
#Modulo            => CTA                                                     #
#Programa          => CTAP07                                                  #
#Objetivo          => PROGRAMA QUE INICIA LA PREPARACION DE LA TABLA DE SALDOS# 
#                     PARA EL ARCHIVO DEL AREA DE PRECALIFICACION             #
#Fecha Inicio      => 30-SEPTIEMBRE-2013                                      #
###############################################################################
DATABASE safre_viv

GLOBALS "CTAP07.inc"

#Parametros generales del proceso
PRIVATE DEFINE p_pid                      DECIMAL(9,0)                           -- PID del proceso
PRIVATE DEFINE p_proceso_cod              SMALLINT                               -- codigo del proceso
PRIVATE DEFINE p_opera_cod                SMALLINT                               -- codigo de la operacion
PRIVATE DEFINE p_usuario_cod              CHAR(20)                            -- clave del usuario firmado
PRIVATE DEFINE p_nombre_archivo           CHAR(40)                            -- nombre dle archivo
PRIVATE DEFINE v_folio                    DECIMAL(9,0)
PRIVATE DEFINE v_fcorte                   DATE

PRIVATE DEFINE v_proceso_desc             LIKE cat_proceso.proceso_desc
PRIVATE DEFINE v_extension                LIKE cat_operacion.extension
PRIVATE DEFINE v_opera_desc               LIKE cat_operacion.opera_desc
PRIVATE DEFINE v_layout                   LIKE cat_operacion.layout_cod
PRIVATE DEFINE v_usuario_proceso          LIKE seg_modulo.usuario
PRIVATE DEFINE v_ruta_rescate             STRING
PRIVATE DEFINE v_ruta_listados            LIKE seg_modulo.ruta_listados


MAIN
   DEFINE r_resultado_opera               INTEGER
   DEFINE v_estado_ant                    SMALLINT
   DEFINE v_fn_genera_saldo_preca         STRING

   DEFINE v_resultado                     SMALLINT
   DEFINE v_mensaje_respuesta             VARCHAR(100)

   DEFINE v_consulta_anterior             STRING

   DEFINE v_id_maximo                     INTEGER
   DEFINE v_intervalo                     INTEGER

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
   DISPLAY " OPERACI�N          : ",v_opera_desc
   DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY "*******************************************************************"
      
   #Primero se valida si se puede ejecutar la generacion de saldos
   --Se establece la fecha de corte como el dia natural inmediato anterior
   LET v_fcorte = TODAY - 1;
   
	LET v_consulta_anterior = "SELECT estado_genera FROM safre_sdo@vivws_tcp:glo_ctr_saldo ",
	"WHERE tpo_saldo = 3 AND f_saldo = ?"
	PREPARE exe_consulta_anterior FROM v_consulta_anterior
	EXECUTE exe_consulta_anterior USING v_fcorte INTO v_estado_ant
	
   IF v_estado_ant = 1 THEN
      DISPLAY "El proceso no se puede ejecutar porque existe una generaci�n de saldo en ejecuci�n"
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
      RETURNING r_resultado_opera
   ELSE
      IF v_estado_ant = 2 THEN
         DISPLAY "El proceso no se puede ejecutar porque el saldo para la fecha de corte ", v_fcorte USING 'dd-mm-yyyy', " ya fue generado"
         CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
         RETURNING r_resultado_opera
      ELSE   
         -- se solicita el numero de folio asociado a la operacion
         CALL fn_genera_folio(p_proceso_cod,p_opera_cod,p_usuario_cod)
         RETURNING v_folio

         #Se actualiza el folio del proceso               
         UPDATE bat_ctr_proceso SET folio = v_folio WHERE pid = p_pid

         UPDATE bat_ctr_operacion SET folio = v_folio WHERE pid = p_pid
         
         #Se genera la tabla de saldos para precalificacion
         DISPLAY "Creando la tabla de saldos..."
         WHENEVER ERROR CONTINUE
            LET v_fn_genera_saldo_preca = "EXECUTE FUNCTION fn_prepara_tabla_preca()"
            PREPARE exe_fn_genera_saldo_preca FROM v_fn_genera_saldo_preca
            EXECUTE exe_fn_genera_saldo_preca INTO v_resultado, v_mensaje_respuesta
            IF SQLCA.SQLCODE <> 0 THEN
               DISPLAY "Ocurrio un ERROR al intentar crear la tabla para el saldo de precalificacion: "
               DISPLAY SQLCA.SQLCODE
               DISPLAY SQLERRMESSAGE
               CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
               RETURNING r_resultado_opera
               CALL fn_actualiza_error()
               RETURN
            END IF
         WHENEVER ERROR STOP
			
         IF v_resultado = 1 THEN
            # Finaliza la operacion de generacion del archivo
				CALL  fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
				RETURNING r_resultado_opera
				IF(r_resultado_opera <> 0)THEN         
					# Actualiza a estado err�neo
					CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
					RETURNING r_resultado_opera
				END IF

				DISPLAY "*******************************************************************"
				DISPLAY ""
				DISPLAY " La tabla para preparar el saldo se genero correctamente"
				DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
				DISPLAY " HORA               : ",TIME(CURRENT)
				DISPLAY ""
				DISPLAY "*******************************************************************"

            SELECT MAX(id_derechohabiente)
            INTO v_id_maximo
            FROM afi_derechohabiente
            LET v_intervalo = v_id_maximo / INTERVALOS

            DISPLAY "*******************************************************************"
				DISPLAY ""
				DISPLAY " El identificador maximo de los afiliados es: "
				DISPLAY v_id_maximo
            DISPLAY ""
            DISPLAY " Se utilizaran intervalos de: "
            DISPLAY v_intervalo
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
            CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
            RETURNING r_resultado_opera
            CALL fn_actualiza_error()
         END IF
      END IF
   END IF
END MAIN

PRIVATE FUNCTION fn_actualiza_error()
   UPDATE safre_sdo@vivws_tcp:glo_ctr_saldo SET estado_genera = 3 
   WHERE tpo_saldo = 3
   AND f_saldo = v_fcorte;
END FUNCTION
