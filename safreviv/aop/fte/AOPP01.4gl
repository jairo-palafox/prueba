#Proyecto          => SAFRE VIVIENDA 2A ETAPA                                 #
#Propietario       => E.F.P.                                                  #
#-----------------------------------------------------------------------------#
#Modulo            => AOP                                                     #
#Programa          => AOPP01                                                  #
#Objetivo          => PROGRAMA PARA PRELIQUIDAR UN REVERSO OPERATIVO POR FOLIO#
#							 COMPLETO																#
#Fecha Inicio      => 09-ABRIL-2013                                           #
###############################################################################
DATABASE safre_viv

#Parametros generales del proceso
PRIVATE DEFINE p_pid                      DECIMAL(9,0)
PRIVATE DEFINE p_proceso_cod              SMALLINT
PRIVATE DEFINE p_opera_cod                SMALLINT
PRIVATE DEFINE p_usuario_cod              CHAR(20)
PRIVATE DEFINE p_nombre_archivo           CHAR(40)
PRIVATE DEFINE v_folio                    DECIMAL(9,0)   --Folio para el ajuste operativo
PRIVATE DEFINE v_folio_ajuste             DECIMAL(9,0)   --Folio del proceso a reversar

PRIVATE DEFINE v_proceso_desc             CHAR(40)
PRIVATE DEFINE v_extension                CHAR(10)
PRIVATE DEFINE v_opera_desc               CHAR(40)
PRIVATE DEFINE v_layout                   SMALLINT
PRIVATE DEFINE v_usuario_proceso          CHAR(20)
PRIVATE DEFINE v_ruta_rescate             STRING
PRIVATE DEFINE v_ruta_listados            CHAR(40)

MAIN
   DEFINE v_resultado                     INTEGER
   DEFINE v_mensaje                       VARCHAR(100)

   DEFINE v_fn_aop_ajustar_folio          STRING


   -- se recuperan los parametros 
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET v_folio_ajuste   = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
	#LET v_folio_ajuste   = ARG_VAL(7)


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
	-- parametros: proceso, operacion, usuario
	CALL fn_genera_folio_dis(p_proceso_cod,p_opera_cod,v_folio_ajuste,p_usuario_cod)
	RETURNING v_folio

	#Se actualiza el folio del proceso               
	UPDATE bat_ctr_proceso SET folio = v_folio WHERE pid = p_pid
	UPDATE bat_ctr_operacion SET folio = v_folio WHERE pid = p_pid

	WHENEVER ERROR CONTINUE

		DISPLAY "Inicia el proceso de preliquidacion para el reverso operativo del folio ", v_folio_ajuste
		LET v_fn_aop_ajustar_folio = "EXECUTE FUNCTION fn_preliquida_reverso_operativo(?,?,?)"
		PREPARE exe_fn_aop_ajustar_folio FROM v_fn_aop_ajustar_folio
		EXECUTE exe_fn_aop_ajustar_folio USING v_folio, v_folio_ajuste, p_usuario_cod 
													INTO v_resultado, v_mensaje
		IF SQLCA.SQLCODE <> 0 THEN
			DISPLAY "Ocurrio un ERROR al intentar preliquidar el Reverso Operativo: "
			DISPLAY SQLERRMESSAGE
			CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
			RETURNING v_resultado
			
		ELSE
			#Se valida que la funcion no regrese algun error de validacion
			IF v_resultado <> 0 THEN
				#Si el resultado es distinto a cero el proceso envio un error de validacion
				DISPLAY "Ocurrio un ERROR al intentar preliquidar el reverso operativo: "
				DISPLAY v_mensaje
            CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
            RETURNING v_resultado
			ELSE
				#Si el resultado es cero significa que el proceso preliquido correctamente el folio

				# Finaliza la operacion
				CALL  fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
				RETURNING v_resultado
				IF(v_resultado <> 0)THEN         
					# Actualiza a estado erróneo
					DISPLAY "Ocurrio un ERROR al intentar actualizar el estado de la operacion..."
					CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
					RETURNING v_resultado
				END IF

            DISPLAY " "
				DISPLAY "Finalizo la ejecucion de la funcion que preliquida el reverso operativo"
            DISPLAY "Mensaje de respuesta: ", v_mensaje

				DISPLAY "*******************************************************************"
				DISPLAY ""
				DISPLAY "Termino la preliquidacion del reverso operarivo: "
				DISPLAY ""
				DISPLAY " PROCESO            : ",v_proceso_desc
				DISPLAY " OPERACIÓN          : ",v_opera_desc
				DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
				DISPLAY " HORA               : ",TIME(CURRENT)
				DISPLAY ""
				DISPLAY "*******************************************************************"
						
			END IF
		END IF
	WHENEVER ERROR STOP

END MAIN

