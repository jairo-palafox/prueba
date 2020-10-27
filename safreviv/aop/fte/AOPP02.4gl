#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
#-----------------------------------------------------------------------------#
#Modulo            => AOP                                                     #
#Programa          => AOPP02                                                  #
#Objetivo          => PROGRAMA PARA EJECUTAR EL REVERSO DE NEGOCIO            #
#Fecha Inicio      => 21-MAYO-2013                                            #
###############################################################################
DATABASE safre_viv

#Parametros generales del proceso
PRIVATE DEFINE p_pid                      DECIMAL(9,0)                           -- PID del proceso
PRIVATE DEFINE p_proceso_cod              SMALLINT                               -- codigo del proceso
PRIVATE DEFINE p_opera_cod                SMALLINT                               -- codigo de la operacion
PRIVATE DEFINE p_usuario_cod              CHAR(20)                            -- clave del usuario firmado
PRIVATE DEFINE p_nombre_archivo           CHAR(40)                            -- nombre dle archivo
PRIVATE DEFINE v_folio                    DECIMAL(9,0)

PRIVATE DEFINE v_proceso_desc             CHAR(40)
PRIVATE DEFINE v_extension                CHAR(10)
PRIVATE DEFINE v_opera_desc               CHAR(40)
PRIVATE DEFINE v_layout                   SMALLINT
PRIVATE DEFINE v_usuario_proceso          CHAR(20)
PRIVATE DEFINE v_ruta_rescate             STRING
PRIVATE DEFINE v_ruta_listados            CHAR(40)

PRIVATE DEFINE v_nombre_funcion				VARCHAR(40)
PRIVATE DEFINE v_funcion_negocio				STRING
PRIVATE DEFINE v_estado							SMALLINT
PRIVATE DEFINE v_folio_aop						LIKE glo_folio.folio_referencia			#Folio del proceso reversado
PRIVATE DEFINE v_proceso_cod_aop				LIKE glo_folio.proceso_cod					#Codigo del proceso reversado

PRIVATE DEFINE v_resultado						SMALLINT
PRIVATE DEFINE v_res_op						   SMALLINT
PRIVATE DEFINE v_respuesta						VARCHAR(100)
MAIN
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

		#Se busca el folio del proceso que se reverso
		SELECT 
			  folio_referencia
		INTO
			  v_folio_aop
		FROM glo_folio 
		WHERE folio = v_folio

		#Proceso_cod del proceso al que pertenece el folio que se reverso
		SELECT 
			  proceso_cod
		INTO
			  v_proceso_cod_aop
		FROM glo_folio 
		WHERE folio = v_folio_aop

		#Se busca el nombre de la funcion para el reverso de negocio 
		SELECT
			nombre_funcion
		INTO
			v_nombre_funcion
		FROM cat_reverso_negocio
		WHERE proceso_cod = v_proceso_cod_aop

		DISPLAY ""
		DISPLAY ""
		DISPLAY "*******************************************************************"
		DISPLAY " Inicia el proceso de reverso de negocio"
		DISPLAY " La función que se ejecutara será ", v_nombre_funcion
		DISPLAY " que corresponde al código de proceso: ", v_proceso_cod_aop
		DISPLAY " "
		DISPLAY " Los parametros que se enviaran son:"
		DISPLAY "		folio 		= ", v_folio_aop USING '#########'
		DISPLAY " 		proceso_cod = ", v_proceso_cod_aop
		DISPLAY "		usuario 		= ", p_usuario_cod
		DISPLAY " "
		DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
		DISPLAY " HORA               : ",TIME(CURRENT)
		DISPLAY "*******************************************************************"
		DISPLAY ""
		DISPLAY ""

		LET v_funcion_negocio = "EXECUTE FUNCTION ", v_nombre_funcion CLIPPED, "(?,?,?)" 
		PREPARE exe_funcion_negocio FROM v_funcion_negocio
		EXECUTE exe_funcion_negocio 	USING v_folio_aop, v_proceso_cod_aop, p_usuario_cod
												INTO v_resultado, v_respuesta
		IF SQLCA.SQLCODE <> 0 THEN
			DISPLAY "*******************************************************************"
			DISPLAY " Ocurrió un ERROR durante la ejecución de la función de reverso de negocio"
			DISPLAY " el codigo de error fue:"
			DISPLAY SQLCA.SQLCODE , " - " , SQLERRMESSAGE
			DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
			DISPLAY " HORA               : ",TIME(CURRENT)
			DISPLAY "*******************************************************************"

			LET v_estado = 4			#Estado liquidado con error en reverso de negocio
			
			UPDATE aop_ctr_ajuste SET cve_estado = v_estado 
			WHERE folio = v_folio
			AND folio_ajustado = v_folio_aop
			AND tpo_ajuste = 1

         CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod)
         RETURNING v_res_op
		ELSE
         DISPLAY " "
			DISPLAY "*******************************************************************"
			IF v_resultado = 0 THEN
				DISPLAY " El reverso de negocio se ejecutó de manera exitosa"
				LET v_estado = 3			#Estado liquidado con reverso de negocio

            DISPLAY " "
            DISPLAY " La función de reverso regreso el siguiente mensaje: "
            DISPLAY " ", v_respuesta
            DISPLAY " "
            DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
            DISPLAY " HORA               : ",TIME(CURRENT)
            DISPLAY "*******************************************************************"

            CALL fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
            RETURNING v_res_op
            IF(v_res_op <> 0)THEN         
               # Actualiza a estado erróneo
               DISPLAY "Ocurrio un ERROR al intentar actualizar el estado de la operacion..."
               CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
               RETURNING v_res_op
            END IF
         
			ELSE
				DISPLAY " ERROR: El reverso de negocio termino con errores"
				DISPLAY " "
				DISPLAY " Es necesario que se comunique con el responsable del proceso para solucionar el problema"
				DISPLAY " "
				DISPLAY " el codigo de error es: ", v_resultado
            DISPLAY " La función de reverso regreso el siguiente mensaje: "
            DISPLAY " ", v_respuesta
            DISPLAY " "
            DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
            DISPLAY " HORA               : ",TIME(CURRENT)
            DISPLAY "*******************************************************************"

            LET v_estado = 4			#Estado liquidado con error en reverso de negocio

            CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
            RETURNING v_res_op
			END IF

			UPDATE aop_ctr_ajuste 
			SET cve_estado = v_estado 
			WHERE folio = v_folio
			AND folio_ajustado = v_folio_aop
			AND tpo_ajuste = 1
						
		END IF
	WHENEVER ERROR STOP
END MAIN