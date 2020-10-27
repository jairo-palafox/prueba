################################################################################
#Versi�n                    => 1.0.0                                           #
#Fecha �ltima modificacion  => 20/08/2012                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => CNT                                                      #
#Programa          => CNTE02                                                   #
#Objetivo          => Confirma p�liza contable                                 #
#Fecha inicio      => 20/08/2012                                               #
################################################################################

IMPORT os

DATABASE safre_viv
GLOBALS "CNTG01.4gl"

GLOBALS

   DEFINE 
      p_proceso_cod        LIKE cat_proceso.proceso_cod,
      p_opera_cod          LIKE cat_operacion.opera_cod,
      p_usuario            LIKE seg_usuario.usuario_cod, 
      p_nom_archivo        STRING,
      p_pid                LIKE bat_ctr_proceso.pid,
      p_folio              DECIMAL (9,0)
      
END GLOBALS 

MAIN 

   --Define las variables de la funci�n
   DEFINE 
      v_texto_linea        VARCHAR (65),
      v_info               CHAR (4),
      v_num_poliza         CHAR (10),
      v_ejercicio          CHAR (4),
      v_folio_ref          CHAR (7),
      v_fecha_hoy          CHAR (10),
      v_fecha_docto        CHAR (8),
      v_fecha_docto_fmt    CHAR (10),
      v_error_docto        VARCHAR (200),
      v_contados           INTEGER,
      v_query              STRING,
      v_referencia         CHAR (16),
      v_carga_archivo      SMALLINT,
      v_confirmados        INTEGER,
      v_folio_cnt_reg      LIKE cnt_transaccion.folio_cnt,
      v_folio_existe       DECIMAL (9,0),
      r_bandera            SMALLINT 



   --Asignaci�n de parametros generales 
   LET p_usuario = ARG_VAL(1)
   LET p_pid = ARG_VAL(2) 
   LET p_proceso_cod = ARG_VAL(3) 
   LET p_opera_cod = ARG_VAL(4)
   LET p_folio = ARG_VAL(5)
   LET p_nom_archivo = ARG_VAL(6)


      
      CALL STARTLOG("SAFREVIV.CNTE02.log")
      
      DISPLAY "Inicia confirmaci�n..."
      
            SELECT COUNT (*)
            INTO v_contados
            FROM safre_tmp:tmp_cnt_confirma

      DISPLAY "Registros a confirmar: ",v_contados
      
            --Si se carg� informaci�n, contin�a el proceso
            IF v_contados > 0 THEN 
               LET v_carga_archivo = TRUE 
               
            ELSE 
               LET v_carga_archivo = FALSE 
               DISPLAY "No hay registros a confirmar" 
            END IF  
                  

            --DISPLAY "v_carga_archivo -- ",v_carga_archivo
            IF v_carga_archivo = FALSE THEN
                --CALL fn_mensaje("Atenci�n","Carga Cancelada","about")
                DISPLAY "Atenci�n - No hay registros para confirmar la p�liza contable"
                --CALL fn_error_opera(v_pid,g_proceso_cod,g_opera_cod2) RETURNING v_carga_archivo
                EXIT PROGRAM 
            END IF  


            --Asigna la fecha del d�a
            LET v_fecha_hoy = TODAY --USING "mm/dd/yyyy"
            --DISPLAY "v_fecha_hoy --> ", v_fecha_hoy

            
            ### Adecuaci�n de la confirmaci�n de la p�liza con el nuevo layout de un registro
            ### para cada proceso

            LET v_confirmados = 1 --Inicializa variable
            
            LET v_query =  "\n SELECT   confirmacion, fec_docto, referencia ",
                           "\n FROM     safre_tmp:tmp_cnt_confirma   "

            --DISPLAY "v_query -- ",v_query


            DISPLAY " \n \n###########  INFORMACI�N DE P�LIZA CONTABLE  ########### " 

            
            PREPARE prp_reenvio FROM v_query
            DECLARE cur_reenvio CURSOR FOR prp_reenvio

            FOREACH  cur_reenvio INTO v_texto_linea, v_fecha_docto, v_referencia

               #### Insertar aqu� la l�gica de negocio ###

						--Asigna los valores del campo confirmacion para despu�s evaluar si es exitosa
						LET v_info = v_texto_linea[1,4]
						--DISPLAY "v_info", v_info

						--Le da formato especificado a la fecha del documento
						LET v_fecha_docto_fmt = v_fecha_docto[5,6] , "/" , v_fecha_docto[7,8], "/", v_fecha_docto[1,4]
						--DISPLAY "v_fecha_docto_fmt --> ", v_fecha_docto_fmt

						--Obtiene el folio del documento *** Se cambia a folio de liquidaci�n
						LET v_folio_ref = v_referencia[11,16]
						--DISPLAY "v_folio_ref --> ", v_folio_ref


                  --Valida que el folio exista en cnt_transaccion, de no ser as�,
                  --env�a mensaje y contin�a con el siguiente registro.

                  SELECT MAX(folio_liquida)
                  INTO   v_folio_existe
                  FROM   cnt_transaccion
                  WHERE  folio_liquida = v_folio_ref
                  AND    estado       IN (20,40,70)

                  --DISPLAY "v_folio_existe -- ",v_folio_existe
                  IF v_folio_existe IS NULL OR v_folio_existe = 0 THEN 
                     
                  
                     --IF v_folio_cnt_reg IS NULL THEN 
                        DISPLAY "INFO: No existe registro contable para el folio de liquidaci�n: ",v_folio_ref
                        CONTINUE FOREACH; 
                     --END IF 

                  END IF 

		  --Recupera folio contable
		  SELECT folio_cnt 
		  INTO   v_folio_cnt_reg
		  FROM   cnt_transaccion
		  WHERE  folio_liquida = v_folio_ref 
        AND    estado       IN (20,40,70)
		  GROUP BY folio_cnt


		  --Eval�a si la confirmaci�n es exitosa
		  IF v_info = "INFO" THEN
		     DISPLAY " La confirmaci�n fue exitosa para el folio de liquidaci�n: ",v_folio_ref
		
	             --Obtiene el n�mero de p�liza del archivo
	             LET v_num_poliza = v_texto_linea[5,14]
	             --DISPLAY "v_num_poliza --> ", v_num_poliza
		             	
	             --Obtiene el ejercicio contable
	             LET v_ejercicio = v_texto_linea[15,18]
	             --DISPLAY "v_ejercicio --> ", v_ejercicio
								   
                           --DISPLAY "Folio liquida ", v_folio_ref
                           --DISPLAY "Fecha liquida ",v_fecha_docto_fmt
                           
								   --Actualiza cnt_ctr_proceso cuando fue exitoso
                           --DISPLAY " ### Actualiza tabla de control ###"
								   UPDATE   cnt_ctr_proceso
								   SET      num_poliza = v_num_poliza,
                                    ejercicio = v_ejercicio,
                                    f_respuesta = v_fecha_hoy,
                                    estado = 30
								   WHERE    folio_liquida = v_folio_ref
                           --AND      f_liquida =  v_fecha_docto_fmt
                           AND      estado IN (20,40,70)
								   --AND      f_emision =  v_fecha_docto_fmt
								   --AND      folio_cnt = v_folio_ref
								   
								   
								   --Actualiza cnt_transaccion cuando fue exitoso
                           --DISPLAY " ### Actualiza tabla de transacciones ###"
								   UPDATE   cnt_transaccion
								   SET      estado = 30
								   WHERE    folio_liquida = v_folio_ref
                          -- AND      f_liquida = v_fecha_docto_fmt
                           AND      estado IN (20,40,70)
								   --AND      f_emision = v_fecha_docto_fmt
								   --AND      folio_cnt = v_folio_ref
								   

						ELSE
							DISPLAY " La confirmaci�n fue fallida para el folio de liquidaci�n: ",v_folio_ref
							
								   --Obtiene la descripci�n del error
								   LET v_error_docto =  v_texto_linea
								   --DISPLAY "v_error_docto --> ", v_error_docto
							

								   --Actualiza cnt_ctr_proceso cuando existe error
								   UPDATE   safre_viv:cnt_ctr_proceso
								   SET      estado = 60,
                                    f_respuesta = v_fecha_hoy
								   WHERE    folio_liquida = v_folio_ref
                           --AND      f_liquida = v_fecha_docto_fmt
								   AND      estado IN (20,40,70)
								   
								   
								   --Actualiza cnt_transaccion cuando existe error
								   --DATABASE safre_viv
								   UPDATE   safre_viv:cnt_transaccion
								   SET      estado = 60
								   WHERE    folio_liquida = v_folio_ref
                           --AND      f_liquida = v_fecha_docto_fmt
								   AND      estado IN (20,40,70)
								   
								   
								   --Inserta en cnt_error_poliza un registro del error informado por el sistema SAP-FICO
								   INSERT INTO safre_viv:cnt_error_poliza
								   VALUES (v_folio_cnt_reg, v_folio_ref, v_fecha_docto_fmt, v_fecha_docto_fmt, v_fecha_hoy, v_error_docto)

                     END IF 
                        
               LET v_confirmados = v_confirmados + 1


               --Actualiza estado para tener referencia contable
               UPDATE glo_folio
               SET folio_referencia = p_folio
               WHERE folio = v_folio_cnt_reg
               
            END FOREACH

   DISPLAY " ######################################################## "
   
   --Actualiza la operaci�n
   CALL fn_actualiza_opera_fin(p_pid, p_proceso_cod,p_opera_cod) RETURNING r_bandera

   

   
   --Valida Operaci�n Final
   IF r_bandera <> 0 THEN
      CALL fn_error_opera(p_pid, p_proceso_cod,p_opera_cod)
      RETURNING r_bandera
   ELSE  
      DISPLAY "Fin de la operaci�n: Se confirm� la p�liza SAP-FICO."

         -- Actualiza el estado del archivo procesado
         CALL fn_act_edo_archivo(p_nom_archivo,p_folio,2,p_usuario)
                       RETURNING r_bandera

         
   END IF 

END MAIN 
