#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
#-----------------------------------------------------------------------------#
#Modulo            => CTA                                                     #
#Programa          => CTAP01                                                  #
#Objetivo          => PROGRAMA PARA GENERAR EL SALDO DIARIO EN LA INSTANCIA   #
#                     DE SALDOS                                               #
#Fecha Inicio      => 28-JUNIO-2012                                           #
###############################################################################
DATABASE safre_viv

#Parametros generales del proceso
PRIVATE DEFINE p_pid                      DECIMAL(9,0)                           -- PID del proceso
PRIVATE DEFINE p_proceso_cod              SMALLINT                               -- codigo del proceso
PRIVATE DEFINE p_opera_cod                SMALLINT                               -- codigo de la operacion
PRIVATE DEFINE p_usuario_cod              CHAR(20)                               -- clave del usuario firmado
PRIVATE DEFINE p_nombre_archivo           CHAR(40)                               -- nombre dle archivo
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
   DEFINE v_estado_ant                    SMALLINT

   DEFINE v_fecha_actual                  DATE

   DEFINE v_fn_inicializa_tabla           STRING
   DEFINE v_fn_genera_saldo               STRING
   DEFINE v_fn_estadisticas               STRING
   DEFINE v_fn_reg_rend_cnt               STRING

   DEFINE v_resultado                     SMALLINT
   DEFINE v_mensaje_respuesta             VARCHAR(100)

   DEFINE v_consulta_anterior             STRING

   DEFINE v_cod_proceso_cnt               SMALLINT
   DEFINE v_cod_proceso                   SMALLINT
   DEFINE v_transaccion                   SMALLINT

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
   LET v_fecha_actual = TODAY
   DISPLAY "*******************************************************************"
   DISPLAY " PROCESO            : ",v_proceso_desc
   DISPLAY " OPERACIÓN          : ",v_opera_desc
   DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY "*******************************************************************"

   #Primero se valida si se puede ejecutar la generacion de saldos
   --Se establece la fecha de corte con la fecha del dia natural anterior
   LET v_fcorte = TODAY - 1;
   
   DATABASE safre_sdo@vivws_tcp
      LET v_consulta_anterior = "SELECT estado_genera FROM glo_ctr_saldo ",
      "WHERE tpo_saldo = 1 AND f_saldo = ?"
      PREPARE exe_consulta_anterior FROM v_consulta_anterior
      EXECUTE exe_consulta_anterior USING v_fcorte INTO v_estado_ant
   
   IF v_estado_ant = 1 THEN
      DISPLAY "El proceso no se puede ejecutar porque existe una generación de saldo en ejecución"
      DATABASE safre_viv
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
      RETURNING r_resultado_opera
      
   ELSE
      IF v_estado_ant = 2 THEN
         DISPLAY "El proceso no se puede ejecutar porque el saldo para la fecha de corte ", v_fcorte USING 'dd-mm-yyyy', " ya fue generado"
         DATABASE safre_viv
         CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
         RETURNING r_resultado_opera
         
      ELSE
         DATABASE safre_viv
         -- se solicita el numero de folio asociado a la operacion
         -- parametros: proceso, operacion, usuario
         CALL fn_genera_folio(p_proceso_cod,p_opera_cod,p_usuario_cod)
         RETURNING v_folio

         #Se actualiza el folio del proceso               
         UPDATE bat_ctr_proceso SET folio = v_folio WHERE pid = p_pid

         UPDATE bat_ctr_operacion SET folio = v_folio WHERE pid = p_pid
         
         #Se genera la tabla de saldos diarios
         DISPLAY "Creando la tabla de saldo diario..."
         DATABASE safre_sdo@vivws_tcp
         WHENEVER ERROR CONTINUE
            LET v_fn_inicializa_tabla = "EXECUTE FUNCTION safre_sdo:fn_inicializa_tabla_saldo_diario()"
            PREPARE exe_fn_inicializa_tabla FROM v_fn_inicializa_tabla
            EXECUTE exe_fn_inicializa_tabla INTO v_resultado, v_mensaje_respuesta
            IF SQLCA.SQLCODE <> 0 THEN
               DISPLAY "Ocurrio un ERROR al intentar crear la tabla de saldo diario: "
               DISPLAY SQLERRMESSAGE
               DATABASE safre_viv
               CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
               RETURNING r_resultado_opera
               
               CALL fn_actualiza_error()
               RETURN
            END IF
         WHENEVER ERROR STOP
         DISPLAY ""
         DISPLAY " termina la ejecucion de la funcion que genera la tabla de saldos"
         IF v_resultado = 1 THEN
            #Se ejecuta la funcion que genera los saldos
            DISPLAY ""
            DISPLAY "lanzando la funcion que genera los saldos del dia"
            DATABASE safre_viv
            WHENEVER ERROR CONTINUE
               LET v_fn_genera_saldo = "EXECUTE FUNCTION safre_viv:fn_genera_saldo_diario(?)"
               PREPARE exe_fn_genera_saldo FROM v_fn_genera_saldo
               EXECUTE exe_fn_genera_saldo USING v_folio INTO v_resultado,v_mensaje_respuesta
               IF SQLCA.SQLCODE <> 0 THEN
                  DISPLAY "Ocurrio un ERROR al intentar crear el saldo diario: "
                  DISPLAY SQLERRMESSAGE
                  CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
                  RETURNING r_resultado_opera
                  CALL fn_actualiza_error()
                  RETURN
               END IF
            WHENEVER ERROR STOP
            DISPLAY ""
            DISPLAY " termina la ejecucion de la funcion que genera los saldos"
            DISPLAY "Mensaje de respuesta: ", v_mensaje_respuesta
            IF v_resultado = 0 THEN
               #Se generan los indices de la tabla de saldos
               DISPLAY ""
               DISPLAY "lanzando la funcion que genera los indices"
               DATABASE safre_sdo@vivws_tcp
               WHENEVER ERROR CONTINUE
               LET v_fn_estadisticas = "EXECUTE FUNCTION safre_sdo:fn_estadisticas_saldo_diario()"
               PREPARE exe_fn_estadisticas FROM v_fn_estadisticas
               EXECUTE exe_fn_estadisticas INTO v_resultado
               IF SQLCA.SQLCODE <> 0 THEN
                  DISPLAY "Ocurrio un ERROR al intentar crear los indices de la tabla de saldo diario: "
                  DISPLAY SQLERRMESSAGE
                  DATABASE safre_viv
                  CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
                  RETURNING r_resultado_opera
                  
                  CALL fn_actualiza_error()
                  RETURN
               END IF
               WHENEVER ERROR STOP
               DISPLAY ""
               DISPLAY " termina la ejecucion de la funcion que genera los indices"
               IF v_resultado = 0 THEN
                  DATABASE safre_viv
                  #Despues de crear el saldo diario se ejecutaran las funciones de registro contable
                  LET v_fn_reg_rend_cnt = "EXECUTE FUNCTION safre_viv:fn_reg_rend_cnt(?,?,?,?,?)"
                  PREPARE exe_fn_reg_rend_cnt FROM v_fn_reg_rend_cnt

                  LET v_cod_proceso = 2101
                  LET v_transaccion = 0

                  DISPLAY ""
                  DISPLAY "*******************************************************************"
                  DISPLAY ""
                  DISPLAY " Ejecutando las funciones de registro contable para los rendimientos..."
                  DISPLAY ""
                  LET v_cod_proceso_cnt = 7
                  EXECUTE exe_fn_reg_rend_cnt USING   v_folio, 
                                                      v_fcorte, 
                                                      v_cod_proceso_cnt, 
                                                      v_cod_proceso, 
                                                      v_transaccion
                                             INTO     v_resultado
                  IF SQLCA.SQLCODE <> 0 THEN
                     DISPLAY "Ocurrio un ERROR al intentar ejecutar la funcion de registro contable: "
                     DISPLAY SQLERRMESSAGE
                     CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
                     RETURNING r_resultado_opera
                     CALL fn_actualiza_error()
                     RETURN
                  END IF

                  DISPLAY " Termino la generacion de registro contable para los rendimientos"
                  
                  DISPLAY "*******************************************************************"
                  DISPLAY ""

                  # Finaliza la operacion
                  DATABASE safre_viv
                  CALL  fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
                  RETURNING r_resultado_opera
                  IF(r_resultado_opera <> 0)THEN         
                     # Actualiza a estado erróneo
                     DISPLAY "Ocurrio un ERROR al intentar actualizar el estado de la operacion..."
                     CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
                     RETURNING r_resultado_opera
                  END IF

                  DISPLAY "*******************************************************************"
                  DISPLAY ""
                  DISPLAY "Termino la generacion del saldo diario: "
                  DISPLAY ""
                  DISPLAY " PROCESO            : ",v_proceso_desc
                  DISPLAY " OPERACIÓN          : ",v_opera_desc
                  DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
                  DISPLAY " HORA               : ",TIME(CURRENT)
                  DISPLAY ""
                  DISPLAY "*******************************************************************"

                  CALL fn_correo_proceso(p_pid,
                                         p_proceso_cod,
                                         p_opera_cod,
                                         '',
                                         v_proceso_desc,
                                         'ID Proceso   : '||p_pid||
                                         'Proceso      : '||p_proceso_cod||
                                         'Operacion    : '||p_opera_cod||
                                         'Fecha Inicio : '||v_fecha_actual||
                                         'Fecha Fin    : '||DATE
                                         )
               ELSE
                  DISPLAY "Ocurrio un ERROR al intentar crear los indices de la tabla de saldo diario"
                  DATABASE safre_viv
                  CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
                  RETURNING r_resultado_opera
                  
                  CALL fn_actualiza_error()
               END IF
            ELSE
               DISPLAY "Ocurrio un ERROR al intentar crear el saldo diario, la funcion regreso el siguiente mensaje: "
               DISPLAY v_mensaje_respuesta
               DATABASE safre_viv
               CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
               RETURNING r_resultado_opera
               
               CALL fn_actualiza_error()
            END IF
         ELSE
            DISPLAY "Ocurrio un ERROR al intentar crear la tabla de saldo diario"
            DATABASE safre_viv
            CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
            RETURNING r_resultado_opera
            
            CALL fn_actualiza_error()
         END IF
      END IF
   END IF
END MAIN

PRIVATE FUNCTION fn_actualiza_error()
   DATABASE safre_sdo@vivws_tcp
   UPDATE glo_ctr_saldo SET estado_genera = 3 
   WHERE tpo_saldo = 1 
   AND f_saldo = v_fcorte;
   
END FUNCTION
