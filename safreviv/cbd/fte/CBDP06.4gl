#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
#-----------------------------------------------------------------------------#
#Modulo            => CBD                                                     #
#Programa          => CBDP06                                                  #
#Objetivo          => PROGRAMA PARA GENERAR SALDOS ESPECIALES EN LA INSTANCIA #
#                     DE SALDOS                                               #
#Fecha Inicio      => 20-MARZO-2014                                           #
###############################################################################
DATABASE safre_viv

#Parametros generales del proceso
PRIVATE DEFINE p_pid                      DECIMAL(9,0)                           -- PID del proceso
PRIVATE DEFINE p_proceso_cod              SMALLINT                               -- codigo del proceso
PRIVATE DEFINE p_opera_cod                SMALLINT                               -- codigo de la operacion
PRIVATE DEFINE p_usuario_cod              CHAR(20)                            -- clave del usuario firmado
PRIVATE DEFINE p_nombre_archivo           CHAR(40)                            -- nombre dle archivo
PRIVATE DEFINE v_folio                    DECIMAL(9,0)
PRIVATE DEFINE p_fcorte                   DATE

PRIVATE DEFINE v_proceso_desc             CHAR(40)
PRIVATE DEFINE v_extension                CHAR(10)
PRIVATE DEFINE v_opera_desc               CHAR(40)
PRIVATE DEFINE v_layout                   SMALLINT
PRIVATE DEFINE v_usuario_proceso          CHAR(20)
PRIVATE DEFINE v_ruta_rescate             STRING
PRIVATE DEFINE v_ruta_listados            CHAR(40)

MAIN
   DEFINE r_resultado_opera               INTEGER
   
   DEFINE v_query                         STRING
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
   LET p_fcorte         = ARG_VAL(7)


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

   CALL fn_genera_folio(p_proceso_cod,p_opera_cod,p_usuario_cod)
         RETURNING v_folio

   #Se actualiza el folio del proceso
   UPDATE bat_ctr_proceso SET folio = v_folio WHERE pid = p_pid

   UPDATE bat_ctr_operacion SET folio = v_folio WHERE pid = p_pid
   
   #Se ejecuta la funcion que genera los saldos
   DISPLAY " "
   DISPLAY " Lanzando la funcion que genera los saldos especiales con corte al ", p_fcorte USING 'dd-mm-yyyy'
      
   WHENEVER ERROR CONTINUE
      LET v_query = "EXECUTE FUNCTION fn_genera_saldo_especial(?)"
      PREPARE exe_fn_genera_saldo_mensual FROM v_query
      EXECUTE exe_fn_genera_saldo_mensual USING p_fcorte INTO v_resultado,v_mensaje_respuesta
      IF SQLCA.SQLCODE <> 0 THEN
         DISPLAY "Ocurrio un ERROR al intentar crear el saldo especial: "
         DISPLAY SQLCA.SQLCODE
         DISPLAY SQLERRMESSAGE
         CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
         RETURNING r_resultado_opera
         RETURN
      END IF
   WHENEVER ERROR STOP
      
   IF v_resultado = 1 THEN
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
      DISPLAY "Termino la generacion del saldos especiales: "
      DISPLAY ""
      DISPLAY " PROCESO            : ",v_proceso_desc
      DISPLAY " OPERACIÓN          : ",v_opera_desc
      DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
      DISPLAY " HORA               : ",TIME(CURRENT)
      DISPLAY ""
      DISPLAY "*******************************************************************"
      CALL fn_genera_archivo()
   ELSE
      DISPLAY "Ocurrio un ERROR al calcular los saldos especiales"
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
      RETURNING r_resultado_opera
      
   END IF
END MAIN

PRIVATE FUNCTION fn_genera_archivo()
   DEFINE v_opera_cod         LIKE cat_operacion.opera_cod -- codigo de operacion
   DEFINE v_ruta_ejecutable   LIKE seg_modulo.ruta_bin -- Ruta del ejecutable
   DEFINE r_resultado_opera   INTEGER
   DEFINE v_comando           STRING
   
   --Obtiene las rutas ejecutable
   SELECT ruta_bin
     INTO v_ruta_ejecutable
   FROM seg_modulo 
   WHERE modulo_cod = 'cbd'

   LET v_opera_cod = 2

   # se valida si se puede generar el proceso
   CALL fn_valida_operacion(p_pid,p_proceso_cod,v_opera_cod) RETURNING r_resultado_opera
   IF ( r_resultado_opera <> 0 ) THEN
      DISPLAY "No paso la operacion"
      CALL fn_muestra_inc_operacion(r_resultado_opera)
   ELSE
      # Inicia operación
      CALL fn_actualiza_opera_ini(p_pid,p_proceso_cod,v_opera_cod,v_folio,"CBDS06",
                            p_nombre_archivo,p_usuario_cod) RETURNING r_resultado_opera
      # En el caso de que exista una inconsistencia al iniciar el proceso, se
      # Muestra un mensaje con la descripcion
      IF(r_resultado_opera)THEN
         CALL fn_muestra_inc_operacion(r_resultado_opera)
      ELSE
         LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/CBDS06.42r ",
                                          p_usuario_cod," ",
                                          p_pid," ",
                                          p_proceso_cod," ",
                                          v_opera_cod," ",
                                          v_folio," '",
                                          p_nombre_archivo CLIPPED, "' ",
                                          p_fcorte, " ",
                                          "1>", v_ruta_listados CLIPPED ,
                                          "/nohup:",p_pid USING "&&&&&",":",
                                                   p_proceso_cod USING "&&&&&",":",
                                                   v_opera_cod USING "&&&&&" ," 2>&1 &"

         RUN v_comando
         IF(STATUS)THEN
            DISPLAY "Saldo Especial", 
                            "Ocurrió un error al iniciar el proceso batch"
         ELSE
            # Se indica que se realizo el proceso de carga
            DISPLAY " "
            DISPLAY "Se ha iniciado la siguiente etapa del proceso..."
         END IF
      END IF
   END IF
END FUNCTION