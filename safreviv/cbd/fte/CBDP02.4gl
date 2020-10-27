#########################################################################################
#Modulo       => CBD                                                                    #
#Programa     => CBDP02                                                                 #
#Objetivo     => Programa que genera la informacion previa                              #
#                para el proceso de conciliacion                                        #
#Fecha inicio => 27/06/2012                                                             #
#########################################################################################

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

MAIN
   DEFINE r_resultado_opera            INTEGER

   DEFINE v_fn_prepara_conciliacion    STRING
   DEFINE v_resultado                  SMALLINT
   DEFINE v_mensaje_respuesta          VARCHAR(100)

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

   CALL fn_inicializa_proceso(p_pid,p_proceso_cod,p_opera_cod,0,
                                          "CBDP02",p_nombre_archivo,p_usuario_cod)
                                 RETURNING r_resultado_opera
   IF ( r_resultado_opera <> 0 ) THEN
      CALL fn_muestra_inc_operacion(r_resultado_opera)
   ELSE
       -- se solicita el numero de folio asociado a la operacion
      -- parametros: proceso, operacion, usuario
      CALL fn_genera_folio(p_proceso_cod,p_opera_cod,p_usuario_cod)
      RETURNING v_folio

      # Inicia operación
      CALL fn_actualiza_opera_ini(p_pid,p_proceso_cod,p_opera_cod,v_folio,"CBDP02",
                            p_nombre_archivo,p_usuario_cod) RETURNING r_resultado_opera
      # En el caso de que exista una inconsistencia al iniciar el proceso, se
      # Muestra un mensaje con la descripcion
      IF(r_resultado_opera)THEN
         CALL fn_muestra_inc_operacion(r_resultado_opera)
      ELSE
         #Se manda a ejecutar la funcion que genera la informacion previa de conciliacion
         LET v_fn_prepara_conciliacion = "EXECUTE FUNCTION safre_viv:fn_cbd_prepara_conciliacion()"
         PREPARE exe_fn_prepara_conciliacion FROM v_fn_prepara_conciliacion
         EXECUTE exe_fn_prepara_conciliacion INTO v_resultado, v_mensaje_respuesta
         IF v_resultado = 0 THEN
            # Finaliza la operacion de generacion del archivo
            CALL  fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
            RETURNING r_resultado_opera
            IF(r_resultado_opera <> 0)THEN         
               # Actualiza a estado erróneo
               CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
               RETURNING r_resultado_opera
            END IF
            DISPLAY "*******************************************************************"
            DISPLAY ""
            DISPLAY "Termino la generacion del saldos para conciliacion: "
            DISPLAY ""
            DISPLAY " PROCESO            : ",v_proceso_desc
            DISPLAY " OPERACIÓN          : ",v_opera_desc
            DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
            DISPLAY " HORA               : ",TIME(CURRENT)
            DISPLAY ""
         ELSE
            CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
            RETURNING r_resultado_opera
         END IF
      END IF
   END IF
END MAIN