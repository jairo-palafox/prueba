#########################################################################################
#Modulo       => CBD                                                                    #
#Programa     => CBDP12                                                                 #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la integracion    #
#                del archivo de la BDNSVIV                                              #
#Fecha inicio => 10/06/2014                                                             #
#########################################################################################
DATABASE safre_viv

GLOBALS "CBDP12.inc"

PRIVATE DEFINE v_folio           DECIMAL(9,0)
PRIVATE DEFINE p_pid             DECIMAL(9,0)
PRIVATE DEFINE p_proceso_cod     SMALLINT
PRIVATE DEFINE p_opera_cod       SMALLINT
PRIVATE DEFINE p_usuario_cod     VARCHAR(20)
PRIVATE DEFINE p_nombre_archivo  VARCHAR(40)

PRIVATE DEFINE v_detalle_monitoreo        STRING


#Parametros de detalle del proceso
PRIVATE DEFINE v_layout            SMALLINT
PRIVATE DEFINE v_ruta_rescate      VARCHAR(40)
PRIVATE DEFINE v_usuario           VARCHAR(20)
PRIVATE DEFINE v_proceso_desc      VARCHAR(40)
PRIVATE DEFINE v_extension         VARCHAR(10)
PRIVATE DEFINE v_opera_desc        VARCHAR(40)
PRIVATE DEFINE v_ruta_listados     VARCHAR(40)

MAIN

   #Variables para el manejo de la ejecucion 
   DEFINE v_fn_cbd_integra_bdnsviv  STRING
   DEFINE v_resultado_oper          SMALLINT
   DEFINE v_resultado_fn            INTEGER
   DEFINE v_mensaje_fn              VARCHAR(200)
   
   -- se recuperan los parametros la clave de usuario desde parametro 
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   --LET v_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)

   WHENEVER ERROR CONTINUE
   
   CALL fn_recupera_inf_proceso(p_proceso_cod, p_opera_cod) 
                               RETURNING  v_proceso_desc,
                                          v_extension, 
                                          v_opera_desc,
                                          v_layout, 
                                          v_ruta_rescate,
                                          v_ruta_listados,
                                          v_usuario

   DISPLAY "*******************************************************************"
   LET v_detalle_monitoreo = " PROCESO            : ",v_proceso_desc,"\n",
                             " OPERACIÓN          : ",v_opera_desc,"\n",
                             " FECHA              : ",TODAY USING 'dd-mm-yyyy',"\n",
                             " HORA               : ",TIME(CURRENT)," "
   DISPLAY v_detalle_monitoreo;
   DISPLAY "*******************************************************************"

   LET p_nombre_archivo = p_nombre_archivo CLIPPED

   -- se solicita el numero de folio asociado a la operacion
   CALL fn_genera_folio(p_proceso_cod,p_opera_cod,p_usuario_cod)
   RETURNING v_folio

   #Se actualiza el folio del proceso               
   UPDATE bat_ctr_proceso SET folio = v_folio WHERE pid = p_pid
   UPDATE bat_ctr_operacion SET folio = v_folio WHERE pid = p_pid

   UPDATE glo_ctr_archivo SET folio = v_folio 
   WHERE proceso_cod = p_proceso_cod AND estado = 1 AND nombre_archivo = p_nombre_archivo

   LET v_fn_cbd_integra_bdnsviv = "EXECUTE FUNCTION fn_cbd_integra_bdnsviv(?)"
   PREPARE exe_fn_cbd_integra_bdnsviv FROM v_fn_cbd_integra_bdnsviv

   DISPLAY "Inicia la integracion del Archivo..."

   EXECUTE exe_fn_cbd_integra_bdnsviv   USING v_folio
                                    INTO  v_resultado_fn, 
                                          v_mensaje_fn
   CASE
      WHEN (SQLCA.SQLCODE = 0)
         IF v_resultado_fn = 0 THEN
            DISPLAY "##########################################################"
            DISPLAY "          TERMINO EL PROCESO DE INTEGRACION               "
            DISPLAY "                                                          "
            DISPLAY "Folio del lote: ", v_folio USING '#######&'
            DISPLAY "Mensaje del proceso: ", v_mensaje_fn
            DISPLAY "                                                          "
            DISPLAY "##########################################################"
            
            CALL fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
                     RETURNING v_resultado_oper

            UPDATE glo_ctr_archivo SET estado = ESTADO_ARCHIVO_INTEGRADO
            WHERE proceso_cod = p_proceso_cod AND estado = 1 AND nombre_archivo = p_nombre_archivo

            #Actualizamos la ejecucion de la siguiente operacion para que se ejecute de forma automatica
            UPDATE bat_ctr_operacion SET ind_tipo_ejecucion = 1 WHERE pid = p_pid AND opera_cod > 2
            
         ELSE
            DISPLAY "##########################################################"
            DISPLAY "          ERROR EN EL PROCESO DE INTEGRACION              "
            DISPLAY "                                                          "
            DISPLAY "Mensaje del proceso: ", v_mensaje_fn
            DISPLAY "                                                          "
            DISPLAY "##########################################################"
         
            CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod)
            RETURNING v_resultado_oper

            CALL fn_desplega_inc_operacion(v_resultado_oper)

            UPDATE glo_ctr_archivo SET estado = ESTADO_ARCHIVO_ERROR 
            WHERE proceso_cod = p_proceso_cod AND estado = 1 AND nombre_archivo = p_nombre_archivo

         END IF
         
                  
      WHEN (SQLCA.SQLCODE < 0)
         DISPLAY "##########################################################"
         DISPLAY "          ERROR EN EL PROCESO DE INTEGRACION              "
         DISPLAY "                                                          "
         DISPLAY SQLERRMESSAGE
         DISPLAY SQLCA.SQLCODE
         DISPLAY "                                                          "
         DISPLAY "##########################################################"
         
         CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod)
         RETURNING v_resultado_oper

         CALL fn_desplega_inc_operacion(v_resultado_oper)

         CALL fn_error_integracion()
         
         UPDATE glo_ctr_archivo SET estado = ESTADO_ARCHIVO_ERROR 
         WHERE proceso_cod = p_proceso_cod AND estado = 1 AND nombre_archivo = p_nombre_archivo
         
      WHEN (SQLCA.SQLCODE = NOTFOUND)
         DISPLAY "NOT FOUND"
         DISPLAY SQLERRMESSAGE
         CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) RETURNING v_resultado_oper

         CALL fn_desplega_inc_operacion(v_resultado_oper)

         UPDATE glo_ctr_archivo SET estado = ESTADO_ARCHIVO_ERROR 
         WHERE proceso_cod = p_proceso_cod AND estado = 1 AND nombre_archivo = p_nombre_archivo

         DISPLAY "Error al procesar la integración"
         DISPLAY "No se puede continuar..."
   END CASE
   
   WHENEVER ERROR STOP
END MAIN

PRIVATE FUNCTION fn_error_integracion()

   DELETE FROM cbd_cza_bdnsviv WHERE folio = v_folio

   DELETE FROM cbd_cifras_concilia_global WHERE folio = v_folio

   DELETE FROM cbd_sum_bdnsviv WHERE folio = v_folio

END FUNCTION