#########################################################################################
#Modulo       => CBD                                                                    #
#Programa     => CBDP31                                                                 #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la integracion    #
#                del archivo de ajuste operativo                                        #
#Fecha inicio => 17/10/2014                                                             #
#########################################################################################
DATABASE safre_viv

GLOBALS "CBDP31.inc"

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
   DEFINE v_fn_cbd_integra_ajuste_operativo  STRING
   DEFINE v_resultado_oper                   SMALLINT
   DEFINE v_resultado_fn                     INTEGER
   DEFINE v_mensaje_fn                       VARCHAR(200)
   
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

   LET v_fn_cbd_integra_ajuste_operativo= "EXECUTE FUNCTION fn_cbd_integra_ajuste_operativo(?,?)"
   PREPARE exe_fn_cbd_integra_ajuste_operativo FROM v_fn_cbd_integra_ajuste_operativo

   DISPLAY "Inicia la integracion del Archivo..."

   EXECUTE exe_fn_cbd_integra_ajuste_operativo USING v_folio,
                                                     p_usuario_cod
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
            
         ELSE
            DISPLAY "##########################################################"
            DISPLAY "          ERROR EN EL PROCESO DE INTEGRACION              "
            DISPLAY "                                                          "
            DISPLAY "Mensaje del proceso: ", v_mensaje_fn
            DISPLAY "                                                          "
            DISPLAY "##########################################################"

            IF v_resultado_fn = 9 THEN
               CALL fn_genera_archivo_rechazos()
            END IF
         
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

         UPDATE glo_ctr_archivo SET estado = ESTADO_ARCHIVO_ERROR 
         WHERE proceso_cod = p_proceso_cod AND estado = 1 AND nombre_archivo = p_nombre_archivo

         CALL fn_desplega_inc_operacion(v_resultado_oper)

         CALL fn_error_integracion()
         
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

   DELETE FROM cbd_detalle_ajuste_operativo WHERE folio = v_folio
   
   DELETE FROM cbd_cifras_ajuste_operativo WHERE folio = v_folio

   DELETE FROM cbd_ctr_ajuste_operativo WHERE folio = v_folio

END FUNCTION

PRIVATE FUNCTION fn_genera_archivo_rechazos()
   DEFINE v_ruta_envio        LIKE seg_modulo.ruta_envio
   DEFINE v_nombre_archivo    STRING
   DEFINE v_archivo           base.Channel
   DEFINE v_formato_fecha            STRING
   DEFINE v_folio_archivo     STRING
   DEFINE v_comando                  STRING

   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio
   INTO v_ruta_envio
   FROM seg_modulo
   WHERE modulo_cod = 'cbd'

   LET v_nombre_archivo = v_ruta_envio CLIPPED,"/EXPORTA_NSS_SIN_CUENTA.sql"
   LET v_formato_fecha = TODAY USING 'ddmmyyyy'
   LET v_folio_archivo = v_folio

    LET v_archivo = base.Channel.create()
   CALL v_archivo.openFile(v_nombre_archivo,"w")
   CALL v_archivo.writeLine('SET PDQPRIORITY HIGH;')
   CALL v_archivo.writeLine('unload TO ' || v_ruta_envio CLIPPED || '/NSS_SIN_CUENTA_SAFRE_' || v_folio_archivo CLIPPED || '_' || v_formato_fecha || '.rajuste')
   CALL v_archivo.writeLine('SELECT')
   CALL v_archivo.writeLine('det.nss,')
   CALL v_archivo.writeLine('det.subcuenta,')
   CALL v_archivo.writeLine('det.fondo_inversion,')
   CALL v_archivo.writeLine('det.monto_acciones')
   CALL v_archivo.writeLine('FROM cbd_detalle_ajuste_operativo det')
   CALL v_archivo.writeLine('WHERE det.id_derechohabiente IS NULL')

   CALL v_archivo.close()

   LET v_comando = "dbaccess safre_viv ", v_nombre_archivo
   RUN v_comando

   LET v_comando = "rm ", v_nombre_archivo
   RUN v_comando
END FUNCTION
