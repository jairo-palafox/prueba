#########################################################################################
#Modulo       => CBD                                                                    #
#Programa     => CBDP01                                                                 #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la integracion    #
#                del archivo de la BDNSVIV                                              #
#Fecha inicio => 29/05/2012                                                             #
#########################################################################################
DATABASE safre_viv

DEFINE v_folio                    LIKE glo_ctr_archivo.folio

MAIN
   ##Parametros generales del proceso
   DEFINE p_pid                      LIKE bat_ctr_operacion.pid                  -- PID del proceso
   DEFINE p_proceso_cod              LIKE bat_ctr_operacion.proceso_cod          -- codigo del proceso
   DEFINE p_opera_cod                LIKE bat_ctr_operacion.opera_cod            -- codigo de la operacion
   DEFINE p_usuario_cod              LIKE seg_usuario.usuario_cod                -- clave del usuario firmado
   DEFINE p_nombre_archivo           LIKE glo_ctr_archivo.nombre_archivo         -- nombre dle archivo
   
   DEFINE v_estado                   SMALLINT

   #Parametros de detalle del proceso
   DEFINE v_layout            LIKE cat_operacion.layout_cod
   DEFINE v_ruta_rescate      STRING
   DEFINE v_usuario           LIKE seg_modulo.usuario
   DEFINE v_proceso_desc      LIKE cat_proceso.proceso_desc
   DEFINE v_extension         LIKE cat_operacion.extension
   DEFINE v_opera_desc        LIKE cat_operacion.opera_desc
   DEFINE v_ruta_listados     LIKE seg_modulo.ruta_listados

   #Variables para el manejo de la ejecucion 
   DEFINE v_fn_integra_bdnsviv      STRING
   DEFINE v_fn_fin_integra_bdnsviv STRING
   DEFINE v_fn_actualiza_archivo    STRING
   DEFINE v_resultado_oper          SMALLINT
   DEFINE v_resultado_fn            INTEGER
   DEFINE v_mensaje_fn              VARCHAR(200)
   DEFINE v_total_rechazos_fn       INTEGER
   DEFINE v_total_registros_fn      INTEGER

   ##Ejecuta prevalidación de saldos
   -- se recuperan los parametros la clave de usuario desde parametro 
   -- argumento con indice 1
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

   LET p_nombre_archivo = p_nombre_archivo CLIPPED

   -- se solicita el numero de folio asociado a la operacion
    -- parametros: proceso, operacion, usuario
    CALL fn_genera_folio(p_proceso_cod,p_opera_cod,p_usuario_cod)
      RETURNING v_folio

      #Se actualiza el folio del proceso               
   UPDATE bat_ctr_proceso SET folio = v_folio WHERE pid = p_pid
   UPDATE bat_ctr_operacion SET folio = v_folio WHERE pid = p_pid

   UPDATE glo_ctr_archivo SET folio = v_folio 
   WHERE proceso_cod = p_proceso_cod AND estado = 1 AND nombre_archivo = p_nombre_archivo

   -- Inicio operacion.
   CALL fn_actualiza_opera_ini(p_pid,p_proceso_cod,p_opera_cod,v_folio,"CBDP01",
                        "",p_usuario_cod)
   RETURNING v_resultado_oper

   IF v_resultado_oper = 0 THEN        --Actualizacion exitosa
      LET v_fn_integra_bdnsviv = "EXECUTE FUNCTION fn_integra_bdnsviv(?,?,?,?)"
      PREPARE exe_fn_integra_bdnsviv FROM v_fn_integra_bdnsviv

      LET v_fn_actualiza_archivo = "EXECUTE FUNCTION fn_act_edo_archivo(?,?,?,?)"
      PREPARE exe_fn_actualiza_archivo FROM v_fn_actualiza_archivo

      EXECUTE exe_fn_integra_bdnsviv 
      USING p_usuario_cod, v_folio, p_nombre_archivo, p_pid
      INTO v_resultado_fn, v_mensaje_fn, v_total_rechazos_fn, v_total_registros_fn
      CASE
         WHEN (SQLCA.SQLCODE = 0)
            DATABASE safre_sdo@vivws_tcp
            LET v_fn_fin_integra_bdnsviv = "EXECUTE FUNCTION fn_fin_integra_bdnsviv(?)"
            PREPARE exe_fn_fin_integra_bdnsviv FROM v_fn_fin_integra_bdnsviv

            EXECUTE exe_fn_fin_integra_bdnsviv USING v_folio
            INTO v_resultado_fn, v_mensaje_fn

            IF SQLCA.SQLCODE < 0 THEN
               DISPLAY SQLERRMESSAGE
               DISPLAY SQLCA.SQLCODE
               DATABASE safre_viv
               CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod)
               RETURNING v_resultado_oper
               DISPLAY fn_mues_desc_valida(v_resultado_oper)

               CALL fn_error_integracion()
               
               LET v_estado = 3     --Estado reversado (Esto para que permita cargar un archivo con el mismo nombre)
               
               UPDATE glo_ctr_archivo SET estado = v_estado 
               WHERE proceso_cod = p_proceso_cod AND estado = 1 AND nombre_archivo = p_nombre_archivo

               #EXECUTE exe_fn_actualiza_archivo USING p_nombre_archivo,
               #                                       v_folio,
               #                                       v_estado,
               #                                       p_usuario_cod
               #                                 INTO  v_resultado_fn
                                                
               
               DISPLAY "Error al procesar la integración"
               DISPLAY "No se puede continuar..."
            END IF

            DATABASE safre_viv
         
            DISPLAY "##########################################################"
            DISPLAY "          TERMINO EL PROCESO DE INTEGRACION               "
            DISPLAY "                                                          "
            DISPLAY "Folio del lote: ", v_folio
            DISPLAY "Mensaje del proceso: ", v_mensaje_fn
            DISPLAY "Total de registros rechazados: ", v_total_rechazos_fn
            DISPLAY "Total de registros integrados: ", v_total_registros_fn
            DISPLAY "                                                          "
            DISPLAY "##########################################################"

            IF v_resultado_fn = 0 THEN
               LET v_estado = 2     --Estado de integrado
               
               CALL fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
                        RETURNING v_resultado_oper

               UPDATE glo_ctr_archivo SET estado = v_estado 
               WHERE proceso_cod = p_proceso_cod AND estado = 1 AND nombre_archivo = p_nombre_archivo
               #EXECUTE exe_fn_actualiza_archivo USING p_nombre_archivo,
               #                                       v_folio,
               #                                       v_estado,
               #                                       p_usuario_cod
               #                                 INTO  v_resultado_fn

               DISPLAY "##########################################################"
               DISPLAY "Actualizando el estado del archivo:"
               DISPLAY "archivo: ", p_nombre_archivo
               DISPLAY "folio: ", v_folio
               #DISPLAY "estado: ", v_estado
               #DISPLAY "usuario: ", p_usuario_cod
               #DISPLAY "respuesta: ", v_resultado_fn
               DISPLAY "##########################################################"
            ELSE
               CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod)
               RETURNING v_resultado_oper
               DISPLAY fn_mues_desc_valida(v_resultado_oper)

               LET v_estado = 3     --Estado reversado (Esto para que permita cargar un archivo con el mismo nombre)

               UPDATE glo_ctr_archivo SET estado = v_estado 
               WHERE proceso_cod = p_proceso_cod AND estado = 1 AND nombre_archivo = p_nombre_archivo

               #EXECUTE exe_fn_actualiza_archivo USING p_nombre_archivo,
               #                                       v_folio,
               #                                       v_estado,
               #                                       p_usuario_cod
               #                                 INTO  v_resultado_fn

               DISPLAY "Error al procesar la integración"
               DISPLAY "No se puede continuar..."
            END IF
            
                     
         WHEN (SQLCA.SQLCODE < 0)
            DISPLAY "Error No. ", SQLCA.SQLCODE
            DISPLAY SQLERRMESSAGE
            CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod)
            RETURNING v_resultado_oper
            DISPLAY fn_mues_desc_valida(v_resultado_oper)

            CALL fn_error_integracion()
            
            LET v_estado = 3     --Estado reversado (Esto para que permita cargar un archivo con el mismo nombre)
            EXECUTE exe_fn_actualiza_archivo USING p_nombre_archivo,
                                                   v_folio,
                                                   v_estado,
                                                   p_usuario_cod
                                             INTO  v_resultado_fn
                                             
            
            DISPLAY "Error al procesar la integración"
            DISPLAY "No se puede continuar..."
         WHEN (SQLCA.SQLCODE = NOTFOUND)
            DISPLAY "NOT FOUND"
            CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) RETURNING v_resultado_oper
            DISPLAY fn_mues_desc_valida(v_resultado_oper)

            LET v_estado = 3     --Estado reversado (Esto para que permita cargar un archivo con el mismo nombre)
            EXECUTE exe_fn_actualiza_archivo USING p_nombre_archivo,
                                                   v_folio,
                                                   v_estado,
                                                   p_usuario_cod
                                             INTO  v_resultado_fn

            DISPLAY "Error al procesar la integración"
            DISPLAY "No se puede continuar..."
      END CASE
   ELSE
      DISPLAY "no actualizo la operacion"
      DISPLAY "no inicia operacion:",v_resultado_oper
      DISPLAY fn_mues_desc_valida(v_resultado_oper)
      DISPLAY "PID    :",p_pid
      DISPLAY "Proceso:",p_proceso_cod
      DISPLAY "Operac :",p_opera_cod
   END IF
   
   WHENEVER ERROR STOP
END MAIN

FUNCTION fn_mues_desc_valida(p_resultado_opera)
  DEFINE p_resultado_opera SMALLINT
        ,v_descripcion LIKE cat_bat_parametro_salida.descripcion
  

   -- Obtiene la descripción resultado de la validacion
   SELECT descripcion
     INTO v_descripcion
     FROM cat_bat_parametro_salida
    WHERE cod_salida = p_resultado_opera

   RETURN v_descripcion CLIPPED
END FUNCTION -- fn_mues_desc_valida

PRIVATE FUNCTION fn_error_integracion()

   DELETE FROM cbd_cza_bdnsviv WHERE folio = v_folio

   DELETE FROM cbd_cifras_concilia_global WHERE folio = v_folio

   DELETE FROM cbd_sum_bdnsviv WHERE folio = v_folio

END FUNCTION